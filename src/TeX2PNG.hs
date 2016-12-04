{-# LANGUAGE OverloadedStrings
      , TemplateHaskell
      , QuasiQuotes
      , DeriveGeneric #-}

module TeX2PNG
    ( Args(..)
    , Background (..)
    , OptArgs
    , (.=)
    , mkPDF
    , mkPNG 
    , bg, dir, dpi 
    , full, math, out
    , page, pkgs, temp
    , tightness )
where

import Control.Exception
import Control.Lens (makeLenses, (^.), (.=))
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Either

import qualified Crypto.Hash.SHA256 as SHA256

import qualified Data.ByteString.Base16 as BS16
import           Data.ByteString.Char8 as BS
import           Data.Maybe
import           Data.Monoid
import           Data.Serialize hiding (expect)
import           Data.Serialize.Text ()
import           Data.Text as T
import           Data.Text.IO as T

import GHC.Generics
import Prelude as P

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Error
import System.Process hiding (readCreateProcessWithExitCode)
import System.Process.Text

import Text.ParserCombinators.ReadP
import Text.Printf.TH
import Text.Read     hiding (lift,choice)
import Text.Read.Lex 

data Args = Args
  { _bg :: Background
  -- , _content :: Text
  , _dir :: Maybe FilePath
  , _dpi :: Int
  , _full :: Bool
  , _math :: Bool
  , _out :: Maybe FilePath
  , _page :: Int
  , _pkgs :: [Text]
  , _temp :: Maybe FilePath
  , _tightness :: Bool
  }
  deriving (Eq,Show,Generic)

instance Serialize Args
instance Serialize Background

data Background = 
      AlphaTransparent 
      | SimplyTransparent
      | RGB Float Float Float
  deriving (Eq,Show,Generic)

makeLenses ''Args

instance Read Background where
  readsPrec _ = readP_to_S $ choice 
          [ SimplyTransparent <$ expect (Ident "transparent")
          , AlphaTransparent <$ expect (Ident "Transparent")
          , expect (Ident "rgb") *> (RGB <$> readDecP <*> readDecP <*> readDecP) ]

backgroundArg :: Background -> String
backgroundArg SimplyTransparent = "transparent"
backgroundArg AlphaTransparent = "Transparent"
backgroundArg (RGB r g b) = [s|rgb %f %f %f|] r g b

pdflatex, latex, dvipng :: String
pdflatex = "pdflatex"
latex = "latex"
dvipng = "dvipng"

type OptArgs = State Args

mkPDF, mkPNG :: OptArgs k -> Text -> IO (Either Text FilePath)
mkPDF args = renderPDF args . generate args
mkPNG args = render args . generate args

runArgs :: OptArgs k -> Args
runArgs cmd = execState cmd $ Args
    { _bg = SimplyTransparent 
    , _dir  = Nothing 
    , _dpi  = 100
    , _full = False
    , _math = False
    , _out  = Nothing
    , _page = 1
    , _pkgs = []
    , _temp = Nothing
    , _tightness = False }

generate :: OptArgs k -> Text -> Text
generate opts content =
  if (args^.full)
  then content
  else T.intercalate "\n"
       [ "\\documentclass{article}"
       , if (args^.tightness)
         then
           "\\usepackage[paperwidth=\\maxdimen,paperheight=\\maxdimen]{geometry}"
         else ""
       , T.intercalate "\n" $ usepackage <$> args^.pkgs
       , header
       , if (args^.math)
         then T.intercalate "\n"
              ["\\begin{align*}"
              , content
              , "\\end{align*}"
              ]
         else content
       , footer
       ]
  where
    usepackage t = "\\usepackage{" <> t <> "}"
    args = runArgs opts

render :: OptArgs k -> Text -> IO (Either Text FilePath)
render opts c = runEitherT $ join $ do
  let args = runArgs opts
  (lExit, lOut, lErr) <- bimapEitherT errLatex id
                         (runLatex args c)
  case lExit of
    ExitSuccess -> do
      (dExit, dOut, dErr) <- bimapEitherT errDvipng id
                             (runDvipng args c)
      case dExit of
        ExitSuccess -> return $ lift $ outFile args c "png"
        _ -> return . left . T.intercalate "\n" $ [dOut, dErr]
    _ -> return . left . T.intercalate "\n" $ [lOut, lErr]

  where
    errLatex, errDvipng :: () -> Text
    errLatex _ = T.intercalate "\n" $
      [ "The `" <> (T.pack latex) <> "` command was not found."
      , "Make sure you have a working LaTeX installation."
      ]
    errDvipng _ = T.intercalate "\n" $
      [ "The `" <> (T.pack dvipng) <> "` command was not found."
      , "If you already have LaTeX installed, you may have to "
      <>"install dvipng manually from CTAN."
      ]

renderPDF :: OptArgs k -> Text -> IO (Either Text FilePath)
renderPDF opts c = runEitherT $ join $ do
  let args = runArgs opts
  (plExit, plOut, plErr) <- bimapEitherT errPdfLatex id
                         (runPdfLatex args c)
  case plExit of
    ExitSuccess -> do
      return $ lift $ outFile args c "pdf"
    _ -> return . left . T.intercalate "\n" $ [plOut, plErr]

  where
    errPdfLatex :: () -> Text
    errPdfLatex _ = T.intercalate "\n" $
      [ "The `" <> (T.pack pdflatex) <> "` command was not found."
      , "Make sure you have a working LaTeX installation."
      ]

runLatex :: Args -> Text -> EitherT () IO (ExitCode, Text, Text)
runLatex args content' = do
  f <- lift $ outFile args content' "tex"
  t <- lift $ tmpDir (args^.temp)
  e <- lift $ getEnvironment
  let contentFileName = t </> takeFileName f
  lift $ T.writeFile (contentFileName) content'
  EitherT $ tryJust (guard . isDoesNotExistError) $
    readCreateProcessWithExitCode
    ((proc latex
       [ "-halt-on-error"
       , "-output-directory=" ++ t
       , contentFileName
       ])
     {env = Just $ ("LC_ALL","C"):e}
    )
    mempty

runPdfLatex :: Args -> Text -> EitherT () IO (ExitCode, Text, Text)
runPdfLatex args content' = do
  o <- lift $ outFile args content' "tex"
  t <- lift $ tmpDir (args^.temp)
  e <- lift $ getEnvironment
  let contentFileName = t </> takeFileName o
  lift $ T.writeFile (contentFileName) content'
  EitherT $ tryJust (guard . isDoesNotExistError) $
    readCreateProcessWithExitCode
    ((proc pdflatex
       [ "-halt-on-error"
       , "-output-directory=" ++ takeDirectory o
       , contentFileName
       ])
     {env = Just $ ("LC_ALL","C"):e}
    )
    mempty

runDvipng :: Args -> Text -> EitherT () IO (ExitCode, Text, Text)
runDvipng args c = do
  f <- lift $ outFile args c "dvi"
  o <- lift $ outFile args c "png"
  tmp <- liftIO $ tmpDir (args^.temp)
  let t = args^.tightness
  EitherT $ tryJust (guard . isDoesNotExistError) $
    readCreateProcessWithExitCode
    (proc dvipng $ P.concat
      [
        [ "-q", "-D", show (args^.dpi)
        , "-p", show (args^.page)
        ]
      , if t then ["-T", "tight"] else []
      , [ "-bg", backgroundArg (args^.bg)
        , "--png", "-z 9"
        , "-o", o
        , tmp </> takeFileName f
        ]
      ])
    mempty

outDir :: Maybe FilePath -> IO FilePath
outDir d = case d of
  Just path -> return path
  Nothing -> getCurrentDirectory

outFile :: Args -> Text -> String -> IO FilePath
outFile args c ext = do
  case args^.out of
    Just path -> return $ path -<.> ext
    Nothing -> do
      dir' <- outDir (args^.dir)
      let name = BS.unpack . BS16.encode . SHA256.hash $ encode args <> encode c
          file = dir' </> name -<.> ext
      return file

tmpDir :: Maybe FilePath -> IO FilePath
tmpDir t = case t of
  Just tmp -> return tmp
  Nothing -> do
    tmp <- getTemporaryDirectory
    let t2pDir = tmp </> "tex2png-hs"
    createDirectoryIfMissing True t2pDir
    return t2pDir

header :: Text
header = T.intercalate "\n"
  [ "\\pagestyle{empty}"
  , "\\usepackage[utf8]{inputenc}"
  , "\\usepackage{lmodern}"
  , "\\usepackage{amsmath,amssymb}"
  , "\\begin{document}"
  , "\\begin{samepage}"
  ]

footer :: Text
footer = T.intercalate "\n"
  [ "\\end{samepage}"
  , "\\end{document}"
  , ""
  ]
