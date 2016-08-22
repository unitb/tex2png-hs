{-# LANGUAGE OverloadedStrings
, TemplateHaskell
, DeriveGeneric #-}

module TeX2PNG
    ( Args(..)
    , mkPDF
    , mkPNG )
where

import           Control.Exception
import           Control.Lens (makeLenses, (^.))
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as BS16
import           Data.ByteString.Char8 as BS
import           Data.Monoid
import           Data.Serialize
import           Data.Serialize.Text ()
import           Data.Text as T
import           Data.Text.IO as T
import           GHC.Generics
import           Prelude as P
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO.Error
import           System.Process hiding (readCreateProcessWithExitCode)
import           System.Process.Text

data Args = Args
  { _bg :: Text
  , _content :: Text
  , _dir :: Maybe FilePath
  , _dpi :: Int
  , _full :: Bool
  , _math :: Bool
  , _out :: Maybe FilePath
  , _page :: Int
  , _pkgs :: Maybe [Text]
  , _temp :: Maybe FilePath
  , _tightness :: Bool
  }
  deriving (Generic)

instance Serialize Args

makeLenses ''Args

pdflatex, latex, dvipng :: String
pdflatex = "pdflatex"
latex = "latex"
dvipng = "dvipng"

mkPDF, mkPNG :: Args -> IO (Either Text FilePath)
mkPDF = renderPDF =<< generate
mkPNG = render =<< generate

generate :: Args -> Text
generate args =
  if (args^.full)
  then args^.content
  else T.intercalate "\n"
       [ "\\documentclass{article}"
       , if (args^.tightness)
         then
           "\\usepackage[paperwidth=\\maxdimen,paperheight=\\maxdimen]{geometry}"
         else ""
       , case args^.pkgs of
           Nothing -> ""
           Just ps -> T.intercalate "\n" $ usepackage <$> ps
       , header
       , if (args^.math)
         then T.intercalate "\n"
              ["\\begin{align*}"
              , args^.content
              , "\\end{align*}"
              ]
         else args^.content
       , footer
       ]
  where
    usepackage t = "\\usepackage{" <> t <> "}"

render :: Text -> Args -> IO (Either Text FilePath)
render c args = runEitherT $ join $ do
  (lExit, lOut, lErr) <- bimapEitherT errLatex id
                         (runLatex args c)
  case lExit of
    ExitSuccess -> do
      (dExit, dOut, dErr) <- bimapEitherT errDvipng id
                             (runDvipng args)
      case dExit of
        ExitSuccess -> return $ lift $ outFile args "png"
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

renderPDF :: Text -> Args -> IO (Either Text FilePath)
renderPDF c args = runEitherT $ join $ do
  (plExit, plOut, plErr) <- bimapEitherT errPdfLatex id
                         (runPdfLatex args c)
  case plExit of
    ExitSuccess -> do
      return $ lift $ outFile args "pdf"
    _ -> return . left . T.intercalate "\n" $ [plOut, plErr]

  where
    errPdfLatex :: () -> Text
    errPdfLatex _ = T.intercalate "\n" $
      [ "The `" <> (T.pack pdflatex) <> "` command was not found."
      , "Make sure you have a working LaTeX installation."
      ]

runLatex :: Args -> Text -> EitherT () IO (ExitCode, Text, Text)
runLatex args content' = do
  f <- lift $ outFile args "tex"
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
  o <- lift $ outFile args "tex"
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

runDvipng :: Args -> EitherT () IO (ExitCode, Text, Text)
runDvipng args = do
  f <- lift $ outFile args "dvi"
  o <- lift $ outFile args "png"
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
      , [ "-bg", T.unpack (args^.bg)
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

outFile :: Args -> String -> IO FilePath
outFile args ext = do
  case args^.out of
    Just path -> return $ path -<.> ext
    Nothing -> do
      dir' <- outDir (args^.dir)
      let name = BS.unpack . BS16.encode . SHA256.hash . encode $ args
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
