{-# LANGUAGE OverloadedStrings
, TemplateHaskell #-}

module TeX2PNG
    ( Args(..)
    , mkPNG )
where

import           Control.Exception
import           Control.Lens (makeLenses, (^.))
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.ByteString.Base16 (encode)
import           Data.ByteString.Char8 as BS
import           Data.Monoid
import           Data.Text as T
import           Data.Text.Encoding as T
import           Data.Text.IO as T
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
  , _out :: Maybe FilePath
  , _page :: Int
  , _temp :: Maybe FilePath
  , _tightness :: Bool
  }

makeLenses ''Args

latex, dvipng :: String
latex = "latex"
dvipng = "dvipng"

mkPNG :: Args -> IO (Either Text FilePath)
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
      , header
      , args^.content
      , footer
      ]

render :: Text -> Args -> IO (Either Text FilePath)
render c args = runEitherT $ join $ do
  (lExit, lOut, lErr) <- bimapEitherT errLatex id
                         (runLatex args c)
  case lExit of
    ExitSuccess -> do
      (dExit, dOut, dErr) <- bimapEitherT errDvipng id
                             (runDvipng args)
      case dExit of
        ExitSuccess -> return $ lift $ outFile args
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

runLatex :: Args -> Text -> EitherT () IO (ExitCode, Text, Text)
runLatex args content' = do
  t <- lift $ tmpDir (args^.temp)
  e <- lift $ getEnvironment
  let contentFileName = t </> "content.tex"
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

runDvipng :: Args -> EitherT () IO (ExitCode, Text, Text)
runDvipng args = do
  o <- lift $ outFile args
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
        , tmp </> "content.dvi"
        ]
      ])
    mempty

outDir :: Maybe FilePath -> IO FilePath
outDir d = case d of
  Just path -> return path
  Nothing -> getCurrentDirectory

outFile :: Args -> IO FilePath
outFile args = do
  case args^.out of
    Just path -> return path
    Nothing -> do
      dir' <- outDir (args^.dir)
      return $
        dir' </> ( flip mappend ".png" . BS.unpack . encode
                  . SHA256.hash . T.encodeUtf8 $ args^.content)

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
  , "\\usepackage{amssymb}"
  , "\\begin{document}"
  , "\\begin{samepage}"
  ]

footer :: Text
footer = T.intercalate "\n"
  [ "\\end{samepage}"
  , "\\end{document}"
  , ""
  ]
