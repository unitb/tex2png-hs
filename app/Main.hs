{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad.State
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Tuple
import           Options.Applicative
import           Options.Applicative.Text
import           Paths_tex2png_hs (version)
import           System.Environment
import           Text.Printf

import           TH
import           TeX2PNG

contents :: Parser T.Text
contents = option text
          (short 'c'
           <> metavar "<string>"
           <> help "The (La)TeX string to be rendered.")

args :: Parser (Args, T.Text)
args = fmap swap . liftA2 (,) contents $
      Args
  <$> option text
  (short 'b'
   <> metavar "<color spec>"
   <> value "transparent"
   <> showDefault
   <> help (T.unpack . T.intercalate " " $
            [ "The background color to pass to dvipng's \"-bg\" option."
            , "It should be given in TeX color \\special syntax, e.g."
            , "\"rgb 0.2 0.2 0.2\". \"Transparent\" and \"transparent\" are"
            , "also accepted. See the dvipng help message for more details."
            ]))
  <*> optional
  (strOption
    (short 'd'
     <> metavar "<path>"
     <> help (T.unpack . T.intercalate " " $
              [ "The output directory. If set, then the image will be saved"
              , "there, otherwise it will be saved in the current directory."
              ])))
  <*> option auto
  (short 'D'
   <> metavar "<int>"
   <> value 100
   <> showDefault
   <> help (T.unpack . T.intercalate " " $
            [ "The dpi argument passed to \"dvipng\"."
            , "Increase this to increase font size."
            ]))
  <*> switch
  (short 'f'
   <> help (T.unpack . T.intercalate " " $
            [ "Specify the full input document. By default, a pre-defined"
            , "header is prepended to the input content, which is then wrapped"
            , "in document tags. This options enables the user to provide a full"
            , "(La)TeX document with custom headers/footers."
            ]))
  <*> switch
  (short 'm'
   <> help "Wrap the content in math mode (align* environment).")
  <*> optional
  (strOption
    (short 'o'
     <> metavar "<path>"
     <> help (T.unpack . T.intercalate " " $
              [ "The output image path. If it is set, then it is the full path"
              , "to the image. If it is not set then the image name will be the"
              , "sha256 digest of the (La)TeX input string with the \".png\""
              , "extension."
              ])))
  <*> option auto
  (short 'p'
   <> metavar "<int>"
   <> value 1
   <> showDefault
   <> help "Page number to render.")
  <*> optional
  (T.splitOn ","
    <$> option text
    (short 'P'
     <> metavar "<packages>"
     <> help (T.unpack . T.intercalate " " $
              [ "Comma-separated list of packages to include. Note that the"
              , "program doesn't check for existence of packages. Therefore,"
              , "make sure that you've correctly installed the packages when"
              , "using this option."
              ])))
  <*> optional
  (strOption
    (short 't'
     <> metavar "<path>"
     <> help (T.unpack . T.intercalate " " $
              [ "The temporary working directory. A random directory is created"
              , "in $TMPDIR by default."
              ])))
  <*> switch
  (short 'T'
   <> help "Crop whitespace around the content (dvipng -T tight).")

main :: IO ()
main = do
  progn <- getProgName
  let opts = info (helper <*> versionOption <*> args)
             (briefDesc
              <> header (printf "%s - convert (La)TeX to PNG images" progn))
      versionOption = infoOption
                      ver
                      (long "version"
                       <> help "Show version"
                       <> hidden)
      ver = $(simpleVersion version)
  res <- execParser opts >>= uncurry (\arg -> mkPNG $ put arg)
  either
    T.putStrLn                                   -- display the erro
    (\path -> putStrLn $ concat ["file=", path]) -- display the file path
    res
