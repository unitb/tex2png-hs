{-# LANGUAGE TemplateHaskell #-}

module TH where

import           Data.Version
import           Development.GitRev (gitDirty, gitHash)
import           Language.Haskell.TH (Q,Exp)
import qualified Language.Haskell.TH.Syntax as TH

-- | Generate a string like @Version 1.2, Git revision 1234@.
--
-- @$(simpleVersion …)@ @::@ 'String'
simpleVersion :: Version -> Q Exp
simpleVersion v =
  [|concat (["Version "
           ,$(TH.lift $ showVersion v)
           ] ++
           if $gitHash == ("UNKNOWN" :: String)
             then []
             else
               [", Git revision "
               ,$gitHash
               ,if $gitDirty
                   then " (dirty)"
                   else ""
               ])|]

-- simpleVersion taken from optparse-simple by FP Complete (under BSD3)
