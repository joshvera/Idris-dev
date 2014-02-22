module IRTS.CodegenObjC (codegenObjC) where

import Idris.Core.TT hiding (mkApp)
import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified

codegenObjC :: [(Name, SExp)] -> -- initialization of globals
               [(Name, SDecl)] -> -- decls
               FilePath -> -- output file name
               [String] -> -- headers
               [String] -> -- libs
               OutputType ->
               IO ()
codegenObjC = undefined
