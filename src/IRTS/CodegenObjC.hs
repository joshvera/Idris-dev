module IRTS.CodegenObjC (codegenObjC) where

import Idris.Core.TT hiding (mkApp)
import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified

import Language.C.Parser.Tokens

import System.FilePath

codegenObjC :: [(Name, SExp)] -> -- initialization of globals
               [(Name, SDecl)] -> -- decls
               FilePath -> -- output file name
               [String] -> -- headers
               [String] -> -- libs
               OutputType ->
               IO ()
codegenObjC globalInit definitions filename headers libs outputType = generateObjCFile filename

generateObjCFile :: FilePath ->  IO ()
generateObjCFile filename = do
   writeFile filename "hello world"
