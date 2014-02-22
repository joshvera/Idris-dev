{-# LANGUAGE PatternGuards #-}

module IRTS.CodegenObjC (codegenObjC) where

import Idris.Core.TT hiding (mkApp)
import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified

import Language.C.Quote as QC

import System.FilePath

codegenObjC :: [(Name, SExp)] -> -- initialization of globals
               [(Name, SDecl)] -> -- decls
               FilePath -> -- output file name
               [String] -> -- headers
               [String] -> -- libs
               OutputType ->
               IO ()
codegenObjC globalInit definitions filename headers libs outputType = generateObjCFile definitions filename

generateObjCFile :: [(Name, SDecl)] -> FilePath ->  IO ()
generateObjCFile definitions filename = do
   writeFile filename $ concat functions
      where
         functions :: [String]
         functions = concatMap translateDeclaration definitions


translateDeclaration :: (Name, SDecl) -> [String]
translateDeclaration (path, SFun name params stackSize body)
   | (MN _ ap) <- name
   , (SChkCase var cases) <- body
   , ap == txt "APPLY" = ["Apply"]

   | (MN _ ev) <- name
   , (SChkCase var cases) <- body
   , ev == txt "EVAL" = ["Eval"]
   | otherwise = ["Expression"]


