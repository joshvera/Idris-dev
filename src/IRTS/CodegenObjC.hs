{-# LANGUAGE PatternGuards #-}

module IRTS.CodegenObjC (codegenObjC) where

import Idris.Core.TT hiding (mkApp)
import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified

import Data.Loc
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
   , ap == txt "APPLY" = 
      ["APPLY"]

   | (MN _ ev) <- name
   , (SChkCase var cases) <- body
   , ev == txt "EVAL" =
      ["EVAL"]

   | otherwise =
      [translateExpression body ++ "\n" ]

translateExpression :: SExp -> String
translateExpression (SError msg) =
   "Error:" ++ msg

translateExpression (SConst constant) =
   translateConstant constant

translateExpression (SApp tc name vars) =
   show $ objcCall name vars

translateExpression _ = "undefined"

objcCall :: Name -> [LVar] -> QC.Exp
objcCall name (IRTS.Lang.Loc i : xs) = 
   FnCall (QC.Var (Id (show name) noLoc) noLoc) [] noLoc
      where
         location = (srclocOf $ linePos "" i)
objc name _ = QC.Var (Id "undefined" noLoc) noLoc

translateConstant :: Idris.Core.TT.Const -> String
translateConstant (Str s) = s
translateConstant _ = "undefined"

logDeclarations :: (Name, SDecl) -> [String]
logDeclarations (path, sdecl) =
   [show sdecl ++ "\n"]
