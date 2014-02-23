{-# LANGUAGE PatternGuards #-}

module IRTS.CodegenObjC (codegenObjC) where

import Idris.Core.TT as TT hiding (mkApp)
import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified

import Data.Loc
import Language.C.Quote as QC
import Text.PrettyPrint.Mainland

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
   writeFile filename $ pretty 80 (ppr functions)
      where
         functions :: [Definition]
         functions = map translateDeclaration definitions

translateDeclaration :: (Name, SDecl) -> Definition
translateDeclaration (name, fun) = FuncDef (objcFun fun) noLoc

objcFun :: SDecl -> Func
objcFun (SFun name paramNames stackSize body) =
   Func declSpec identifier decl params blockItems noLoc
      where
         declSpec = cdeclSpec [Tstatic noLoc] [] (Tvoid noLoc)
         identifier = Id (show name) noLoc
         decl = DeclRoot noLoc
         -- Fix me: figure out where to find types of params
         params = Params [] True noLoc
         blockItems = translateExpression body

printError :: String -> Exp
printError msg =
   FnCall (QC.Var (Id "NSLog" noLoc) noLoc) [ObjCLitString [QC.StringConst [msg] msg noLoc] noLoc] noLoc

translateExpression :: SExp -> [BlockItem]
translateExpression (SError msg) =
   [BlockStm (ObjCThrow (Just (printError message)) noLoc)]
   where
      message = "Error:" ++ msg

translateExpression (SConst constant) =
   [BlockStm (Exp (Just (Const (translateConstant constant) noLoc)) noLoc)]

translateExpression (SApp tc name vars) =
   [BlockStm (Exp (Just (objcCall name vars)) noLoc)]

translateExpression _ = []

objcCall :: Name -> [LVar] -> QC.Exp
objcCall name (IRTS.Lang.Loc i : xs) =
   FnCall (QC.Var (Id (show name) noLoc) noLoc) [] noLoc
      where
         location = (srclocOf $ linePos "" i)
objc name _ = QC.Var (Id "undefined" noLoc) noLoc

translateConstant :: TT.Const -> QC.Const
translateConstant (Str s) = toConst s noLoc
translateConstant (I i) = toConst i noLoc
translateConstant (BI i) = toConst i noLoc
translateConstant (Fl i) = toConst i noLoc
translateConstant _ = toConst "undefined" noLoc

logDeclarations :: (Name, SDecl) -> [String]
logDeclarations (path, sdecl) =
   [show sdecl ++ "\n"]
