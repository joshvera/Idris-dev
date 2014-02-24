{-# LANGUAGE PatternGuards #-}

module IRTS.CodegenObjC (codegenObjC, printError) where

import Idris.Core.TT as TT hiding (mkApp)
import IRTS.CodegenCommon
import IRTS.Lang as TT
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
         blockItems = [BlockStm (Exp (Just $ translateExpression body) noLoc)]

printError :: String -> Exp
printError msg =
   FnCall (QC.Var (mkId "NSLog") noLoc) [litString] noLoc 
      where
         litString = 
            ObjCLitString [StringConst [string] "" noLoc] noLoc
         string = ((pretty 80) . dquotes . text) msg


translateExpression :: SExp -> QC.Exp

translateExpression (SConst constant) =
  Const (translateConstant constant) noLoc

translateExpression (SApp tc name vars) =
   objcCall name vars

translateExpression (SLet name value body) =
   objcLet name value body

translateExpression (SError error) =
   printError error

translateExpression _ = translateVariable (TT.Loc 10)

mkId :: String -> Id
mkId ident = Id ident noLoc

translateVariable :: LVar -> QC.Exp
translateVariable (TT.Loc i) = QC.Var (Id identifier noLoc) noLoc
   where
      identifier = ("__var_" ++) $ show i

objcCall :: Name -> [LVar] -> QC.Exp
objcCall name (TT.Loc i : xs) =
   FnCall (QC.Var (Id (show name) noLoc) noLoc) [] noLoc
      where
         location = (srclocOf $ linePos "" i)
objcCall name _ = QC.Var (Id "undefined" noLoc) noLoc

objcLet :: LVar -> SExp -> SExp -> QC.Exp
objcLet name sValue body =
   Seq assignment exprBody noLoc
   where
      exprBody = (translateExpression body)
      assignment = Assign identifier JustAssign value noLoc
      identifier = translateVariable name
      value = translateExpression sValue

translateConstant :: TT.Const -> QC.Const
translateConstant (Str s) = toConst s noLoc
translateConstant (I i) = toConst i noLoc
translateConstant (BI i) = toConst i noLoc
translateConstant (Fl i) = toConst i noLoc
translateConstant _ = toConst "undefined" noLoc

logDeclarations :: (Name, SDecl) -> [String]
logDeclarations (path, sdecl) =
   [show sdecl ++ "\n"]
