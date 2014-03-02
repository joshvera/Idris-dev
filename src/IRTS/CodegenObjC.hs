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
         functions = concatMap translateDeclaration definitions

translateDeclaration :: (Name, SDecl) -> [Definition]
translateDeclaration (path, fun@(SFun name params stackSize body))
  | (SCon i name vars) <- body
    = translateConstructor i name vars

  | otherwise =
    [FuncDef (objcFun fun) noLoc]

objcFun :: SDecl -> Func
objcFun (SFun name paramNames stackSize body) =
   Func declSpec identifier decl params blockItems noLoc
      where
         declSpec = cdeclSpec [Tstatic noLoc] [] (Tvoid noLoc)
         identifier = Id (show name) noLoc
         decl = DeclRoot noLoc
         -- Fix me: figure out where to find types of params
         params = Params (map nameToParam paramNames) True noLoc
         blockItems = [BlockStm (Exp (Just $ translateExpression body) noLoc)]

nameToParam :: Name -> Param
nameToParam name = Param (Just (mkId (show name))) (cdeclSpec [] [] (Tnamed (mkId "NSObject") [] noLoc)) (Ptr [] (DeclRoot noLoc) noLoc) noLoc

printError :: String -> Exp
printError msg =
   FnCall (QC.Var (mkId "NSLog") noLoc) [litString] noLoc
      where
         litString =
            ObjCLitString [StringConst [string] "" noLoc] noLoc
         string = ((pretty 80) . dquotes . text) msg

translateConstructor :: Int -> Name -> [LVar] -> [QC.Definition]
translateConstructor i name args =
   [interface, implementation]
      where
         interface = ObjCClassIface className Nothing [] [] properties [] noLoc
         implementation = ObjCClassImpl className Nothing [] [] noLoc
         className = mkId (show name)
         properties = map toObjCProperty args

toObjCProperty :: LVar -> ObjCIfaceDecl
toObjCProperty var = ObjCIfaceProp [ObjCNonatomic noLoc, ObjCStrong noLoc, ObjCReadonly noLoc] fieldGroup noLoc
   where
      fieldGroup = FieldGroup (cdeclSpec [] [] (Tnamed (mkVarId var) [] noLoc)) [] noLoc

mkVarId :: LVar -> Id
mkVarId (TT.Loc i) = mkId $ ("__var_" ++) $ show i

translateExpression :: SExp -> QC.Exp

translateExpression (SConst constant) =
  Const (translateConstant constant) noLoc

translateExpression (SApp tc name vars) =
   objcCall name vars

translateExpression (SLet name value body) =
   objcLet name value body

translateExpression (SError error) =
   printError error

translateExpression (SUpdate var e) =
  objcAssign var e

translateExpression e =
  printError $ "Not yet implemented: " ++ filter (/= '\'') (show e)

mkId :: String -> Id
mkId ident = Id ident noLoc

translateVariable :: LVar -> QC.Exp
translateVariable (TT.Loc i) = mkVar identifier
   where
      identifier = ("__var_" ++) $ show i

objcAssign :: LVar -> SExp -> QC.Exp
objcAssign name e = Assign identifier JustAssign value noLoc
  where
    identifier = translateVariable name
    value = translateExpression e

objcCall :: Name -> [LVar] -> QC.Exp
objcCall name xs =
   FnCall ((mkVar . show) name) (map translateVariable xs) noLoc

mkVar :: String -> QC.Exp
mkVar s = QC.Var (mkId s) noLoc

objcLet :: LVar -> SExp -> SExp -> QC.Exp
objcLet name sValue body =
   Seq (objcAssign name sValue) exprBody noLoc
   where
     exprBody = (translateExpression body)

translateConstant :: TT.Const -> QC.Const
translateConstant (Str s) = toConst s noLoc
translateConstant (I i) = toConst i noLoc
translateConstant (BI i) = toConst i noLoc
translateConstant (Fl i) = toConst i noLoc
translateConstant _ = toConst "undefined" noLoc

logDeclarations :: (Name, SDecl) -> [String]
logDeclarations (path, sdecl) =
   [show sdecl ++ "\n"]