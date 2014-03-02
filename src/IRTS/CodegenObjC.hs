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
import System.IO.Unsafe (unsafePerformIO)

debugLog :: Show a => String -> a -> a
debugLog msg x = x `seq` unsafePerformIO (putStr msg >> print x >> return x)

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
  putStrLn (show definitions)
  writeFile filename $ pretty 80 (ppr $ classes ++ functions)
    where
      classes = idrisObjectClassDefs
      functions :: [Definition]
      functions = concatMap translateDeclaration definitions

translateDeclaration :: (Name, SDecl) -> [Definition]
translateDeclaration (path, fun) = [FuncDef (objcFun fun) noLoc]

objcFun :: SDecl -> Func
objcFun (SFun name paramNames stackSize body) =
   Func declSpec identifier decl params blockItems noLoc
      where
         declSpec = cdeclSpec [Tstatic noLoc] [] (Tvoid noLoc)
         identifier = nameToId (debugLog "Translating fun:" name)
         decl = DeclRoot noLoc
         -- Fix me: figure out where to find types of params
         params = Params (map nameToParam paramNames) False noLoc
         blockItems = [BlockStm (Exp (Just $ translateExpression paramNames body) noLoc)]

nameToParam :: Name -> Param
nameToParam name = Param (Just $ nameToId name) (cdeclSpec [] [] (Tnamed (mkId "NSObject") [] noLoc)) cPtrDecl noLoc

cPtrDecl :: Decl
cPtrDecl = Ptr [] (DeclRoot noLoc) noLoc

nameToId :: Name -> Id
nameToId name = mkId ("IDR" ++ translateName name)

translateName :: Name -> String
translateName (UN name) = "UN_" ++ (str name)
translateName (NS name _) = "NS_" ++ translateName name
translateName (MN i name) = "MN_" ++ (str name) ++ show i
translateName (SN name) = "SN_" ++ translateSpecialName name
translateName NErased = "NErased"

translateSpecialName :: SpecialName -> String
translateSpecialName name
  | WhereN i m n <- name =
    'W' : translateName m ++ translateName n ++ show i
  | InstanceN n s <- name =
    'I' : translateName n ++ concatMap str s
  | ParentN n s <- name =
    'P' : translateName n ++ str s
  | MethodN n <- name =
    'M' : translateName n
  | CaseN n <- name =
    'C' : translateName n

printError :: String -> Exp
printError msg = objcLog msg []

objcLog :: String -> [Name] -> QC.Exp
objcLog msg names = FnCall (QC.Var (mkId "NSLog") noLoc) (litString : args) noLoc
  where
    litString = ObjCLitString [StringConst [string] "" noLoc] noLoc
    string = ((pretty 80) . dquotes . text) msg
    args = map translateVariable names

idrisObjectClassDefs :: [QC.Definition]
idrisObjectClassDefs =
   [interface, implementation]
      where
         interface = ObjCClassIface className Nothing [] [] properties [] noLoc
         implementation = ObjCClassImpl className Nothing [] [] noLoc
         className = nameToId $ sUN "IdrisObject"

         methodType = Type (cdeclSpec [] [] (Tnamed (mkId "instancetype") [] noLoc)) (DeclRoot noLoc) noLoc
         method = ObjCIfaceMeth (ObjCMethodProto False (Just methodType)  [] [ObjCParam (Just (mkId "initWithIdentifier")) (Just $ mkObjectType "NSNumber") [] (Just $ mkId "identifier") noLoc, ObjCParam (Just $ mkId "array") (Just $ mkObjectType "NSArray") [] (Just $ mkId "array") noLoc] False [] noLoc) noLoc
         properties = [toObjCProperty "NSNumber" "identifier",toObjCProperty "NSArray" "arguments", method]

mkObjectType :: String -> QC.Type
mkObjectType s = QC.Type (cdeclSpec [] [] (Tnamed (mkId s) [] noLoc)) cPtrDecl noLoc

toObjCProperty :: String -> String -> ObjCIfaceDecl
toObjCProperty cls name = ObjCIfaceProp [ObjCNonatomic noLoc, ObjCStrong noLoc, ObjCReadonly noLoc] fieldGroup noLoc
   where
      fieldGroup = FieldGroup (cdeclSpec [] [] (Tnamed (mkId cls) [] noLoc)) [Field (Just $ mkId name) (Just cPtrDecl) Nothing noLoc] noLoc

mkVarId :: LVar -> Id
mkVarId (TT.Loc i) = mkId $ ("var_" ++) $ show i

translateExpression :: [Name] -> SExp -> QC.Exp

translateExpression _ (SConst constant) =
  Const (translateConstant constant) noLoc

translateExpression names (SApp tc name vars) =
   objcCall name (map (varToName names) vars)

translateExpression names (SLet var value body) =
   objcLet names var value body

translateExpression _ (SError error) =
   printError error

translateExpression names (SUpdate var e) =
  objcAssign (varToName names var) e

translateExpression _ SNothing = mkVar $ mkId "nil"

translateExpression names (SV var) = (translateVariable . (varToName names)) var

translateExpression names (SChkCase var cases) = translateCase (varToName names var) cases

translateExpression names (SForeign _ _ "putStr" [(_, var)]) =
  objcLog "%@" [(varToName names var)]

translateExpression names (SCon i name vars) =
  objcCon i (map (varToName names) vars)

translateExpression _ e =
  printError $ "Not yet implemented: " ++ filter (/= '\'') (show e)

objcCon :: Int -> [Name] -> QC.Exp
objcCon i names = ObjCMsg (ObjCRecvClassName (mkId "IdrisObject") noLoc) [ObjCArg (Just $ mkId "new") Nothing noLoc] [] noLoc

varToName :: [Name] -> LVar -> Name
varToName names (TT.Loc i) = (debugLog "varToName" names) !! i

translateCase :: Name -> [SAlt] -> QC.Exp
translateCase var [] = makeReturnExpr (translateVariable var)
translateCase var [SDefaultCase e] = makeReturnExpr (translateExpression [var] e)
translateCase var [SConstCase _ e] = makeReturnExpr (translateExpression [var] e)
translateCase var _ = makeReturnExpr (translateVariable var)

makeReturnExpr :: QC.Exp -> QC.Exp
makeReturnExpr = mkStmExpr. (:[]) . mkBlockStm . mkReturnStm

mkReturnStm :: QC.Exp -> QC.Stm
mkReturnStm exp = Return (Just exp) noLoc

mkBlockStm :: QC.Stm -> QC.BlockItem
mkBlockStm stm = BlockStm stm

mkStmExpr :: [QC.BlockItem] -> QC.Exp
mkStmExpr items = StmExpr items noLoc


mkId :: String -> QC.Id
mkId ident = Id ident noLoc

translateVariable :: Name -> QC.Exp
translateVariable name = mkVar $ nameToId name 

objcAssign :: Name -> SExp -> QC.Exp
objcAssign name e = Assign identifier JustAssign value noLoc
  where
    identifier = translateVariable name
    value = translateExpression [] e

objcCall :: Name -> [Name] -> QC.Exp
objcCall name xs =
   FnCall (translateVariable name) (map translateVariable xs) noLoc

mkVar :: Id -> QC.Exp
mkVar ident = QC.Var ident noLoc

mkVarName :: LVar -> Name
mkVarName (TT.Loc i) = sUN $ "var_" ++ (show i)

objcLet :: [Name] -> LVar -> SExp -> SExp -> QC.Exp
objcLet names var sValue body =
  FnCall blockLit [value] noLoc
   where
     blockLit = BlockLit (BlockParam [nameToParam name] noLoc) [] items noLoc
     value = translateExpression [] sValue
     name = mkVarName var
     exprBody = translateExpression (names ++ [name]) body
     items = [ BlockStm (Exp (Just exprBody) noLoc) ]

arithTyToObjCType :: ArithTy -> TypeSpec
arithTyToObjCType (ATInt iTy) = intTyToObjCType iTy
arityTyToObjCType (ATFloat) = doubleType

doubleType :: TypeSpec
doubleType = Tdouble noLoc

integerType :: TypeSpec
integerType = Tint Nothing noLoc

charType :: TypeSpec
charType = Tchar Nothing noLoc

shortType :: TypeSpec
shortType = Tshort Nothing noLoc

numberType :: TypeSpec
numberType = undefined

nativeTyToObjCType :: NativeTy -> TypeSpec
nativeTyToObjCType IT8 = charType
nativeTyToObjCType IT16 = shortType
nativeTyToObjCType IT32 = integerType
nativeTyToObjCType IT64 = doubleType

intTyToObjCType :: IntTy -> TypeSpec
intTyToObjCType (ITFixed nativeTy) = nativeTyToObjCType nativeTy
intTyToObjCType (ITNative) = integerType
intTyToObjCType (ITBig) = numberType
intTyToObjCType (ITChar) = charType
intTyToObjCType (ITVec nativeTy _) = charType

translateConstant :: TT.Const -> QC.Const
translateConstant (Str s) = toConst s noLoc
translateConstant (I i) = toConst i noLoc
translateConstant (BI i) = toConst i noLoc
translateConstant (Fl i) = toConst i noLoc
translateConstant _ = toConst "undefined" noLoc

logDeclarations :: (Name, SDecl) -> [String]
logDeclarations (path, sdecl) =
   [show sdecl ++ "\n"]
