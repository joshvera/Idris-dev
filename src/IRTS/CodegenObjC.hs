{-# LANGUAGE PatternGuards #-}

module IRTS.CodegenObjC (codegenObjC, printError) where

import Idris.Core.TT as TT hiding (mkApp)
import IRTS.CodegenCommon
import IRTS.Lang as TT
import IRTS.Simplified

import Data.List
import Data.Ord
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
  writeFile filename $ pretty 80 (ppr $ classes ++ functions ++ [mainDef])
    where
      classes = idrisObjectClassDefs
      functions :: [Definition]
      functions = sortBy (comparing cConstructors) $ concatMap translateDeclarations definitions

mainDef :: Definition
mainDef = FuncDef func noLoc
  where
    func = Func declSpec name noDecl params blockItems noLoc
    declSpec = cdeclSpec [] [] intTypeSpec 
    name = nameToId $ sUN "main"
    params = Params mainParams False noLoc
    blockItems = [ BlockStm $ mkExprStm $ objcCall (sMN 0 "runMain") []
                 , (BlockStm . mkReturnStm . translateVariable . sUN) "0"
                 ]

mainParams :: [Param]
mainParams = [Param (Just $ mkId "argc") intDeclSpec noDecl noLoc, Param (Just $ mkId "argv") charDeclSpec arrayPtrDecl noLoc]
  where
    charDeclSpec = cdeclSpec [] [] (Tchar Nothing noLoc)
    arrayPtrDecl = Array [] (NoArraySize noLoc) (Ptr [] noDecl noLoc) noLoc

intDeclSpec = cdeclSpec [] [] (Tint Nothing noLoc)

noDecl = DeclRoot noLoc

cConstructors :: Definition -> Int
cConstructors (DecDef _ _) = 0
cConstructors (FuncDef _ _) = 1
cConstructors _ = 2

translateDeclarations :: (Name, SDecl) -> [Definition]
translateDeclarations (path, f) = [DecDef initGroup noLoc, FuncDef func noLoc]
  where
    func = (objcFun f)
    initGroup = funcProto func

objcFun :: SDecl -> Func
objcFun (SFun name paramNames stackSize body) =
   Func declSpec identifier cPtrDecl params blockItems noLoc
      where
         declSpec = cdeclSpec [Tstatic noLoc] [] (mkObjectTypeSpec "NSObject")
         identifier = nameToId name
         -- Fix me: figure out where to find types of params
         params = Params (map nameToParam paramNames) False noLoc
         blockItems = translateDeclaration paramNames body

nameToParam :: Name -> Param
nameToParam name = Param (Just $ nameToId name) (cdeclSpec [] [] (Tnamed (mkId "id") [] noLoc)) (DeclRoot noLoc) noLoc

cPtrDecl :: Decl
cPtrDecl = Ptr [] (DeclRoot noLoc) noLoc

nameToId :: Name -> Id
nameToId name = mkId (translateName name)

translateName :: Name -> String
translateName (UN name) = (str name)
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
objcLog msg names = FnCall (translateVariable $ sUN "NSLog") (litString : args) noLoc
  where
    litString = ObjCLitString [StringConst [string] "" noLoc] noLoc
    string = ((pretty 80) . dquotes . text) msg
    args = map translateVariable names

idrisObjectClassDefs :: [QC.Definition]
idrisObjectClassDefs =
   [interface, implementation]
      where
         interface = ObjCClassIface className (Just $ mkId "NSObject") [] [] properties [] noLoc
         implementation = ObjCClassImpl className Nothing [] methods noLoc
         className = nameToId $ sUN "IdrisObject"

         methodType = Type (cdeclSpec [] [] (Tnamed (mkId "instancetype") [] noLoc)) (DeclRoot noLoc) noLoc
         methodPrototype = (ObjCMethodProto False (Just methodType)  [] [ObjCParam (Just (mkId "initWithConstructorID")) (Just nsIntegerType) [] (Just $ mkId "constructorID") noLoc, ObjCParam (Just $ mkId "arguments") (Just $ mkObjectType "NSArray") [] (Just $ mkId "arguments") noLoc] False [] noLoc)
         methodImplementation = initMethodImp
         methodInterface = ObjCIfaceMeth methodPrototype noLoc
         properties = [toObjCPrimitiveProperty "NSInteger" "constructorID",toObjCProperty "NSArray" "arguments", methodInterface]
         methods = [ObjCMethDef methodPrototype initMethodImp noLoc]

initMethodImp :: [QC.BlockItem]
initMethodImp =
  [ initAssignment
  , earlyNilReturn
  , assignIdentifier
  , assignArray
  , returnSelf
  ]
    where
      initAssignment = (BlockStm . mkExprStm) $ objcAssignExp (sUN "self") (ObjCMsg (ObjCRecvSuper noLoc) [(ObjCArg (Just (mkId "init")) Nothing noLoc)] [] noLoc)
      self = (translateVariable $ sUN "self")
      nil = (translateVariable $ sUN "nil")
      ifSelfEqualsNil = objcPtrEquals self nil
      earlyNilReturn = BlockStm $ If ifSelfEqualsNil (mkReturnStm nil) Nothing noLoc
      assignIdentifier = (BlockStm . mkExprStm) $ objcAssignExp (sUN "_constructorID") (translateVariable $ sUN "constructorID")
      assignArray = (BlockStm . mkExprStm) $ objcAssignExp (sUN "_arguments") (translateVariable $ sUN "arguments")
      returnSelf = (BlockStm . mkReturnStm) self

mkExprStm :: QC.Exp -> QC.Stm
mkExprStm exp = Exp (Just exp) noLoc

mkObjectType :: String -> QC.Type
mkObjectType s = QC.Type (cdeclSpec [] [] (mkObjectTypeSpec s)) cPtrDecl noLoc

nsIntegerType :: QC.Type
nsIntegerType = QC.Type (cdeclSpec [] [] (mkObjectTypeSpec "NSInteger")) (DeclRoot noLoc) noLoc

mkObjectTypeSpec :: String -> QC.TypeSpec
mkObjectTypeSpec s = (Tnamed (mkId s) [] noLoc)

intTypeSpec :: QC.TypeSpec
intTypeSpec = Tint Nothing noLoc

toObjCProperty :: String -> String -> ObjCIfaceDecl
toObjCProperty cls name = ObjCIfaceProp [ObjCNonatomic noLoc, ObjCStrong noLoc, ObjCReadonly noLoc] fieldGroup noLoc
   where
      fieldGroup = FieldGroup (cdeclSpec [] [] (mkObjectTypeSpec cls)) [Field (Just $ mkId name) (Just cPtrDecl) Nothing noLoc] noLoc

toObjCPrimitiveProperty :: String -> String -> ObjCIfaceDecl
toObjCPrimitiveProperty cls name = ObjCIfaceProp [ObjCNonatomic noLoc, ObjCAssign noLoc, ObjCReadonly noLoc] fieldGroup noLoc
   where
      fieldGroup = FieldGroup (cdeclSpec [] [] (mkObjectTypeSpec cls)) [Field (Just $ mkId name) (Just (DeclRoot noLoc)) Nothing noLoc] noLoc

mkVarId :: LVar -> Id
mkVarId (TT.Loc i) = mkId $ ("var_" ++) $ show i

translateDeclaration :: [Name] -> SExp -> [QC.BlockItem]
translateDeclaration names exp@(SChkCase var cases) =
  [BlockStm $ translateCaseStm names (varToName names var) cases]

translateDeclaration names exp@(SError error) =
  [ BlockStm $ (Exp (Just $ translateExpression names exp) noLoc) ] ++
  translateDeclaration names SNothing

translateDeclaration names exp@(SForeign _ _ "putStr" _) =
  [ BlockStm $ (Exp (Just $ translateExpression names exp) noLoc) ] ++
  translateDeclaration names SNothing

translateDeclaration names exp@(SOp _ _) =
  [ BlockStm $ (Exp (Just $ translateExpression names exp) noLoc) ] ++
  translateDeclaration names SNothing

translateDeclaration names exp =
  [(BlockStm . mkReturnStm) $ translateExpression names exp]

translateExpression :: [Name] -> SExp -> QC.Exp

translateExpression _ (SConst constant)
  | (Str s) <- constant
  = ObjCLitString [cConstant] noLoc
  | otherwise = Const cConstant noLoc
    where
      cConstant = (translateConstant constant)

translateExpression names (SApp tc name vars) =
   objcCall name (map (varToName names) vars)

translateExpression names (SLet var value body) =
   objcLet names var value body

translateExpression _ (SError error) =
   printError error

translateExpression names (SUpdate var e) =
  objcAssign (varToName names var) e

translateExpression _ SNothing = mkVar $ mkId "NSNull.null"

translateExpression names (SV var) = (translateVariable . (varToName names)) var

translateExpression names (SChkCase var cases) = translateCase names (varToName names var) cases

translateExpression names (SForeign _ _ "putStr" [(_, var)]) =
  objcLog "%@" [(varToName names var)]

translateExpression names (SCon i name vars) =
  objcCon i (map (varToName names) vars)

translateExpression _ e =
  printError $ "Not yet implemented: " ++ filter (/= '\'') (show e)

objcCon :: Int -> [Name] -> QC.Exp
objcCon i names = ObjCMsg (ObjCRecvExp allocExp noLoc) [ObjCArg (Just $ mkId "initWithConstructorID") ((Just . translateVariable . sUN . show) i) noLoc, ObjCArg (Just $ mkId "arguments") (Just $ objcArray names) noLoc] [] noLoc
  where
    allocExp = ObjCMsg (ObjCRecvClassName (mkId "IdrisObject") noLoc) [ObjCArg (Just $ mkId "alloc") Nothing noLoc] [] noLoc

objcArray :: [Name] -> QC.Exp
objcArray vars = ObjCLitArray (map translateVariable vars) noLoc

varToName :: [Name] -> LVar -> Name
varToName names (TT.Loc i) = (debugLog "varToName" names) !! i

translateCaseStm :: [Name] -> Name -> [SAlt] -> QC.Stm
translateCaseStm _ var [] =
  mkReturnStm (translateVariable var)

translateCaseStm _ var [SDefaultCase e] =
  mkReturnStm (translateExpression [var] e)

translateCaseStm _ var [SConstCase _ e] =
  mkReturnStm (translateExpression [var] e)

translateCaseStm names var cases =
  Switch constructorId (Block (map BlockStm (caseExps cases)) noLoc) noLoc
    where
      constructorId = Cond (isIdrisObject var) (objcGetProperty var (sUN "constructorID")) (translateVariable $ sUN "NSNotFound") noLoc

      caseExps :: [SAlt] -> [QC.Stm]
      caseExps [SConCase _ i _ params e] =
        [objcCase (objcPtrEquals constructorId (objcNumber i)) (mkReturnStm (translateExpression names e))]
      caseExps [SConstCase _ e] =
        [(mkDefaultStm . mkReturnStm) (translateExpression names e)]
      caseExps [SDefaultCase e] =
        [(mkDefaultStm . mkReturnStm) (translateExpression names e)]
      caseExps ((SConCase parentStackPos i _ params e) : xs) =
        objcCase (translateVariable . sUN $ show i) conCaseStm : (caseExps xs)
          where
            conCaseStm = Block statements noLoc
            statements = (mapInd assignmentExpr params) ++ [BlockStm $ mkReturnStm (translateExpression (names ++ params) e)]

            assignmentExpr name i = objcAssignInitExp (Tnamed (mkId "id") [] noLoc) name (objcObjectAtIndex (objcGetProperty var (sUN "arguments")) i)
      caseExps ((SDefaultCase e) : xs) =
        [(mkDefaultStm . mkReturnStm) (translateExpression names e)]
      caseExps ((SConstCase _ e) : xs) =
        [(mkDefaultStm . mkReturnStm) (translateExpression names e)]

translateCase :: [Name] -> Name -> [SAlt] -> QC.Exp
translateCase names var cases =
  mkBlockStmExpr $ translateCaseStm names var []

objcAssignInitExp :: TypeSpec -> Name -> Exp -> BlockItem
objcAssignInitExp ty name exp = BlockDecl $ cinitGroup (cdeclSpec [] [] ty) [] [init]
  where
    init = Init (nameToId name) (DeclRoot noLoc) Nothing (Just initializer) [] noLoc
    initializer = ExpInitializer exp noLoc

mkStm :: Exp -> Stm
mkStm e = Exp (Just e) noLoc

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

mkDefaultStm :: Stm -> Stm
mkDefaultStm stm = Default stm noLoc

isIdrisObject :: Name -> QC.Exp
isIdrisObject name =
  ObjCMsg (ObjCRecvExp (translateVariable name) noLoc) [ObjCArg (Just $ mkId "isKindOfClass") (Just cls) noLoc] [] noLoc
    where
      cls = ObjCMsg (ObjCRecvClassName (mkId "IdrisObject") noLoc) [ObjCArg (Just $ mkId "class") Nothing noLoc] [] noLoc

objcGetProperty :: Name -> Name -> QC.Exp
objcGetProperty name property = ObjCMsg (ObjCRecvExp (translateVariable name) noLoc) [ObjCArg (Just $ nameToId property) Nothing noLoc] [] noLoc

objcObjectAtIndex :: QC.Exp -> Int -> QC.Exp
objcObjectAtIndex array n = ObjCMsg (ObjCRecvExp array noLoc) [ObjCArg (Just $ mkId "objectAtIndex") (Just $ (translateVariable . sUN) (show n)) noLoc] [] noLoc

objcPtrEquals :: QC.Exp -> QC.Exp -> QC.Exp
objcPtrEquals x y = BinOp Eq x y noLoc

objcCase :: QC.Exp -> QC.Stm -> QC.Stm
objcCase e stm = Case e (Block [(BlockStm stm), (BlockStm (Break noLoc))] noLoc) noLoc

objcIf :: QC.Exp -> QC.Stm -> Maybe QC.Stm -> QC.Stm
objcIf e stm elseStm = If e stm elseStm noLoc

objcNumber :: Int -> QC.Exp
objcNumber i = ObjCLitConst Nothing (IntConst "" Unsigned (toInteger i) noLoc) noLoc

mkBlockStmExpr :: QC.Stm -> QC.Exp
mkBlockStmExpr = mkStmExpr. (:[]) . mkBlockStm

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
objcAssign name e = objcAssignExp name value
  where
    value = translateExpression [] e

objcAssignExp :: Name -> QC.Exp -> QC.Exp
objcAssignExp name e = Assign identifier JustAssign e noLoc
  where
    identifier = translateVariable name

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
     value = translateExpression names sValue
     name = mkVarName var
     exprBody = translateExpression (names ++ [name]) body
     items = [(BlockStm . mkReturnStm) exprBody]

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
