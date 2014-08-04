module IRTS.Swift.AST where

import Data.List.NonEmpty

data SwiftType = ArrayTy
               | FunTy
               | IdentTy
               | TupleTy
               | OptionalTy
               | ImplicitOptionalTy
               | ProtocolTy
               | MetaTy

data Statement = Expr
              | Decl
              | Loop
              | Branch
              | Labeled

data Loop = For
          | ForIn
          | While
          | DoWhile

data OpChar = Div
            | Eq
            | Sub
            | Add
            | Bang
            | Times
            | Modulo
            | LT
            | GT
            | And
            | Or
            | Caret
            | Tilde
            | Dot

data Op = MkOp OpChar (Maybe Op)

type BinOp = Op
type PreOp = Op
type PostOp = Op

data PostExpr = PrimExpr
              | PostOpExpr PostExpr PostOp
              | FunCallExpr
              | InitExpr
              | ExplicitMemberExpr
              | PostSelfExpr
              | DynamicTypeExpr
              | SubscriptExpr
              | ForcedValueExpr
              | OptionalChainExpr

data PreExpr = MkPreExpr (Maybe PreOp) PostExpr

data AssignOp = AssignOp

data CondOp = CondOp Expr

data TypeCastOp = IsCastOp SwiftType
                | AsCastOp SwiftType
                | MaybeAsCastOp SwiftType

data BinExpr = BinExpr BinOp PreExpr
             | AssignExpr AssignOp PreExpr
             | CondOpExpr CondOp PreExpr
             | CastExpr TypeCastOp

type BinExprs = NonEmpty BinExpr

data Expr = MkExpr PreExpr (Maybe BinExpr)

data ExprList = NonEmpty Expr

data ForInit = ForVar VarDecl | ForExprs ExprList

data CodeBlock = CodeBlock (Maybe Statements)

data For = ForLoop (Maybe ForInit) (Maybe Expr) (Maybe Expr) CodeBlock

type Statements = NonEmpty Statement

type TopLevelDecl = Maybe Statements

data Attr = AutoClosure | NoReturn

type Attrs = NonEmpty Attr

data TypeAnnotation = MkAnnotation (Maybe Attrs) SwiftType

type GenericArg = SwiftType

type GenericArgClause = NonEmpty GenericArg

type Ident = String

type TypeName = Ident

type TypeIdent = NonEmpty (TypeName, (Maybe GenericArgClause))

type EnumCaseName = Ident

type TuplePattern = NonEmpty Pattern

data Pattern = WildcardPattern (Maybe TypeAnnotation)
             | IdentPattern (Maybe TypeAnnotation)
             | ValueBindingPattern
             | TuplePattern (Maybe TypeAnnotation)
             | EnumCasePattern (Maybe TypeIdent) EnumCaseName (Maybe TuplePattern)
             | TypeCastPattern
             | ExprPattern

-- FIXME: Initializers are not quite strings
data Init = String

data PatternInit = MkPatternInit Pattern (Maybe Init)

type PatternInits = NonEmpty PatternInit

data AccessModifier = Internal
                    | InternalSet
                    | Private
                    | PrivateSet
                    | Public
                    | PublicSet

type AccessModifiers = NonEmpty AccessModifier

data DeclModifier = Class
                  | Convenience
                  | Dynamic
                  | Final
                  | Lazy
                  | Mutating
                  | NonMutating
                  | Optional
                  | Override
                  | Required
                  | Static
                  | Unowned
                  | UnownedSafe
                  | UnownedUnsafe
                  | Weak
                  | MkAccessModifier AccessModifier

type DeclModifiers = NonEmpty DeclModifier

data ConstDecl = MkConstDecl (Maybe Attrs) (Maybe DeclModifiers) PatternInits

data VarDeclHead = MkVarDeclHead (Maybe Attrs) (Maybe DeclModifiers)

type VarName = Ident

data GetClause = MkGetClause (Maybe Attrs) CodeBlock

data SetClause = MkSetClause (Maybe Attrs) (Maybe SetterName) CodeBlock

type SetterName = Ident

data GetSetBlock = MkGetSetBlock GetClause (Maybe SetClause)

data GetSetKeywordBlock = MkGetSetKeywordBlock GetKeywordClause (Maybe SetKeywordClause)

data GetKeywordClause = MkGetKeywordClause (Maybe Attrs)

data SetKeywordClause = MkSetKeywordClause (Maybe Attrs)

data WillSetClause = MkWillSetClause (Maybe Attrs) (Maybe SetterName) CodeBlock

data DidSetClause = MkDidSetClause (Maybe Attrs) (Maybe SetterName) CodeBlock

data WillSetDidSetBlock = MkWillDidSetBlock WillSetClause (Maybe DidSetClause)

data VarDecl = EmptyDecl VarDeclHead PatternInits
             | BlockDecl VarDeclHead VarName TypeAnnotation CodeBlock
             | GetSetDecl VarDeclHead VarName TypeAnnotation GetSetBlock
             | GetSetKeywordDecl VarDeclHead VarName TypeAnnotation GetSetKeywordBlock
             | WillSetDidSetDecl VarDeclHead VarName TypeAnnotation (Maybe Init) WillSetDidSetBlock

data TypealiasDecl = MkTypealiasDecl TypealiasHead TypealiasAssign

data TypealiasHead = MkTypeAliasHead (Maybe Attrs) (Maybe AccessModifier) TypealiasName

type TypealiasName = Ident

type TypealiasAssign = SwiftType

data ImportDecl = MkImportDecl (Maybe Attrs) (Maybe ImportKind) ImportPath

data ImportKind = TypealiasImport | StructImport | ClassImport | EnumImport | ProtocolImport | VarImport | FuncImport

type ImportPath = NonEmpty ImportPathIdent

data ImportPathIdent = MkImportPathIdent Ident | MkImportPathOp Op

data FunDecl = FunHead FunName (Maybe GenericParamClause) FunSignature FunBody 

data FunHead = MkFunHead (Maybe Attrs) (Maybe DeclModifiers)

data FunName = MkFunNameIdent Ident | MkFunNameOp Op

data FunSignature = MkFunSignature ParamClauses (Maybe FunResult)

data FunResult = MkFunResult (Maybe Attrs) SwiftType

type FunBody = CodeBlock

data ParamClause = EmptyClause | MkParamClause Params | MkVariableParamClause Params

type Params = NonEmpty Param

data InOutIdent = InOutIdent

data LetIdent = LetIdent

data HashIdent = HashIdent

data WildcardIdent

data ExtParamName = Ident | WildcardIdent

data Param = MkParam (Maybe InOutIdent) (Maybe LetIdent) (Maybe HashIdent) (Maybe ExtParamName) LocalParamName TypeAnnotation (Maybe DefaultArgClause)

type ParamClauses = NonEmpty ParamClause

data Decl = ImportDecl
          | ConstDecl
          | VarDecl
          | TypealiasDecl
          | FunDecl
          | EnumDecl
          | StructDecl
          | ClassDecl
          | ProtocolDecl
          | InitDecl
          | DeinitDecl
          | ExtDecl
          | SubscriptDecl
          | OpDecl
          | MkDecl Decl (Maybe Declarations)

type Declarations = NonEmpty Decl
