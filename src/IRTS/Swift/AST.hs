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



data Pattern = WildcardPattern (Maybe TypeAnnotation)
             | IdentPattern (Maybe TypeAnnotation)
             | ValueBindingPattern
             | TuplePattern (Maybe TypeAnnotation)
             | EnumCasePattern (Maybe TypeIdent) EnumCaseName (Maybe TuplePattern)
             | TypeCastPattern
             | ExprPattern

data PatternInit = MkPatternInit Pattern (Maybe Init)

type PatternInits = NonEmpty PatternInit

data ConstDecl = MkConstDecl (Maybe Attrs) (Maybe DeclSpecifiers) PatternInits

data VarDecl = EmptyDecl VarDeclHead PatternInits
             | BlockDecl VarDeclHead VarName TypeAnnotation CodeBlock
             | GetSetDecl VarDeclHead VarName TypeAnnotation GetSetBlock
             | GetSetKeywordDecl VarDeclHead VarName TypeAnnotation GetSetKeywordBlock
             | WillSetDidSetDecl VarDeclHead VarName TypeAnnotation (Maybe Init) WillSetDidSetDecl

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
