module IRTS.Swift.AST where

import Prelude hiding (Enum)
import Data.List.NonEmpty

data TupleTy = TupleTy (Maybe TupleTyElements) | VariableTupleTy (Maybe TupleTyElements)

type TupleTyElements = NonEmpty TupleTyElement

type ElementName = Ident

data TupleTyElement = TupleTyElement (Maybe Attrs) (Maybe InOutIdent) SwiftType
                    | AnnotatedTupleTyElement (Maybe InOutIdent) ElementName TypeAnnotation

data SwiftType = ArrayTy
               | FunTy
               | IdentTy
               | TupleType TupleTy
               | OptionalTy
               | ImplicitOptionalTy
               | ProtocolTy
               | MetaTy

data SwitchCase = Case CaseLabel Statements | Default DefaultLabel Statements | CasePassthrough CaseLabel | DefaultPassthrough DefaultLabel

data CaseLabel = CaseLabel (NonEmpty CaseItems)

type CaseItems = NonEmpty (Pattern, (Maybe GuardClause))

data DefaultLabel = DefaultLabel

data GuardClause = GuardClause GuardExpr

type GuardExpr = Expr

type LabelName = Ident

data Labeled = LabeledLoop LabelName Loop | LabeledSwitch LabelName Switch

type SwitchCases = NonEmpty SwitchCase

data Switch = Switch Expr (Maybe SwitchCases)

data ElseClause = Else CodeBlock | ElseIf If

data IfCond = IfExpr Expr | IfDecl Decl

data If = If IfCond CodeBlock (Maybe ElseClause)

data Branch = IfBranch If | SwitchBranch Switch

data Break = Break (Maybe LabelName)

data Continue = Continue (Maybe LabelName)

data Fallthrough = Fallthrough

data Return = Return (Maybe Expr)

data ControlTransfer = BreakStm Break | ContinueStm Continue | FallthroughStm Fallthrough | ReturnStm Return

data Statement = ExprStm Expr
              | DeclStm Decl
              | LoopStm Loop
              | BranchStm Branch
              | LabeledStm Labeled
              | ControlTransferStm ControlTransfer

data ForIn = ForIn Pattern Expr CodeBlock

data While = While WhileCond CodeBlock

data DoWhile = DoWhile CodeBlock While

data WhileCond = WhileExpr Expr | WhileDecl Decl

data Loop = ForLoop For
          | ForInLoop ForIn
          | WhileLoop While
          | DoWhileLoop DoWhile


data Op = PrefixOp PreOp | PostfixOp PostOp | InfixOp InOp

-- TODO add valid unicode chars
data OpHead = Div
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

-- TODO Add valid Unicode chars
data OpChar = OpHeadChar OpHead

type OpChars = NonEmpty OpChar

data DotOpHead = DotOpHead

data DotOpChar = DotOpChar OpChar | DotOpDot

type DotOpChars = NonEmpty DotOpChar

data OpToken = OpToken OpHead (Maybe OpChars) | DotOpToken DotOpHead (Maybe DotOpChars)

data PreOp = PreOp OpToken
data PostOp = PostOp OpToken

data PrecedenceClause = PrecedenceClause Int

data AssocClause = Left | Right | None

data InOpAttrs = InOpAttrs (Maybe PrecedenceClause) (Maybe AssocClause)
data InOp = InOp OpToken (Maybe InOpAttrs)

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

type BinOp = Op

data BinExpr = BinExpr BinOp PreExpr
             | AssignExpr AssignOp PreExpr
             | CondOpExpr CondOp PreExpr
             | CastExpr TypeCastOp

type BinExprs = NonEmpty BinExpr

data Expr = MkExpr PreExpr (Maybe BinExpr)

type Exprs = NonEmpty Expr

data ForInit = ForVar Var | ForExprs Exprs

data CodeBlock = CodeBlock (Maybe Statements)

data For = For (Maybe ForInit) (Maybe Expr) (Maybe Expr) CodeBlock

type Statements = NonEmpty Statement

type TopLevelDecl = Maybe Statements

data Attr = AutoClosure | NoReturn

type Attrs = NonEmpty Attr

data TypeAnnotation = MkAnnotation (Maybe Attrs) SwiftType

type GenericArg = SwiftType

type GenericArgClause = NonEmpty GenericArg

-- FIXME: Identifiers are not quite strings
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

type InitExpr = Expr

data PatternInit = MkPatternInit Pattern (Maybe InitExpr)

type PatternInits = NonEmpty PatternInit

data AccessModifier = Internal
                    | InternalSet
                    | Private
                    | PrivateSet
                    | Public
                    | PublicSet

type AccessModifiers = NonEmpty AccessModifier

data DeclModifier = ClassMod
                  | ConvenienceMod
                  | DynamicMod
                  | FinalMod
                  | LazyMod
                  | MutatingMod
                  | NonMutatingMod
                  | OptionalMod
                  | OverrideMod
                  | RequiredMod
                  | StaticMod
                  | UnownedMod
                  | UnownedSafeMod
                  | UnownedUnsafeMod
                  | WeakMod
                  | AccessMod AccessModifier

type DeclModifiers = NonEmpty DeclModifier

data Const = Const (Maybe Attrs) (Maybe DeclModifiers) PatternInits

data VarDeclHead = VarDeclHead (Maybe Attrs) (Maybe DeclModifiers)

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

data Var = EmptyDecl VarDeclHead PatternInits
             | BlockDecl VarDeclHead VarName TypeAnnotation CodeBlock
             | GetSetDecl VarDeclHead VarName TypeAnnotation GetSetBlock
             | GetSetKeywordDecl VarDeclHead VarName TypeAnnotation GetSetKeywordBlock
             | WillSetDidSetDecl VarDeclHead VarName TypeAnnotation (Maybe Init) WillSetDidSetBlock

data Typealias = Typealias TypealiasHead TypealiasAssign

data TypealiasHead = MkTypeAliasHead (Maybe Attrs) (Maybe AccessModifier) TypealiasName

type TypealiasName = Ident

type TypealiasAssign = SwiftType

data Import = Import (Maybe Attrs) (Maybe ImportKind) ImportPath

data ImportKind = TypealiasImport | StructImport | ClassImport | EnumImport | ProtocolImport | VarImport | FuncImport

type ImportPath = NonEmpty ImportPathIdent

data ImportPathIdent = MkImportPathIdent Ident | MkImportPathOp Op

type ProtocolIdent = TypeIdent

type ProtocolIdents = NonEmpty ProtocolIdent

data ProtocolTy = MkProtocolTy ProtocolIdents

data Req = MkConformanceReq TypeIdent TypeIdent | MkProtocolReq TypeIdent ProtocolTy | MkSameTypeReq TypeIdent TypeIdent

type Reqs = NonEmpty Req

data ReqClause = MkReqClause Reqs

data GenericParam = MkGenericParam TypeName | MkConformanceGenericParam TypeName TypeIdent | MkProtocolGenericParam TypeName ProtocolTy

type GenericParams = NonEmpty GenericParam

data GenericParamClause = MkGenericParamClause GenericParams (Maybe ReqClause)

data Fun = Fun FunHead FunName (Maybe GenericParamClause) FunSignature FunBody

data FunHead = MkFunHead (Maybe Attrs) (Maybe DeclModifiers)

data FunName = MkFunNameIdent Ident | MkFunNameOp Op

data FunSignature = MkFunSignature ParamClauses (Maybe FunResult)

data FunResult = MkFunResult (Maybe Attrs) SwiftType

type FunBody = CodeBlock

data ParamClause = EmptyClause | ParamClause Params | VariableParamClause Params

type Params = NonEmpty Param

data InOutIdent = InOutIdent

data LetIdent = LetIdent

data HashIdent = HashIdent

data WildcardIdent = WildcardIdent

data ExtParamName = MkExtIdent Ident | MkExtWildcard WildcardIdent

data LocalParamName = MkLocalIdent Ident | MkLocalWildcard WildcardIdent

type DefaultArgClause = Expr

data Param = MkParam (Maybe InOutIdent) (Maybe LetIdent) (Maybe HashIdent) (Maybe ExtParamName) LocalParamName TypeAnnotation (Maybe DefaultArgClause)

type ParamClauses = NonEmpty ParamClause

-- TODO this should be a literal
data ValueAssign = ValueAssign

data ValueEnumCase = MkValueEnumCase EnumCaseName (Maybe ValueAssign)

type ValueEnumCases = NonEmpty ValueEnumCase

data ValueCaseClause = MkValueCaseClause (Maybe Attrs) ValueEnumCases

data ValueMember = MkValueMember Decl | ValueCaseClause

type ValueMembers = NonEmpty UnionMember

data ValueEnum = MkValueEnum EnumName (Maybe GenericParamClause) TypeIdent (Maybe ValueMembers)

type EnumName = Ident

data UnionEnumCase = MkUnionEnumCase EnumCaseName (Maybe TupleTy)

type UnionEnumCases = NonEmpty UnionEnumCase

data UnionCaseClause = MkUnionCaseClause (Maybe Attrs) UnionEnumCases

data UnionMember = MkUnionDeclMember Decl | MkUnionCaseMember UnionCaseClause

data UnionMembers = NonEmpty UnionMember

data UnionEnum = MkUnionEnum EnumName (Maybe GenericParamClause) (Maybe UnionMembers)

data Enum = Enum (Maybe Attrs) (Maybe AccessModifier) UnionEnum | ValueEnum (Maybe Attrs) (Maybe AccessModifier) ValueEnum

type TypeInheritanceClauses = NonEmpty TypeIdent

type StructName = Ident

data StructBody = StructBody (Maybe Declarations)

data Struct = Struct (Maybe Attrs) (Maybe AccessModifier) StructName (Maybe GenericParamClause) (Maybe TypeInheritanceClauses) StructBody

data ClassBody = ClassBody (Maybe Declarations)

type ClassName = Ident

data Class = Class (Maybe Attrs) (Maybe AccessModifier) ClassName (Maybe GenericParamClause) (Maybe TypeInheritanceClauses) ClassBody

data InitHead = InitHead (Maybe Attrs) (Maybe DeclModifiers)

type InitBody = CodeBlock

data Init = Init InitHead (Maybe GenericParamClause) ParamClause InitBody

data ProtocolProperty = ProtocolProperty FunHead FunName (Maybe GenericParamClause) FunSignature

data ProtocolMethod = ProtocolMethod FunHead FunName (Maybe GenericParamClause) FunSignature

data ProtocolInit = ProtocolInit InitHead (Maybe GenericParamClause) ParamClause

data SubscriptResult = SubscriptResult (Maybe Attrs) SwiftType

data SubscriptHead = SubscriptHead (Maybe Attrs) (Maybe DeclModifiers) ParamClause

data Subscript = Subscript SubscriptHead SubscriptResult CodeBlock
               | GetSetSubscript SubscriptHead SubscriptResult GetSetBlock
               | GetSetKeywordSubscript SubscriptHead SubscriptResult GetSetKeywordBlock

data ProtocolSubscript = ProtocolSubscript SubscriptHead SubscriptResult GetSetKeywordBlock

data ProtocolAssocType = TypealiasHead (Maybe TypeInheritanceClauses) (Maybe TypealiasAssign)

data ProtocolMemberDecl = ProtocolPropertyDecl ProtocolProperty
                    | ProtocolMethodDecl ProtocolMethod
                    | ProtocolInitDecl ProtocolInit
                    | ProtocolSubscriptDecl ProtocolSubscript
                    | ProtocolAssocTypeDecl ProtocolAssocType

type ProtocolMemberDecls = NonEmpty ProtocolMemberDecl

data ProtocolBody = ProtocolBody (Maybe ProtocolMemberDecls)

type ProtocolName = Ident

data Protocol = Protocol (Maybe Attrs) (Maybe AccessModifier) ProtocolName (Maybe TypeInheritanceClauses) ProtocolBody

data DeInit = DeInit (Maybe Attrs) CodeBlock

type ExtensionBody = Maybe Declarations

data Extension = Extension (Maybe AccessModifier) TypeIdent (Maybe TypeInheritanceClauses) ExtensionBody

data Decl = ImportDecl Import
          | ConstDecl Const
          | VarDecl Var
          | TypealiasDecl Typealias
          | FunDecl Fun
          | EnumDecl Enum
          | StructDecl Struct
          | ClassDecl Class
          | ProtocolDecl Protocol
          | InitDecl Init
          | DeInitDecl DeInit
          | ExtensionDecl Extension
          | SubscriptDecl Subscript
          | OpDecl Op
          | MkDecl Decl (Maybe Declarations)

type Declarations = NonEmpty Decl
