module CXErrorCode : sig

  type t = 
    | Success
    | Failure
    | Crashed
    | InvalidArguments
    | ASTReadError

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXAvailabilityKind : sig

  type t = 
    | Available
    | Deprecated
    | NotAvailable
    | NotAccessible

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXDiagnosticSeverity : sig

  type t = 
    | Ignored
    | Note
    | Warning
    | Error
    | Fatal

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXLoadDiag_Error : sig

  type t = 
    | None
    | Unknown
    | CannotLoad
    | InvalidFile

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXDiagnosticDisplayOptions : sig

  type t = 
    | DisplaySourceLocation
    | DisplayColumn
    | DisplaySourceRanges
    | DisplayOption
    | DisplayCategoryId
    | DisplayCategoryName

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXTranslationUnit_Flags : sig

  type t = 
    | None
    | DetailedPreprocessingRecord
    | Incomplete
    | PrecompiledPreamble
    | CacheCompletionResults
    | ForSerialization
    | CXXChainedPCH
    | SkipFunctionBodies
    | IncludeBriefCommentsInCodeCompletion
    | CreatePreambleOnFirstParse

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXSaveTranslationUnit_Flags : sig

  type t = 
    | None

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXSaveError : sig

  type t = 
    | None
    | Unknown
    | TranslationErrors
    | InvalidTU

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXTUResourceUsageKind : sig

  type t = 
    | AST
    | Identifiers
    | Selectors
    | GlobalCompletionResults
    | SourceManagerContentCache
    | AST_SideTables
    | SourceManager_Membuffer_Malloc
    | SourceManager_Membuffer_MMap
    | ExternalASTSource_Membuffer_Malloc
    | ExternalASTSource_Membuffer_MMap
    | Preprocessor
    | PreprocessingRecord
    | SourceManager_DataStructures
    | Preprocessor_HeaderSearch
    | MEMORY_IN_BYTES_BEGIN
    | MEMORY_IN_BYTES_END
    | First
    | Last

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXCursorKind : sig

  type t = 
    | UnexposedDecl
    | StructDecl
    | UnionDecl
    | ClassDecl
    | EnumDecl
    | FieldDecl
    | EnumConstantDecl
    | FunctionDecl
    | VarDecl
    | ParmDecl
    | ObjCInterfaceDecl
    | ObjCCategoryDecl
    | ObjCProtocolDecl
    | ObjCPropertyDecl
    | ObjCIvarDecl
    | ObjCInstanceMethodDecl
    | ObjCClassMethodDecl
    | ObjCImplementationDecl
    | ObjCCategoryImplDecl
    | TypedefDecl
    | CXXMethod
    | Namespace
    | LinkageSpec
    | Constructor
    | Destructor
    | ConversionFunction
    | TemplateTypeParameter
    | NonTypeTemplateParameter
    | TemplateTemplateParameter
    | FunctionTemplate
    | ClassTemplate
    | ClassTemplatePartialSpecialization
    | NamespaceAlias
    | UsingDirective
    | UsingDeclaration
    | TypeAliasDecl
    | ObjCSynthesizeDecl
    | ObjCDynamicDecl
    | CXXAccessSpecifier
    | FirstDecl
    | LastDecl
    | FirstRef
    | ObjCSuperClassRef
    | ObjCProtocolRef
    | ObjCClassRef
    | TypeRef
    | CXXBaseSpecifier
    | TemplateRef
    | NamespaceRef
    | MemberRef
    | LabelRef
    | OverloadedDeclRef
    | VariableRef
    | LastRef
    | FirstInvalid
    | InvalidFile
    | NoDeclFound
    | NotImplemented
    | InvalidCode
    | LastInvalid
    | FirstExpr
    | UnexposedExpr
    | DeclRefExpr
    | MemberRefExpr
    | CallExpr
    | ObjCMessageExpr
    | BlockExpr
    | IntegerLiteral
    | FloatingLiteral
    | ImaginaryLiteral
    | StringLiteral
    | CharacterLiteral
    | ParenExpr
    | UnaryOperator
    | ArraySubscriptExpr
    | BinaryOperator
    | CompoundAssignOperator
    | ConditionalOperator
    | CStyleCastExpr
    | CompoundLiteralExpr
    | InitListExpr
    | AddrLabelExpr
    | StmtExpr
    | GenericSelectionExpr
    | GNUNullExpr
    | CXXStaticCastExpr
    | CXXDynamicCastExpr
    | CXXReinterpretCastExpr
    | CXXConstCastExpr
    | CXXFunctionalCastExpr
    | CXXTypeidExpr
    | CXXBoolLiteralExpr
    | CXXNullPtrLiteralExpr
    | CXXThisExpr
    | CXXThrowExpr
    | CXXNewExpr
    | CXXDeleteExpr
    | UnaryExpr
    | ObjCStringLiteral
    | ObjCEncodeExpr
    | ObjCSelectorExpr
    | ObjCProtocolExpr
    | ObjCBridgedCastExpr
    | PackExpansionExpr
    | SizeOfPackExpr
    | LambdaExpr
    | ObjCBoolLiteralExpr
    | ObjCSelfExpr
    | OMPArraySectionExpr
    | LastExpr
    | FirstStmt
    | UnexposedStmt
    | LabelStmt
    | CompoundStmt
    | CaseStmt
    | DefaultStmt
    | IfStmt
    | SwitchStmt
    | WhileStmt
    | DoStmt
    | ForStmt
    | GotoStmt
    | IndirectGotoStmt
    | ContinueStmt
    | BreakStmt
    | ReturnStmt
    | GCCAsmStmt
    | AsmStmt
    | ObjCAtTryStmt
    | ObjCAtCatchStmt
    | ObjCAtFinallyStmt
    | ObjCAtThrowStmt
    | ObjCAtSynchronizedStmt
    | ObjCAutoreleasePoolStmt
    | ObjCForCollectionStmt
    | CXXCatchStmt
    | CXXTryStmt
    | CXXForRangeStmt
    | SEHTryStmt
    | SEHExceptStmt
    | SEHFinallyStmt
    | MSAsmStmt
    | NullStmt
    | DeclStmt
    | OMPParallelDirective
    | OMPSimdDirective
    | OMPForDirective
    | OMPSectionsDirective
    | OMPSectionDirective
    | OMPSingleDirective
    | OMPParallelForDirective
    | OMPParallelSectionsDirective
    | OMPTaskDirective
    | OMPMasterDirective
    | OMPCriticalDirective
    | OMPTaskyieldDirective
    | OMPBarrierDirective
    | OMPTaskwaitDirective
    | OMPFlushDirective
    | SEHLeaveStmt
    | OMPOrderedDirective
    | OMPAtomicDirective
    | OMPForSimdDirective
    | OMPParallelForSimdDirective
    | OMPTargetDirective
    | OMPTeamsDirective
    | OMPTaskgroupDirective
    | OMPCancellationPointDirective
    | OMPCancelDirective
    | OMPTargetDataDirective
    | OMPTaskLoopDirective
    | OMPTaskLoopSimdDirective
    | OMPDistributeDirective
    | LastStmt
    | TranslationUnit
    | FirstAttr
    | UnexposedAttr
    | IBActionAttr
    | IBOutletAttr
    | IBOutletCollectionAttr
    | CXXFinalAttr
    | CXXOverrideAttr
    | AnnotateAttr
    | AsmLabelAttr
    | PackedAttr
    | PureAttr
    | ConstAttr
    | NoDuplicateAttr
    | CUDAConstantAttr
    | CUDADeviceAttr
    | CUDAGlobalAttr
    | CUDAHostAttr
    | CUDASharedAttr
    | VisibilityAttr
    | DLLExport
    | DLLImport
    | LastAttr
    | PreprocessingDirective
    | MacroDefinition
    | MacroExpansion
    | MacroInstantiation
    | InclusionDirective
    | FirstPreprocessing
    | LastPreprocessing
    | ModuleImportDecl
    | TypeAliasTemplateDecl
    | FirstExtraDecl
    | LastExtraDecl
    | OverloadCandidate

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXLinkageKind : sig

  type t = 
    | Invalid
    | NoLinkage
    | Internal
    | UniqueExternal
    | External

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXVisibilityKind : sig

  type t = 
    | Invalid
    | Hidden
    | Protected
    | Default

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXLanguageKind : sig

  type t = 
    | Invalid
    | C
    | ObjC
    | CPlusPlus

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXTypeKind : sig

  type t = 
    | Invalid
    | Unexposed
    | Void
    | Bool
    | Char_U
    | UChar
    | Char16
    | Char32
    | UShort
    | UInt
    | ULong
    | ULongLong
    | UInt128
    | Char_S
    | SChar
    | WChar
    | Short
    | Int
    | Long
    | LongLong
    | Int128
    | Float
    | Double
    | LongDouble
    | NullPtr
    | Overload
    | Dependent
    | ObjCId
    | ObjCClass
    | ObjCSel
    | FirstBuiltin
    | LastBuiltin
    | Complex
    | Pointer
    | BlockPointer
    | LValueReference
    | RValueReference
    | Record
    | Enum
    | Typedef
    | ObjCInterface
    | ObjCObjectPointer
    | FunctionNoProto
    | FunctionProto
    | ConstantArray
    | Vector
    | IncompleteArray
    | VariableArray
    | DependentSizedArray
    | MemberPointer
    | Auto

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXCallingConv : sig

  type t = 
    | Default
    | C
    | X86StdCall
    | X86FastCall
    | X86ThisCall
    | X86Pascal
    | AAPCS
    | AAPCS_VFP
    | IntelOclBicc
    | X86_64Win64
    | X86_64SysV
    | X86VectorCall
    | Invalid
    | Unexposed

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXTemplateArgumentKind : sig

  type t = 
    | Null
    | Type
    | Declaration
    | NullPtr
    | Integral
    | Template
    | TemplateExpansion
    | Expression
    | Pack
    | Invalid

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXTypeLayoutError : sig

  type t = 
    | Invalid
    | Incomplete
    | Dependent
    | NotConstantSize
    | InvalidFieldName

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CX_CXXAccessSpecifier : sig

  type t = 
    | InvalidAccessSpecifier
    | Public
    | Protected
    | Private

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CX_StorageClass : sig

  type t = 
    | C_Invalid
    | C_None
    | C_Extern
    | C_Static
    | C_PrivateExtern
    | C_OpenCLWorkGroupLocal
    | C_Auto
    | C_Register

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXChildVisitResult : sig

  type t = 
    | Break
    | Continue
    | Recurse

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXNameRefFlags : sig

  type t = 
    | CXNameRange_WantQualifier
    | CXNameRange_WantTemplateArgs
    | CXNameRange_WantSinglePiece

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXTokenKind : sig

  type t = 
    | Punctuation
    | Keyword
    | Identifier
    | Literal
    | Comment

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXCompletionChunkKind : sig

  type t = 
    | Optional
    | TypedText
    | Text
    | Placeholder
    | Informative
    | CurrentParameter
    | LeftParen
    | RightParen
    | LeftBracket
    | RightBracket
    | LeftBrace
    | RightBrace
    | LeftAngle
    | RightAngle
    | Comma
    | ResultType
    | Colon
    | SemiColon
    | Equal
    | HorizontalSpace
    | VerticalSpace

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXCodeComplete_Flags : sig

  type t = 
    | IncludeMacros
    | IncludeCodePatterns
    | IncludeBriefComments

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXCompletionContext : sig

  type t = 
    | Unexposed
    | AnyType
    | AnyValue
    | ObjCObjectValue
    | ObjCSelectorValue
    | CXXClassTypeValue
    | DotMemberAccess
    | ArrowMemberAccess
    | ObjCPropertyAccess
    | EnumTag
    | UnionTag
    | StructTag
    | ClassTag
    | Namespace
    | NestedNameSpecifier
    | ObjCInterface
    | ObjCProtocol
    | ObjCCategory
    | ObjCInstanceMessage
    | ObjCClassMessage
    | ObjCSelectorName
    | MacroName
    | NaturalLanguage
    | Unknown

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

module CXVisitorResult : sig

  type t = 
    | Break
    | Continue

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string

end

