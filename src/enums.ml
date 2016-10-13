module CXErrorCode = struct

  type t = 
    | Success
    | Failure
    | Crashed
    | InvalidArguments
    | ASTReadError

  let to_int64 = function
    | Success -> 0L
    | Failure -> 1L
    | Crashed -> 2L
    | InvalidArguments -> 3L
    | ASTReadError -> 4L

  let of_int64 = function
    | 0L -> Success
    | 1L -> Failure
    | 2L -> Crashed
    | 3L -> InvalidArguments
    | 4L -> ASTReadError
    | _ -> failwith "CXErrorCode.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXAvailabilityKind = struct

  type t = 
    | Available
    | Deprecated
    | NotAvailable
    | NotAccessible

  let to_int64 = function
    | Available -> 0L
    | Deprecated -> 1L
    | NotAvailable -> 2L
    | NotAccessible -> 3L

  let of_int64 = function
    | 0L -> Available
    | 1L -> Deprecated
    | 2L -> NotAvailable
    | 3L -> NotAccessible
    | _ -> failwith "CXAvailabilityKind.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXDiagnosticSeverity = struct

  type t = 
    | Ignored
    | Note
    | Warning
    | Error
    | Fatal

  let to_int64 = function
    | Ignored -> 0L
    | Note -> 1L
    | Warning -> 2L
    | Error -> 3L
    | Fatal -> 4L

  let of_int64 = function
    | 0L -> Ignored
    | 1L -> Note
    | 2L -> Warning
    | 3L -> Error
    | 4L -> Fatal
    | _ -> failwith "CXDiagnosticSeverity.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXLoadDiag_Error = struct

  type t = 
    | None
    | Unknown
    | CannotLoad
    | InvalidFile

  let to_int64 = function
    | None -> 0L
    | Unknown -> 1L
    | CannotLoad -> 2L
    | InvalidFile -> 3L

  let of_int64 = function
    | 0L -> None
    | 1L -> Unknown
    | 2L -> CannotLoad
    | 3L -> InvalidFile
    | _ -> failwith "CXLoadDiag_Error.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXDiagnosticDisplayOptions = struct

  type t = 
    | DisplaySourceLocation
    | DisplayColumn
    | DisplaySourceRanges
    | DisplayOption
    | DisplayCategoryId
    | DisplayCategoryName

  let to_int64 = function
    | DisplaySourceLocation -> 1L
    | DisplayColumn -> 2L
    | DisplaySourceRanges -> 4L
    | DisplayOption -> 8L
    | DisplayCategoryId -> 16L
    | DisplayCategoryName -> 32L

  let of_int64 = function
    | 1L -> DisplaySourceLocation
    | 2L -> DisplayColumn
    | 4L -> DisplaySourceRanges
    | 8L -> DisplayOption
    | 16L -> DisplayCategoryId
    | 32L -> DisplayCategoryName
    | _ -> failwith "CXDiagnosticDisplayOptions.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXTranslationUnit_Flags = struct

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

  let to_int64 = function
    | None -> 0L
    | DetailedPreprocessingRecord -> 1L
    | Incomplete -> 2L
    | PrecompiledPreamble -> 4L
    | CacheCompletionResults -> 8L
    | ForSerialization -> 16L
    | CXXChainedPCH -> 32L
    | SkipFunctionBodies -> 64L
    | IncludeBriefCommentsInCodeCompletion -> 128L
    | CreatePreambleOnFirstParse -> 256L

  let of_int64 = function
    | 0L -> None
    | 1L -> DetailedPreprocessingRecord
    | 2L -> Incomplete
    | 4L -> PrecompiledPreamble
    | 8L -> CacheCompletionResults
    | 16L -> ForSerialization
    | 32L -> CXXChainedPCH
    | 64L -> SkipFunctionBodies
    | 128L -> IncludeBriefCommentsInCodeCompletion
    | 256L -> CreatePreambleOnFirstParse
    | _ -> failwith "CXTranslationUnit_Flags.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXSaveTranslationUnit_Flags = struct

  type t = 
    | None

  let to_int64 = function
    | None -> 0L

  let of_int64 = function
    | 0L -> None
    | _ -> failwith "CXSaveTranslationUnit_Flags.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXSaveError = struct

  type t = 
    | None
    | Unknown
    | TranslationErrors
    | InvalidTU

  let to_int64 = function
    | None -> 0L
    | Unknown -> 1L
    | TranslationErrors -> 2L
    | InvalidTU -> 3L

  let of_int64 = function
    | 0L -> None
    | 1L -> Unknown
    | 2L -> TranslationErrors
    | 3L -> InvalidTU
    | _ -> failwith "CXSaveError.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXTUResourceUsageKind = struct

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

  let to_int64 = function
    | AST -> 1L
    | Identifiers -> 2L
    | Selectors -> 3L
    | GlobalCompletionResults -> 4L
    | SourceManagerContentCache -> 5L
    | AST_SideTables -> 6L
    | SourceManager_Membuffer_Malloc -> 7L
    | SourceManager_Membuffer_MMap -> 8L
    | ExternalASTSource_Membuffer_Malloc -> 9L
    | ExternalASTSource_Membuffer_MMap -> 10L
    | Preprocessor -> 11L
    | PreprocessingRecord -> 12L
    | SourceManager_DataStructures -> 13L
    | Preprocessor_HeaderSearch -> 14L
    | MEMORY_IN_BYTES_BEGIN -> 1L
    | MEMORY_IN_BYTES_END -> 14L
    | First -> 1L
    | Last -> 14L

  let of_int64 = function
    | 1L -> AST
    | 2L -> Identifiers
    | 3L -> Selectors
    | 4L -> GlobalCompletionResults
    | 5L -> SourceManagerContentCache
    | 6L -> AST_SideTables
    | 7L -> SourceManager_Membuffer_Malloc
    | 8L -> SourceManager_Membuffer_MMap
    | 9L -> ExternalASTSource_Membuffer_Malloc
    | 10L -> ExternalASTSource_Membuffer_MMap
    | 11L -> Preprocessor
    | 12L -> PreprocessingRecord
    | 13L -> SourceManager_DataStructures
    | 14L -> Preprocessor_HeaderSearch
    (*| 1L -> MEMORY_IN_BYTES_BEGIN
    | 14L -> MEMORY_IN_BYTES_END
    | 1L -> First
    | 14L -> Last*)
    | _ -> failwith "CXTUResourceUsageKind.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXCursorKind = struct

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

  let to_int64 = function
    | UnexposedDecl -> 1L
    | StructDecl -> 2L
    | UnionDecl -> 3L
    | ClassDecl -> 4L
    | EnumDecl -> 5L
    | FieldDecl -> 6L
    | EnumConstantDecl -> 7L
    | FunctionDecl -> 8L
    | VarDecl -> 9L
    | ParmDecl -> 10L
    | ObjCInterfaceDecl -> 11L
    | ObjCCategoryDecl -> 12L
    | ObjCProtocolDecl -> 13L
    | ObjCPropertyDecl -> 14L
    | ObjCIvarDecl -> 15L
    | ObjCInstanceMethodDecl -> 16L
    | ObjCClassMethodDecl -> 17L
    | ObjCImplementationDecl -> 18L
    | ObjCCategoryImplDecl -> 19L
    | TypedefDecl -> 20L
    | CXXMethod -> 21L
    | Namespace -> 22L
    | LinkageSpec -> 23L
    | Constructor -> 24L
    | Destructor -> 25L
    | ConversionFunction -> 26L
    | TemplateTypeParameter -> 27L
    | NonTypeTemplateParameter -> 28L
    | TemplateTemplateParameter -> 29L
    | FunctionTemplate -> 30L
    | ClassTemplate -> 31L
    | ClassTemplatePartialSpecialization -> 32L
    | NamespaceAlias -> 33L
    | UsingDirective -> 34L
    | UsingDeclaration -> 35L
    | TypeAliasDecl -> 36L
    | ObjCSynthesizeDecl -> 37L
    | ObjCDynamicDecl -> 38L
    | CXXAccessSpecifier -> 39L
    | FirstDecl -> 1L
    | LastDecl -> 39L
    | FirstRef -> 40L
    | ObjCSuperClassRef -> 40L
    | ObjCProtocolRef -> 41L
    | ObjCClassRef -> 42L
    | TypeRef -> 43L
    | CXXBaseSpecifier -> 44L
    | TemplateRef -> 45L
    | NamespaceRef -> 46L
    | MemberRef -> 47L
    | LabelRef -> 48L
    | OverloadedDeclRef -> 49L
    | VariableRef -> 50L
    | LastRef -> 50L
    | FirstInvalid -> 70L
    | InvalidFile -> 70L
    | NoDeclFound -> 71L
    | NotImplemented -> 72L
    | InvalidCode -> 73L
    | LastInvalid -> 73L
    | FirstExpr -> 100L
    | UnexposedExpr -> 100L
    | DeclRefExpr -> 101L
    | MemberRefExpr -> 102L
    | CallExpr -> 103L
    | ObjCMessageExpr -> 104L
    | BlockExpr -> 105L
    | IntegerLiteral -> 106L
    | FloatingLiteral -> 107L
    | ImaginaryLiteral -> 108L
    | StringLiteral -> 109L
    | CharacterLiteral -> 110L
    | ParenExpr -> 111L
    | UnaryOperator -> 112L
    | ArraySubscriptExpr -> 113L
    | BinaryOperator -> 114L
    | CompoundAssignOperator -> 115L
    | ConditionalOperator -> 116L
    | CStyleCastExpr -> 117L
    | CompoundLiteralExpr -> 118L
    | InitListExpr -> 119L
    | AddrLabelExpr -> 120L
    | StmtExpr -> 121L
    | GenericSelectionExpr -> 122L
    | GNUNullExpr -> 123L
    | CXXStaticCastExpr -> 124L
    | CXXDynamicCastExpr -> 125L
    | CXXReinterpretCastExpr -> 126L
    | CXXConstCastExpr -> 127L
    | CXXFunctionalCastExpr -> 128L
    | CXXTypeidExpr -> 129L
    | CXXBoolLiteralExpr -> 130L
    | CXXNullPtrLiteralExpr -> 131L
    | CXXThisExpr -> 132L
    | CXXThrowExpr -> 133L
    | CXXNewExpr -> 134L
    | CXXDeleteExpr -> 135L
    | UnaryExpr -> 136L
    | ObjCStringLiteral -> 137L
    | ObjCEncodeExpr -> 138L
    | ObjCSelectorExpr -> 139L
    | ObjCProtocolExpr -> 140L
    | ObjCBridgedCastExpr -> 141L
    | PackExpansionExpr -> 142L
    | SizeOfPackExpr -> 143L
    | LambdaExpr -> 144L
    | ObjCBoolLiteralExpr -> 145L
    | ObjCSelfExpr -> 146L
    | OMPArraySectionExpr -> 147L
    | LastExpr -> 147L
    | FirstStmt -> 200L
    | UnexposedStmt -> 200L
    | LabelStmt -> 201L
    | CompoundStmt -> 202L
    | CaseStmt -> 203L
    | DefaultStmt -> 204L
    | IfStmt -> 205L
    | SwitchStmt -> 206L
    | WhileStmt -> 207L
    | DoStmt -> 208L
    | ForStmt -> 209L
    | GotoStmt -> 210L
    | IndirectGotoStmt -> 211L
    | ContinueStmt -> 212L
    | BreakStmt -> 213L
    | ReturnStmt -> 214L
    | GCCAsmStmt -> 215L
    | AsmStmt -> 215L
    | ObjCAtTryStmt -> 216L
    | ObjCAtCatchStmt -> 217L
    | ObjCAtFinallyStmt -> 218L
    | ObjCAtThrowStmt -> 219L
    | ObjCAtSynchronizedStmt -> 220L
    | ObjCAutoreleasePoolStmt -> 221L
    | ObjCForCollectionStmt -> 222L
    | CXXCatchStmt -> 223L
    | CXXTryStmt -> 224L
    | CXXForRangeStmt -> 225L
    | SEHTryStmt -> 226L
    | SEHExceptStmt -> 227L
    | SEHFinallyStmt -> 228L
    | MSAsmStmt -> 229L
    | NullStmt -> 230L
    | DeclStmt -> 231L
    | OMPParallelDirective -> 232L
    | OMPSimdDirective -> 233L
    | OMPForDirective -> 234L
    | OMPSectionsDirective -> 235L
    | OMPSectionDirective -> 236L
    | OMPSingleDirective -> 237L
    | OMPParallelForDirective -> 238L
    | OMPParallelSectionsDirective -> 239L
    | OMPTaskDirective -> 240L
    | OMPMasterDirective -> 241L
    | OMPCriticalDirective -> 242L
    | OMPTaskyieldDirective -> 243L
    | OMPBarrierDirective -> 244L
    | OMPTaskwaitDirective -> 245L
    | OMPFlushDirective -> 246L
    | SEHLeaveStmt -> 247L
    | OMPOrderedDirective -> 248L
    | OMPAtomicDirective -> 249L
    | OMPForSimdDirective -> 250L
    | OMPParallelForSimdDirective -> 251L
    | OMPTargetDirective -> 252L
    | OMPTeamsDirective -> 253L
    | OMPTaskgroupDirective -> 254L
    | OMPCancellationPointDirective -> 255L
    | OMPCancelDirective -> 256L
    | OMPTargetDataDirective -> 257L
    | OMPTaskLoopDirective -> 258L
    | OMPTaskLoopSimdDirective -> 259L
    | OMPDistributeDirective -> 260L
    | LastStmt -> 260L
    | TranslationUnit -> 300L
    | FirstAttr -> 400L
    | UnexposedAttr -> 400L
    | IBActionAttr -> 401L
    | IBOutletAttr -> 402L
    | IBOutletCollectionAttr -> 403L
    | CXXFinalAttr -> 404L
    | CXXOverrideAttr -> 405L
    | AnnotateAttr -> 406L
    | AsmLabelAttr -> 407L
    | PackedAttr -> 408L
    | PureAttr -> 409L
    | ConstAttr -> 410L
    | NoDuplicateAttr -> 411L
    | CUDAConstantAttr -> 412L
    | CUDADeviceAttr -> 413L
    | CUDAGlobalAttr -> 414L
    | CUDAHostAttr -> 415L
    | CUDASharedAttr -> 416L
    | VisibilityAttr -> 417L
    | DLLExport -> 418L
    | DLLImport -> 419L
    | LastAttr -> 419L
    | PreprocessingDirective -> 500L
    | MacroDefinition -> 501L
    | MacroExpansion -> 502L
    | MacroInstantiation -> 502L
    | InclusionDirective -> 503L
    | FirstPreprocessing -> 500L
    | LastPreprocessing -> 503L
    | ModuleImportDecl -> 600L
    | TypeAliasTemplateDecl -> 601L
    | FirstExtraDecl -> 600L
    | LastExtraDecl -> 601L
    | OverloadCandidate -> 700L

  let of_int64 = function
    | 1L -> UnexposedDecl
    | 2L -> StructDecl
    | 3L -> UnionDecl
    | 4L -> ClassDecl
    | 5L -> EnumDecl
    | 6L -> FieldDecl
    | 7L -> EnumConstantDecl
    | 8L -> FunctionDecl
    | 9L -> VarDecl
    | 10L -> ParmDecl
    | 11L -> ObjCInterfaceDecl
    | 12L -> ObjCCategoryDecl
    | 13L -> ObjCProtocolDecl
    | 14L -> ObjCPropertyDecl
    | 15L -> ObjCIvarDecl
    | 16L -> ObjCInstanceMethodDecl
    | 17L -> ObjCClassMethodDecl
    | 18L -> ObjCImplementationDecl
    | 19L -> ObjCCategoryImplDecl
    | 20L -> TypedefDecl
    | 21L -> CXXMethod
    | 22L -> Namespace
    | 23L -> LinkageSpec
    | 24L -> Constructor
    | 25L -> Destructor
    | 26L -> ConversionFunction
    | 27L -> TemplateTypeParameter
    | 28L -> NonTypeTemplateParameter
    | 29L -> TemplateTemplateParameter
    | 30L -> FunctionTemplate
    | 31L -> ClassTemplate
    | 32L -> ClassTemplatePartialSpecialization
    | 33L -> NamespaceAlias
    | 34L -> UsingDirective
    | 35L -> UsingDeclaration
    | 36L -> TypeAliasDecl
    | 37L -> ObjCSynthesizeDecl
    | 38L -> ObjCDynamicDecl
    | 39L -> CXXAccessSpecifier
    (*| 1L -> FirstDecl
    | 39L -> LastDecl
    | 40L -> FirstRef*)
    | 40L -> ObjCSuperClassRef
    | 41L -> ObjCProtocolRef
    | 42L -> ObjCClassRef
    | 43L -> TypeRef
    | 44L -> CXXBaseSpecifier
    | 45L -> TemplateRef
    | 46L -> NamespaceRef
    | 47L -> MemberRef
    | 48L -> LabelRef
    | 49L -> OverloadedDeclRef
    | 50L -> VariableRef
    (*| 50L -> LastRef
    | 70L -> FirstInvalid*)
    | 70L -> InvalidFile
    | 71L -> NoDeclFound
    | 72L -> NotImplemented
    | 73L -> InvalidCode
    (*| 73L -> LastInvalid
    | 100L -> FirstExpr*)
    | 100L -> UnexposedExpr
    | 101L -> DeclRefExpr
    | 102L -> MemberRefExpr
    | 103L -> CallExpr
    | 104L -> ObjCMessageExpr
    | 105L -> BlockExpr
    | 106L -> IntegerLiteral
    | 107L -> FloatingLiteral
    | 108L -> ImaginaryLiteral
    | 109L -> StringLiteral
    | 110L -> CharacterLiteral
    | 111L -> ParenExpr
    | 112L -> UnaryOperator
    | 113L -> ArraySubscriptExpr
    | 114L -> BinaryOperator
    | 115L -> CompoundAssignOperator
    | 116L -> ConditionalOperator
    | 117L -> CStyleCastExpr
    | 118L -> CompoundLiteralExpr
    | 119L -> InitListExpr
    | 120L -> AddrLabelExpr
    | 121L -> StmtExpr
    | 122L -> GenericSelectionExpr
    | 123L -> GNUNullExpr
    | 124L -> CXXStaticCastExpr
    | 125L -> CXXDynamicCastExpr
    | 126L -> CXXReinterpretCastExpr
    | 127L -> CXXConstCastExpr
    | 128L -> CXXFunctionalCastExpr
    | 129L -> CXXTypeidExpr
    | 130L -> CXXBoolLiteralExpr
    | 131L -> CXXNullPtrLiteralExpr
    | 132L -> CXXThisExpr
    | 133L -> CXXThrowExpr
    | 134L -> CXXNewExpr
    | 135L -> CXXDeleteExpr
    | 136L -> UnaryExpr
    | 137L -> ObjCStringLiteral
    | 138L -> ObjCEncodeExpr
    | 139L -> ObjCSelectorExpr
    | 140L -> ObjCProtocolExpr
    | 141L -> ObjCBridgedCastExpr
    | 142L -> PackExpansionExpr
    | 143L -> SizeOfPackExpr
    | 144L -> LambdaExpr
    | 145L -> ObjCBoolLiteralExpr
    | 146L -> ObjCSelfExpr
    | 147L -> OMPArraySectionExpr
    (*| 147L -> LastExpr
    | 200L -> FirstStmt*)
    | 200L -> UnexposedStmt
    | 201L -> LabelStmt
    | 202L -> CompoundStmt
    | 203L -> CaseStmt
    | 204L -> DefaultStmt
    | 205L -> IfStmt
    | 206L -> SwitchStmt
    | 207L -> WhileStmt
    | 208L -> DoStmt
    | 209L -> ForStmt
    | 210L -> GotoStmt
    | 211L -> IndirectGotoStmt
    | 212L -> ContinueStmt
    | 213L -> BreakStmt
    | 214L -> ReturnStmt
    (*| 215L -> GCCAsmStmt*)
    | 215L -> AsmStmt
    | 216L -> ObjCAtTryStmt
    | 217L -> ObjCAtCatchStmt
    | 218L -> ObjCAtFinallyStmt
    | 219L -> ObjCAtThrowStmt
    | 220L -> ObjCAtSynchronizedStmt
    | 221L -> ObjCAutoreleasePoolStmt
    | 222L -> ObjCForCollectionStmt
    | 223L -> CXXCatchStmt
    | 224L -> CXXTryStmt
    | 225L -> CXXForRangeStmt
    | 226L -> SEHTryStmt
    | 227L -> SEHExceptStmt
    | 228L -> SEHFinallyStmt
    | 229L -> MSAsmStmt
    | 230L -> NullStmt
    | 231L -> DeclStmt
    | 232L -> OMPParallelDirective
    | 233L -> OMPSimdDirective
    | 234L -> OMPForDirective
    | 235L -> OMPSectionsDirective
    | 236L -> OMPSectionDirective
    | 237L -> OMPSingleDirective
    | 238L -> OMPParallelForDirective
    | 239L -> OMPParallelSectionsDirective
    | 240L -> OMPTaskDirective
    | 241L -> OMPMasterDirective
    | 242L -> OMPCriticalDirective
    | 243L -> OMPTaskyieldDirective
    | 244L -> OMPBarrierDirective
    | 245L -> OMPTaskwaitDirective
    | 246L -> OMPFlushDirective
    | 247L -> SEHLeaveStmt
    | 248L -> OMPOrderedDirective
    | 249L -> OMPAtomicDirective
    | 250L -> OMPForSimdDirective
    | 251L -> OMPParallelForSimdDirective
    | 252L -> OMPTargetDirective
    | 253L -> OMPTeamsDirective
    | 254L -> OMPTaskgroupDirective
    | 255L -> OMPCancellationPointDirective
    | 256L -> OMPCancelDirective
    | 257L -> OMPTargetDataDirective
    | 258L -> OMPTaskLoopDirective
    | 259L -> OMPTaskLoopSimdDirective
    | 260L -> OMPDistributeDirective
    (*| 260L -> LastStmt*)
    | 300L -> TranslationUnit
    (*| 400L -> FirstAttr*)
    | 400L -> UnexposedAttr
    | 401L -> IBActionAttr
    | 402L -> IBOutletAttr
    | 403L -> IBOutletCollectionAttr
    | 404L -> CXXFinalAttr
    | 405L -> CXXOverrideAttr
    | 406L -> AnnotateAttr
    | 407L -> AsmLabelAttr
    | 408L -> PackedAttr
    | 409L -> PureAttr
    | 410L -> ConstAttr
    | 411L -> NoDuplicateAttr
    | 412L -> CUDAConstantAttr
    | 413L -> CUDADeviceAttr
    | 414L -> CUDAGlobalAttr
    | 415L -> CUDAHostAttr
    | 416L -> CUDASharedAttr
    | 417L -> VisibilityAttr
    | 418L -> DLLExport
    | 419L -> DLLImport
    (*| 419L -> LastAttr*)
    | 500L -> PreprocessingDirective
    | 501L -> MacroDefinition
    | 502L -> MacroExpansion
    (*| 502L -> MacroInstantiation*)
    | 503L -> InclusionDirective
    (*| 500L -> FirstPreprocessing
    | 503L -> LastPreprocessing*)
    | 600L -> ModuleImportDecl
    | 601L -> TypeAliasTemplateDecl
    (*| 600L -> FirstExtraDecl
    | 601L -> LastExtraDecl*)
    | 700L -> OverloadCandidate
    | _ -> failwith "CXCursorKind.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXLinkageKind = struct

  type t = 
    | Invalid
    | NoLinkage
    | Internal
    | UniqueExternal
    | External

  let to_int64 = function
    | Invalid -> 0L
    | NoLinkage -> 1L
    | Internal -> 2L
    | UniqueExternal -> 3L
    | External -> 4L

  let of_int64 = function
    | 0L -> Invalid
    | 1L -> NoLinkage
    | 2L -> Internal
    | 3L -> UniqueExternal
    | 4L -> External
    | _ -> failwith "CXLinkageKind.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXVisibilityKind = struct

  type t = 
    | Invalid
    | Hidden
    | Protected
    | Default

  let to_int64 = function
    | Invalid -> 0L
    | Hidden -> 1L
    | Protected -> 2L
    | Default -> 3L

  let of_int64 = function
    | 0L -> Invalid
    | 1L -> Hidden
    | 2L -> Protected
    | 3L -> Default
    | _ -> failwith "CXVisibilityKind.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXLanguageKind = struct

  type t = 
    | Invalid
    | C
    | ObjC
    | CPlusPlus

  let to_int64 = function
    | Invalid -> 0L
    | C -> 1L
    | ObjC -> 2L
    | CPlusPlus -> 3L

  let of_int64 = function
    | 0L -> Invalid
    | 1L -> C
    | 2L -> ObjC
    | 3L -> CPlusPlus
    | _ -> failwith "CXLanguageKind.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXTypeKind = struct

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

  let to_int64 = function
    | Invalid -> 0L
    | Unexposed -> 1L
    | Void -> 2L
    | Bool -> 3L
    | Char_U -> 4L
    | UChar -> 5L
    | Char16 -> 6L
    | Char32 -> 7L
    | UShort -> 8L
    | UInt -> 9L
    | ULong -> 10L
    | ULongLong -> 11L
    | UInt128 -> 12L
    | Char_S -> 13L
    | SChar -> 14L
    | WChar -> 15L
    | Short -> 16L
    | Int -> 17L
    | Long -> 18L
    | LongLong -> 19L
    | Int128 -> 20L
    | Float -> 21L
    | Double -> 22L
    | LongDouble -> 23L
    | NullPtr -> 24L
    | Overload -> 25L
    | Dependent -> 26L
    | ObjCId -> 27L
    | ObjCClass -> 28L
    | ObjCSel -> 29L
    | FirstBuiltin -> 2L
    | LastBuiltin -> 29L
    | Complex -> 100L
    | Pointer -> 101L
    | BlockPointer -> 102L
    | LValueReference -> 103L
    | RValueReference -> 104L
    | Record -> 105L
    | Enum -> 106L
    | Typedef -> 107L
    | ObjCInterface -> 108L
    | ObjCObjectPointer -> 109L
    | FunctionNoProto -> 110L
    | FunctionProto -> 111L
    | ConstantArray -> 112L
    | Vector -> 113L
    | IncompleteArray -> 114L
    | VariableArray -> 115L
    | DependentSizedArray -> 116L
    | MemberPointer -> 117L
    | Auto -> 118L

  let of_int64 = function
    | 0L -> Invalid
    | 1L -> Unexposed
    | 2L -> Void
    | 3L -> Bool
    | 4L -> Char_U
    | 5L -> UChar
    | 6L -> Char16
    | 7L -> Char32
    | 8L -> UShort
    | 9L -> UInt
    | 10L -> ULong
    | 11L -> ULongLong
    | 12L -> UInt128
    | 13L -> Char_S
    | 14L -> SChar
    | 15L -> WChar
    | 16L -> Short
    | 17L -> Int
    | 18L -> Long
    | 19L -> LongLong
    | 20L -> Int128
    | 21L -> Float
    | 22L -> Double
    | 23L -> LongDouble
    | 24L -> NullPtr
    | 25L -> Overload
    | 26L -> Dependent
    | 27L -> ObjCId
    | 28L -> ObjCClass
    | 29L -> ObjCSel
    (*| 2L -> FirstBuiltin
    | 29L -> LastBuiltin*)
    | 100L -> Complex
    | 101L -> Pointer
    | 102L -> BlockPointer
    | 103L -> LValueReference
    | 104L -> RValueReference
    | 105L -> Record
    | 106L -> Enum
    | 107L -> Typedef
    | 108L -> ObjCInterface
    | 109L -> ObjCObjectPointer
    | 110L -> FunctionNoProto
    | 111L -> FunctionProto
    | 112L -> ConstantArray
    | 113L -> Vector
    | 114L -> IncompleteArray
    | 115L -> VariableArray
    | 116L -> DependentSizedArray
    | 117L -> MemberPointer
    | 118L -> Auto
    | _ -> failwith "CXTypeKind.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXCallingConv = struct

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

  let to_int64 = function
    | Default -> 0L
    | C -> 1L
    | X86StdCall -> 2L
    | X86FastCall -> 3L
    | X86ThisCall -> 4L
    | X86Pascal -> 5L
    | AAPCS -> 6L
    | AAPCS_VFP -> 7L
    | IntelOclBicc -> 9L
    | X86_64Win64 -> 10L
    | X86_64SysV -> 11L
    | X86VectorCall -> 12L
    | Invalid -> 100L
    | Unexposed -> 200L

  let of_int64 = function
    | 0L -> Default
    | 1L -> C
    | 2L -> X86StdCall
    | 3L -> X86FastCall
    | 4L -> X86ThisCall
    | 5L -> X86Pascal
    | 6L -> AAPCS
    | 7L -> AAPCS_VFP
    | 9L -> IntelOclBicc
    | 10L -> X86_64Win64
    | 11L -> X86_64SysV
    | 12L -> X86VectorCall
    | 100L -> Invalid
    | 200L -> Unexposed
    | _ -> failwith "CXCallingConv.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXTemplateArgumentKind = struct

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

  let to_int64 = function
    | Null -> 0L
    | Type -> 1L
    | Declaration -> 2L
    | NullPtr -> 3L
    | Integral -> 4L
    | Template -> 5L
    | TemplateExpansion -> 6L
    | Expression -> 7L
    | Pack -> 8L
    | Invalid -> 9L

  let of_int64 = function
    | 0L -> Null
    | 1L -> Type
    | 2L -> Declaration
    | 3L -> NullPtr
    | 4L -> Integral
    | 5L -> Template
    | 6L -> TemplateExpansion
    | 7L -> Expression
    | 8L -> Pack
    | 9L -> Invalid
    | _ -> failwith "CXTemplateArgumentKind.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXTypeLayoutError = struct

  type t = 
    | Invalid
    | Incomplete
    | Dependent
    | NotConstantSize
    | InvalidFieldName

  let to_int64 = function
    | Invalid -> -1L
    | Incomplete -> -2L
    | Dependent -> -3L
    | NotConstantSize -> -4L
    | InvalidFieldName -> -5L

  let of_int64 = function
    | -1L -> Invalid
    | -2L -> Incomplete
    | -3L -> Dependent
    | -4L -> NotConstantSize
    | -5L -> InvalidFieldName
    | _ -> failwith "CXTypeLayoutError.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CX_CXXAccessSpecifier = struct

  type t = 
    | InvalidAccessSpecifier
    | Public
    | Protected
    | Private

  let to_int64 = function
    | InvalidAccessSpecifier -> 0L
    | Public -> 1L
    | Protected -> 2L
    | Private -> 3L

  let of_int64 = function
    | 0L -> InvalidAccessSpecifier
    | 1L -> Public
    | 2L -> Protected
    | 3L -> Private
    | _ -> failwith "CX_CXXAccessSpecifier.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CX_StorageClass = struct

  type t = 
    | C_Invalid
    | C_None
    | C_Extern
    | C_Static
    | C_PrivateExtern
    | C_OpenCLWorkGroupLocal
    | C_Auto
    | C_Register

  let to_int64 = function
    | C_Invalid -> 0L
    | C_None -> 1L
    | C_Extern -> 2L
    | C_Static -> 3L
    | C_PrivateExtern -> 4L
    | C_OpenCLWorkGroupLocal -> 5L
    | C_Auto -> 6L
    | C_Register -> 7L

  let of_int64 = function
    | 0L -> C_Invalid
    | 1L -> C_None
    | 2L -> C_Extern
    | 3L -> C_Static
    | 4L -> C_PrivateExtern
    | 5L -> C_OpenCLWorkGroupLocal
    | 6L -> C_Auto
    | 7L -> C_Register
    | _ -> failwith "CX_StorageClass.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXChildVisitResult = struct

  type t = 
    | Break
    | Continue
    | Recurse

  let to_int64 = function
    | Break -> 0L
    | Continue -> 1L
    | Recurse -> 2L

  let of_int64 = function
    | 0L -> Break
    | 1L -> Continue
    | 2L -> Recurse
    | _ -> failwith "CXChildVisitResult.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXNameRefFlags = struct

  type t = 
    | CXNameRange_WantQualifier
    | CXNameRange_WantTemplateArgs
    | CXNameRange_WantSinglePiece

  let to_int64 = function
    | CXNameRange_WantQualifier -> 1L
    | CXNameRange_WantTemplateArgs -> 2L
    | CXNameRange_WantSinglePiece -> 4L

  let of_int64 = function
    | 1L -> CXNameRange_WantQualifier
    | 2L -> CXNameRange_WantTemplateArgs
    | 4L -> CXNameRange_WantSinglePiece
    | _ -> failwith "CXNameRefFlags.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXTokenKind = struct

  type t = 
    | Punctuation
    | Keyword
    | Identifier
    | Literal
    | Comment

  let to_int64 = function
    | Punctuation -> 0L
    | Keyword -> 1L
    | Identifier -> 2L
    | Literal -> 3L
    | Comment -> 4L

  let of_int64 = function
    | 0L -> Punctuation
    | 1L -> Keyword
    | 2L -> Identifier
    | 3L -> Literal
    | 4L -> Comment
    | _ -> failwith "CXTokenKind.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXCompletionChunkKind = struct

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

  let to_int64 = function
    | Optional -> 0L
    | TypedText -> 1L
    | Text -> 2L
    | Placeholder -> 3L
    | Informative -> 4L
    | CurrentParameter -> 5L
    | LeftParen -> 6L
    | RightParen -> 7L
    | LeftBracket -> 8L
    | RightBracket -> 9L
    | LeftBrace -> 10L
    | RightBrace -> 11L
    | LeftAngle -> 12L
    | RightAngle -> 13L
    | Comma -> 14L
    | ResultType -> 15L
    | Colon -> 16L
    | SemiColon -> 17L
    | Equal -> 18L
    | HorizontalSpace -> 19L
    | VerticalSpace -> 20L

  let of_int64 = function
    | 0L -> Optional
    | 1L -> TypedText
    | 2L -> Text
    | 3L -> Placeholder
    | 4L -> Informative
    | 5L -> CurrentParameter
    | 6L -> LeftParen
    | 7L -> RightParen
    | 8L -> LeftBracket
    | 9L -> RightBracket
    | 10L -> LeftBrace
    | 11L -> RightBrace
    | 12L -> LeftAngle
    | 13L -> RightAngle
    | 14L -> Comma
    | 15L -> ResultType
    | 16L -> Colon
    | 17L -> SemiColon
    | 18L -> Equal
    | 19L -> HorizontalSpace
    | 20L -> VerticalSpace
    | _ -> failwith "CXCompletionChunkKind.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXCodeComplete_Flags = struct

  type t = 
    | IncludeMacros
    | IncludeCodePatterns
    | IncludeBriefComments

  let to_int64 = function
    | IncludeMacros -> 1L
    | IncludeCodePatterns -> 2L
    | IncludeBriefComments -> 4L

  let of_int64 = function
    | 1L -> IncludeMacros
    | 2L -> IncludeCodePatterns
    | 4L -> IncludeBriefComments
    | _ -> failwith "CXCodeComplete_Flags.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXCompletionContext = struct

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

  let to_int64 = function
    | Unexposed -> 0L
    | AnyType -> 1L
    | AnyValue -> 2L
    | ObjCObjectValue -> 4L
    | ObjCSelectorValue -> 8L
    | CXXClassTypeValue -> 16L
    | DotMemberAccess -> 32L
    | ArrowMemberAccess -> 64L
    | ObjCPropertyAccess -> 128L
    | EnumTag -> 256L
    | UnionTag -> 512L
    | StructTag -> 1024L
    | ClassTag -> 2048L
    | Namespace -> 4096L
    | NestedNameSpecifier -> 8192L
    | ObjCInterface -> 16384L
    | ObjCProtocol -> 32768L
    | ObjCCategory -> 65536L
    | ObjCInstanceMessage -> 131072L
    | ObjCClassMessage -> 262144L
    | ObjCSelectorName -> 524288L
    | MacroName -> 1048576L
    | NaturalLanguage -> 2097152L
    | Unknown -> 4194303L

  let of_int64 = function
    | 0L -> Unexposed
    | 1L -> AnyType
    | 2L -> AnyValue
    | 4L -> ObjCObjectValue
    | 8L -> ObjCSelectorValue
    | 16L -> CXXClassTypeValue
    | 32L -> DotMemberAccess
    | 64L -> ArrowMemberAccess
    | 128L -> ObjCPropertyAccess
    | 256L -> EnumTag
    | 512L -> UnionTag
    | 1024L -> StructTag
    | 2048L -> ClassTag
    | 4096L -> Namespace
    | 8192L -> NestedNameSpecifier
    | 16384L -> ObjCInterface
    | 32768L -> ObjCProtocol
    | 65536L -> ObjCCategory
    | 131072L -> ObjCInstanceMessage
    | 262144L -> ObjCClassMessage
    | 524288L -> ObjCSelectorName
    | 1048576L -> MacroName
    | 2097152L -> NaturalLanguage
    | 4194303L -> Unknown
    | _ -> failwith "CXCompletionContext.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

module CXVisitorResult = struct

  type t = 
    | Break
    | Continue

  let to_int64 = function
    | Break -> 0L
    | Continue -> 1L

  let of_int64 = function
    | 0L -> Break
    | 1L -> Continue
    | _ -> failwith "CXVisitorResult.to_int"

  let of_int x = of_int64 (Int64.of_int x)

  let to_int x = Int64.to_int (to_int64 x)

end

