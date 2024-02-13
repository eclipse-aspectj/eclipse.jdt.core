-- AspectJ Extension
-- Alexander Kriegisch, 2021, 2022, 2023
-- Andy Clement, Nov 2007, Nov 2011, Jul 2013
-- previous versions, Adrian Colyer, Jim Hugunin
--main options
%options ACTION, AN=JavaAction.java, GP=java,
-- ASPECTJ: OUTPUT-SIZE=200 (from 125) since Java 21 merge
%options FILE-PREFIX=java, ESCAPE=$, PREFIX=TokenName, OUTPUT-SIZE=200 ,
%options NOGOTO-DEFAULT, SINGLE-PRODUCTIONS, LALR=1 , TABLE,

--error recovering options.....
-- ASPECTJ: INCLUDE THE COMMAS OR JIKESPG CAN GIVE AN ARRAYINDEXOUTOFBOUNDS EXCEPTION
%options ERROR_MAPS,

--grammar understanding options
%options first follow
%options TRACE=FULL ,
%options VERBOSE,

%options DEFERRED,
%options NAMES=MAXIMUM,
%options SCOPES,

--Usefull macros helping reading/writing semantic actions
$Define
$putCase
/.    case $rule_number : if (DEBUG) { System.out.println("$rule_text"); }  //$NON-NLS-1$
		   ./

$break
/.
			break;
./


$readableName
/.1#$rule_number#./
$compliance
/.2#$rule_number#./
$recovery
/.2#$rule_number# recovery./
$recovery_template
/.3#$rule_number#./
$no_statements_recovery
/.4#$rule_number# 1./
-- here it starts really ------------------------------------------
$Terminals

	Identifier

	abstract assert boolean break byte case catch char class
	continue const default do double else enum extends false final finally float
	for goto if implements import instanceof int
	interface long native new non-sealed null package private
	protected public return short static strictfp super switch
	synchronized this throw throws transient true try void
	volatile while module open requires transitive exports opens to uses provides with

-- AspectJ Extension
	aspect pointcut around before after declare privileged
-- End AspectJ Extension

	IntegerLiteral
	LongLiteral
	FloatingPointLiteral
	DoubleLiteral
	CharacterLiteral
	StringLiteral
	TextBlock
	StringTemplate
	TextBlockTemplate

	PLUS_PLUS
	MINUS_MINUS
	EQUAL_EQUAL
	LESS_EQUAL
	GREATER_EQUAL
	NOT_EQUAL
	LEFT_SHIFT
	RIGHT_SHIFT
	UNSIGNED_RIGHT_SHIFT
	PLUS_EQUAL
	MINUS_EQUAL
	MULTIPLY_EQUAL
	DIVIDE_EQUAL
	AND_EQUAL
	OR_EQUAL
	XOR_EQUAL
	REMAINDER_EQUAL
	LEFT_SHIFT_EQUAL
	RIGHT_SHIFT_EQUAL
	UNSIGNED_RIGHT_SHIFT_EQUAL
	OR_OR
	AND_AND
	PLUS
	MINUS
	NOT
	REMAINDER
	XOR
	AND
	MULTIPLY
	OR
	TWIDDLE
	DIVIDE
	GREATER
	LESS
	LPAREN
	RPAREN
	LBRACE
	RBRACE
	LBRACKET
	RBRACKET
	SEMICOLON
	QUESTION
	COLON
	COMMA
	DOT
	EQUAL
	AT
	ELLIPSIS
	ARROW
	COLON_COLON
	BeginLambda
	BeginIntersectionCast
	BeginTypeArguments
	ElidedSemicolonAndRightBrace
	AT308
	AT308DOTDOTDOT
	BeginCaseExpr
	RestrictedIdentifierYield
	RestrictedIdentifierrecord
	RestrictedIdentifiersealed
	RestrictedIdentifierpermits
	BeginCaseElement
	RestrictedIdentifierWhen
	UNDERSCORE

--    BodyMarker

$Alias

	'::'   ::= COLON_COLON
	'->'   ::= ARROW
	'++'   ::= PLUS_PLUS
	'--'   ::= MINUS_MINUS
	'=='   ::= EQUAL_EQUAL
	'<='   ::= LESS_EQUAL
	'>='   ::= GREATER_EQUAL
	'!='   ::= NOT_EQUAL
	'<<'   ::= LEFT_SHIFT
	'>>'   ::= RIGHT_SHIFT
	'>>>'  ::= UNSIGNED_RIGHT_SHIFT
	'+='   ::= PLUS_EQUAL
	'-='   ::= MINUS_EQUAL
	'*='   ::= MULTIPLY_EQUAL
	'/='   ::= DIVIDE_EQUAL
	'&='   ::= AND_EQUAL
	'|='   ::= OR_EQUAL
	'^='   ::= XOR_EQUAL
	'%='   ::= REMAINDER_EQUAL
	'<<='  ::= LEFT_SHIFT_EQUAL
	'>>='  ::= RIGHT_SHIFT_EQUAL
	'>>>=' ::= UNSIGNED_RIGHT_SHIFT_EQUAL
	'||'   ::= OR_OR
	'&&'   ::= AND_AND
	'+'    ::= PLUS
	'-'    ::= MINUS
	'!'    ::= NOT
	'%'    ::= REMAINDER
	'^'    ::= XOR
	'&'    ::= AND
	'*'    ::= MULTIPLY
	'|'    ::= OR
	'~'    ::= TWIDDLE
	'/'    ::= DIVIDE
	'>'    ::= GREATER
	'<'    ::= LESS
	'('    ::= LPAREN
	')'    ::= RPAREN
	'{'    ::= LBRACE
	'}'    ::= RBRACE
	'['    ::= LBRACKET
	']'    ::= RBRACKET
	';'    ::= SEMICOLON
	'?'    ::= QUESTION
	':'    ::= COLON
	','    ::= COMMA
	'.'    ::= DOT
	'='    ::= EQUAL
	'@'	   ::= AT
	'...'  ::= ELLIPSIS
	'@308' ::= AT308
	'@308...' ::= AT308DOTDOTDOT
	'_' ::= UNDERSCORE

$Start
	Goal

$Rules

/.// This method is part of an automatic generation : do NOT edit-modify
protected void consumeRule(int act) {
  switch ( act ) {
./

Goal ::= '++' CompilationUnit
Goal ::= '--' MethodBody
-- Initializer
Goal ::= '>>' StaticInitializer
Goal ::= '>>' Initializer
-- error recovery
-- Modifiersopt is used to properly consume a header and exit the rule reduction at the end of the parse() method
Goal ::= '>>>' Header1 Modifiersopt
Goal ::= '!' Header2 Modifiersopt
Goal ::= '*' BlockStatements
Goal ::= '*' CatchHeader
-- JDOM
Goal ::= '&&' FieldDeclaration
Goal ::= '||' ImportDeclaration
Goal ::= '?' PackageDeclaration
Goal ::= '+' TypeDeclaration
Goal ::= '/' GenericMethodDeclaration
Goal ::= '&' ClassBodyDeclarations
Goal ::= '-' RecordBodyDeclarations
-- code snippet
Goal ::= '%' Expression
Goal ::= '%' ArrayInitializer
-- completion parser
Goal ::= '~' BlockStatementsopt
Goal ::= '{' BlockStatementopt
-- source type converter
Goal ::= '||' MemberValue
-- syntax diagnosis
Goal ::= '?' AnnotationTypeMemberDeclaration
-- JSR 335 Reconnaissance missions.
Goal ::= '->' ParenthesizedLambdaParameterList
Goal ::= '(' ParenthesizedCastNameAndBounds
Goal ::= '<' ReferenceExpressionTypeArgumentsAndTrunk
-- JSR 308 Reconnaissance mission.
Goal ::= '@' TypeAnnotations
-- JSR 354 Reconnaissance mission.
Goal ::= '->' YieldStatement
Goal ::= '->' SwitchLabelCaseLhs
-- JSR 360 Restricted
Goal ::= RestrictedIdentifiersealed Modifiersopt
Goal ::= RestrictedIdentifierpermits PermittedSubclasses
-- jsr 427 --
Goal ::= BeginCaseElement Pattern
Goal ::= RestrictedIdentifierWhen Expression
/:$readableName Goal:/

Literal -> IntegerLiteral
Literal -> LongLiteral
Literal -> FloatingPointLiteral
Literal -> DoubleLiteral
Literal -> CharacterLiteral
Literal -> StringLiteral
Literal -> TextBlock
Literal -> null
Literal -> BooleanLiteral

/:$readableName Literal:/
BooleanLiteral -> true
BooleanLiteral -> false
/:$readableName BooleanLiteral:/

-- AspectJ Extension
-- we do this because AspectJ keywords are 'pseudo' keywords
-- although they are recognised by the scanner as tokens, there
-- are many places where they are still allowed as identifiers
-- for maximum compatibility with existing Java programs.
-- JavaIdentifer replaces Identifier at many points in the productions
-- that follow, and allows any Java identifier or an AspectJ 'pseudo'
-- keyword to appear at those points in the grammar.
JavaIdentifier -> 'Identifier'
JavaIdentifier -> AjSimpleName
/:$readableName identifier:/

JavaIdentifierNoAround -> 'Identifier'
JavaIdentifierNoAround -> AjSimpleNameNoAround
/:$readableName identifier:/
-- End AspectJ Extension

Type ::= PrimitiveType
/.$putCase consumePrimitiveType(); $break ./
Type -> ReferenceType
/:$readableName Type:/

PrimitiveType -> TypeAnnotationsopt NumericType
/:$readableName PrimitiveType:/
NumericType -> IntegralType
NumericType -> FloatingPointType
/:$readableName NumericType:/

PrimitiveType -> TypeAnnotationsopt 'boolean'
PrimitiveType -> TypeAnnotationsopt 'void'
IntegralType -> 'byte'
IntegralType -> 'short'
IntegralType -> 'int'
IntegralType -> 'long'
IntegralType -> 'char'
/:$readableName IntegralType:/
FloatingPointType -> 'float'
FloatingPointType -> 'double'
/:$readableName FloatingPointType:/

ReferenceType ::= ClassOrInterfaceType
/.$putCase consumeReferenceType(); $break ./
ReferenceType -> ArrayType
/:$readableName ReferenceType:/

---------------------------------------------------------------
-- 1.5 feature
---------------------------------------------------------------
ClassOrInterfaceType -> ClassOrInterface
ClassOrInterfaceType -> GenericType
/:$readableName Type:/

ClassOrInterface ::= Name
/.$putCase consumeClassOrInterfaceName(); $break ./
ClassOrInterface ::= GenericType '.' Name
/.$putCase consumeClassOrInterface(); $break ./
/:$readableName Type:/

GenericType ::= ClassOrInterface TypeArguments
/.$putCase consumeGenericType(); $break ./
/:$readableName GenericType:/

GenericType ::= ClassOrInterface '<' '>'
/.$putCase consumeGenericTypeWithDiamond(); $break ./
/:$readableName GenericType:/
/:$compliance 1.7:/

--
-- These rules have been rewritten to avoid some conflicts introduced
-- by adding the 1.1 features
--
-- ArrayType ::= PrimitiveType '[' ']'
-- ArrayType ::= Name '[' ']'
-- ArrayType ::= ArrayType '[' ']'
--

ArrayTypeWithTypeArgumentsName ::= GenericType '.' Name
/.$putCase consumeArrayTypeWithTypeArgumentsName(); $break ./
/:$readableName ArrayTypeWithTypeArgumentsName:/

ArrayType ::= PrimitiveType Dims
/.$putCase consumePrimitiveArrayType(); $break ./
ArrayType ::= Name Dims
/.$putCase consumeNameArrayType(); $break ./
ArrayType ::= ArrayTypeWithTypeArgumentsName Dims
/.$putCase consumeGenericTypeNameArrayType(); $break ./
ArrayType ::= GenericType Dims
/.$putCase consumeGenericTypeArrayType(); $break ./
/:$readableName ArrayType:/

ClassType -> ClassOrInterfaceType
/:$readableName ClassType:/

--------------------------------------------------------------
--------------------------------------------------------------

-- Was this in the old version (37 compiler):
-- START
-- Name -> SimpleName
-- Name -> QualifiedName
-- /:$readableName Name:/
-- /:$recovery_template Identifier:/
--
-- SimpleName -> 'Identifier'
-- /:$readableName SimpleName:/
--
-- QualifiedName ::= Name '.' SimpleName
-- /.$putCase consumeQualifiedName(); $break ./
-- /:$readableName QualifiedName:/
-- END

-- AspectJ Extension
-- see earlier comments for why we do this... its all about
-- treating aspectj keywords as pseudo keywords for maximum
-- compatibility with existing Java applications.
NameOrAj -> AjName
NameOrAj -> Name
/:$readableName name:/

SimpleNameOrAj -> AjSimpleName
SimpleNameOrAj -> SimpleName
/:$readableName name:/

-- the addition of the consumeZero calls here is what has caused the jikespg parser to crash at the end?!?
AjName ::= AjSimpleName
/.$putCase consumeZeroTypeAnnotations(); $break ./
AjName ::= AjQualifiedName
/.$putCase consumeZeroTypeAnnotations(); $break ./
/:$readableName name:/

AjSimpleName -> AjSimpleNameNoAround
/:$readableName identifer:/

AjSimpleNameNoAround -> 'aspect'
AjSimpleNameNoAround -> 'privileged'
AjSimpleNameNoAround -> 'pointcut'
AjSimpleName -> 'around'
AjSimpleNameNoAround -> 'before'
AjSimpleNameNoAround -> 'after'
AjSimpleNameNoAround -> 'declare'
/:$readableName identifier (aspect keywords permitted):/

AjQualifiedName ::= AjName '.' SimpleNameOrAj
/.$putCase consumeQualifiedName(); $break ./
/:$readableName qualified name:/

UnannotatableNameOrAj -> AjName
UnannotatableNameOrAj -> UnannotatableName
/:$readableName name:/

-- End AspectJ Extension


Name ::= SimpleName
/.$putCase consumeZeroTypeAnnotations(); $break ./
Name -> TypeAnnotations SimpleName
/:$compliance 1.8:/
Name -> QualifiedName
/:$readableName Name:/
/:$recovery_template JavaIdentifier:/ -- AspectJ Extension (JavaIdentifier was Identifier)

SimpleName -> 'Identifier'
/:$readableName SimpleName:/

UnannotatableName -> SimpleName
UnannotatableName ::= UnannotatableName '.' SimpleName
/.$putCase consumeUnannotatableQualifiedName(); $break ./
/:$readableName UnannotatableQualifiedName:/

QualifiedName ::= Name '.' JavaIdentifier -- AspectJ Extension (JavaIdentifier was SimpleName)
/.$putCase consumeQualifiedName(false); $break ./
QualifiedName ::= Name '.' TypeAnnotations JavaIdentifier -- AspectJ Extension (JavaIdentifier was SimpleName)
/.$putCase consumeQualifiedName(true); $break ./
/:$compliance 1.8:/
/:$readableName QualifiedName:/

TypeAnnotationsopt ::= $empty
/.$putCase consumeZeroTypeAnnotations(); $break ./
TypeAnnotationsopt -> TypeAnnotations
/:$compliance 1.8:/
/:$readableName TypeAnnotationsopt:/

-- Production name hardcoded in parser. Must be ::= and not ->
TypeAnnotations ::= TypeAnnotations0
/:$readableName TypeAnnotations:/

TypeAnnotations0 -> TypeAnnotation
/:$compliance 1.8:/
TypeAnnotations0 ::= TypeAnnotations0 TypeAnnotation
/. $putCase consumeOneMoreTypeAnnotation(); $break ./
/:$compliance 1.8:/
/:$readableName TypeAnnotations:/

TypeAnnotation ::= NormalTypeAnnotation
/. $putCase consumeTypeAnnotation(); $break ./
/:$compliance 1.8:/
TypeAnnotation ::= MarkerTypeAnnotation
/. $putCase consumeTypeAnnotation(); $break ./
/:$compliance 1.8:/
TypeAnnotation ::= SingleMemberTypeAnnotation
/. $putCase consumeTypeAnnotation(); $break ./
/:$compliance 1.8:/
/:$readableName TypeAnnotation:/

TypeAnnotationName ::= @308 UnannotatableName
/.$putCase consumeAnnotationName() ; $break ./
/:$readableName AnnotationName:/
/:$compliance 1.8:/
/:$recovery_template @ Identifier:/
NormalTypeAnnotation ::= TypeAnnotationName '(' MemberValuePairsopt ')'
/.$putCase consumeNormalAnnotation(true) ; $break ./
/:$readableName NormalAnnotation:/
/:$compliance 1.8:/
MarkerTypeAnnotation ::= TypeAnnotationName
/.$putCase consumeMarkerAnnotation(true) ; $break ./
/:$readableName MarkerAnnotation:/
/:$compliance 1.8:/
SingleMemberTypeAnnotation ::= TypeAnnotationName '(' SingleMemberAnnotationMemberValue ')'
/.$putCase consumeSingleMemberAnnotation(true) ; $break ./
/:$readableName SingleMemberAnnotation:/
/:$compliance 1.8:/

RejectTypeAnnotations ::= $empty
/.$putCase consumeNonTypeUseName(); $break ./
/:$readableName RejectTypeAnnotations:/

PushZeroTypeAnnotations ::= $empty
/.$putCase consumeZeroTypeAnnotations(); $break ./
/:$readableName ZeroTypeAnnotations:/

VariableDeclaratorIdOrThis ::= 'this'
/.$putCase consumeExplicitThisParameter(false); $break ./
/:$compliance 1.8:/
VariableDeclaratorIdOrThis ::= UnannotatableName '.' 'this'
/.$putCase consumeExplicitThisParameter(true); $break ./
/:$compliance 1.8:/
VariableDeclaratorIdOrThis ::= VariableDeclaratorId
/.$putCase consumeVariableDeclaratorIdParameter(); $break ./
/:$readableName VariableDeclaratorId:/

CompilationUnit ::= EnterCompilationUnit InternalCompilationUnit
/.$putCase consumeCompilationUnit(); $break ./
/:$readableName CompilationUnit:/

InternalCompilationUnit ::= PackageDeclaration
/.$putCase consumeInternalCompilationUnit(); $break ./
InternalCompilationUnit ::= PackageDeclaration ImportDeclarations ReduceImports
/.$putCase consumeInternalCompilationUnit(); $break ./
InternalCompilationUnit ::= PackageDeclaration ImportDeclarations ReduceImports TypeDeclarations
/.$putCase consumeInternalCompilationUnitWithTypes(); $break ./
InternalCompilationUnit ::= PackageDeclaration TypeDeclarations
/.$putCase consumeInternalCompilationUnitWithTypes(); $break ./
InternalCompilationUnit ::= ImportDeclarations ReduceImports
/.$putCase consumeInternalCompilationUnit(); $break ./
InternalCompilationUnit ::= TypeDeclarations
/.$putCase consumeInternalCompilationUnitWithTypes(); $break ./
InternalCompilationUnit ::= ImportDeclarations ReduceImports TypeDeclarations
/.$putCase consumeInternalCompilationUnitWithTypes(); $break ./
InternalCompilationUnit ::= $empty
/.$putCase consumeEmptyInternalCompilationUnit(); $break ./
/:$readableName CompilationUnit:/

--Java9 features
InternalCompilationUnit ::= ImportDeclarations ReduceImports ModuleDeclaration
/:$compliance 9:/
/.$putCase consumeInternalCompilationUnitWithModuleDeclaration(); $break ./
InternalCompilationUnit ::= ModuleDeclaration
/:$compliance 9:/
/.$putCase consumeInternalCompilationUnitWithModuleDeclaration(); $break ./
ModuleDeclaration ::= ModuleHeader ModuleBody
/:$compliance 9:/
/.$putCase consumeModuleDeclaration(); $break ./

-- to work around shift/reduce conflicts, we allow Modifiersopt in order to support annotations
-- in a module declaration, and then report errors if any modifiers other than annotations are
-- encountered
ModuleHeader ::= Modifiersopt ModuleModifieropt 'module' UnannotatableName
/:$compliance 9:/
/.$putCase consumeModuleHeader(); $break ./
ModuleModifieropt ::= $empty
ModuleModifieropt ::= ModuleModifier
/:$compliance 9:/
/.$putCase consumeModuleModifiers(); $break ./
ModuleModifier -> 'open'

ModuleBody ::= '{' ModuleStatementsOpt '}'
/:$compliance 9:/
/:$no_statements_recovery:/
ModuleStatementsOpt ::= $empty
/:$compliance 9:/
/.$putCase consumeEmptyModuleStatementsOpt(); $break ./
ModuleStatementsOpt -> ModuleStatements
/:$compliance 9:/
ModuleStatements ::= ModuleStatement
ModuleStatements ::= ModuleStatements ModuleStatement
/:$compliance 9:/
/.$putCase consumeModuleStatements(); $break ./

ModuleStatement ::= RequiresStatement
/:$compliance 9:/
ModuleStatement ::= ExportsStatement
/:$compliance 9:/
ModuleStatement ::= OpensStatement
/:$compliance 9:/
ModuleStatement ::= UsesStatement
/:$compliance 9:/
ModuleStatement ::= ProvidesStatement
/:$compliance 9:/

RequiresStatement ::=  SingleRequiresModuleName ';'
/:$compliance 9:/
/.$putCase consumeRequiresStatement(); $break ./
SingleRequiresModuleName ::= 'requires' RequiresModifiersopt UnannotatableName
/:$compliance 9:/
/.$putCase consumeSingleRequiresModuleName(); $break ./
RequiresModifiersopt ::= RequiresModifiers
/:$compliance 9:/
/.$putCase consumeModifiers(); $break ./
RequiresModifiersopt ::= $empty
/:$compliance 9:/
/.$putCase consumeDefaultModifiers(); $break ./
RequiresModifiers -> RequiresModifier
RequiresModifiers ::= RequiresModifiers RequiresModifier
/:$compliance 9:/
/.$putCase consumeModifiers2(); $break ./
RequiresModifier -> 'transitive'
RequiresModifier -> 'static'
ExportsStatement ::=  ExportsHeader TargetModuleListopt ';'
/:$compliance 9:/
/.$putCase consumeExportsStatement(); $break ./
ExportsHeader ::= 'exports' SinglePkgName
/:$compliance 9:/
/.$putCase consumeExportsHeader(); $break ./
TargetModuleListopt ::= $empty
TargetModuleListopt ::= 'to' TargetModuleNameList
/:$compliance 9:/
/.$putCase consumeTargetModuleList(); $break ./
TargetModuleName ::= UnannotatableName
/:$compliance 9:/
/.$putCase consumeSingleTargetModuleName(); $break ./
TargetModuleNameList -> TargetModuleName
TargetModuleNameList ::= TargetModuleNameList ',' TargetModuleName
/:$compliance 9:/
/.$putCase consumeTargetModuleNameList(); $break ./
SinglePkgName ::= UnannotatableName
/:$compliance 9:/
/.$putCase consumeSinglePkgName(); $break ./
OpensStatement ::=  OpensHeader TargetModuleListopt ';'
/:$compliance 9:/
/.$putCase consumeOpensStatement(); $break ./
OpensHeader ::= 'opens' SinglePkgName
/:$compliance 9:/
/.$putCase consumeOpensHeader(); $break ./
UsesStatement ::=  UsesHeader ';'
/:$compliance 9:/
/.$putCase consumeUsesStatement(); $break ./
UsesHeader ::= 'uses' Name
/.$putCase consumeUsesHeader(); $break ./
ProvidesStatement ::= ProvidesInterface WithClause ';'
/:$compliance 9:/
/.$putCase consumeProvidesStatement(); $break ./
ProvidesInterface ::= 'provides' Name
/:$compliance 9:/
/.$putCase consumeProvidesInterface(); $break ./
ServiceImplName ::= Name
/:$compliance 9:/
/.$putCase consumeSingleServiceImplName(); $break ./
ServiceImplNameList -> ServiceImplName
ServiceImplNameList ::= ServiceImplNameList ',' ServiceImplName
/:$compliance 9:/
/.$putCase consumeServiceImplNameList(); $break ./

WithClause ::= 'with' ServiceImplNameList
/:$compliance 9:/
/.$putCase consumeWithClause(); $break ./

ReduceImports ::= $empty
/.$putCase consumeReduceImports(); $break ./
/:$readableName ReduceImports:/

EnterCompilationUnit ::= $empty
/.$putCase consumeEnterCompilationUnit(); $break ./
/:$readableName EnterCompilationUnit:/

Header -> ImportDeclaration
Header -> PackageDeclaration
Header -> ClassHeader
Header -> InterfaceHeader
Header -> EnumHeader
Header -> RecordHeaderPart
Header -> AnnotationTypeDeclarationHeader
Header -> StaticInitializer
Header -> RecoveryMethodHeader
Header -> FieldDeclaration
Header -> AllocationHeader
Header -> ArrayCreationHeader
Header -> ModuleHeader
Header -> RequiresStatement
Header -> ExportsStatement
Header -> UsesStatement
Header -> ProvidesStatement
Header -> OpensStatement
/:$readableName Header:/

Header1 -> Header
Header1 -> ConstructorHeader
/:$readableName Header1:/

Header2 -> Header
Header2 -> EnumConstantHeader
/:$readableName Header2:/

CatchHeader ::= 'catch' '(' CatchFormalParameter ')' '{'
/.$putCase consumeCatchHeader(); $break ./
/:$readableName CatchHeader:/

ImportDeclarations -> ImportDeclaration
ImportDeclarations ::= ImportDeclarations ImportDeclaration
/.$putCase consumeImportDeclarations(); $break ./
/:$readableName ImportDeclarations:/

TypeDeclarations -> TypeDeclaration
TypeDeclarations ::= TypeDeclarations TypeDeclaration
/.$putCase consumeTypeDeclarations(); $break ./
/:$readableName TypeDeclarations:/

PackageDeclaration ::= PackageDeclarationName ';'
/.$putCase consumePackageDeclaration(); $break ./
/:$readableName PackageDeclaration:/

PackageDeclarationName ::= Modifiers 'package' PushRealModifiers Name RejectTypeAnnotations
/.$putCase consumePackageDeclarationNameWithModifiers(); $break ./
/:$readableName PackageDeclarationName:/
/:$compliance 1.5:/

PackageDeclarationName ::= PackageComment 'package' Name RejectTypeAnnotations
/.$putCase consumePackageDeclarationName(); $break ./
/:$readableName PackageDeclarationName:/

PackageComment ::= $empty
/.$putCase consumePackageComment(); $break ./
/:$readableName PackageComment:/

ImportDeclaration -> SingleTypeImportDeclaration
ImportDeclaration -> TypeImportOnDemandDeclaration
-----------------------------------------------
-- 1.5 feature
-----------------------------------------------
ImportDeclaration -> SingleStaticImportDeclaration
ImportDeclaration -> StaticImportOnDemandDeclaration
/:$readableName ImportDeclaration:/

SingleTypeImportDeclaration ::= SingleTypeImportDeclarationName ';'
/.$putCase consumeImportDeclaration(); $break ./
/:$readableName SingleTypeImportDeclaration:/

SingleTypeImportDeclarationName ::= 'import' Name RejectTypeAnnotations
/.$putCase consumeSingleTypeImportDeclarationName(); $break ./
/:$readableName SingleTypeImportDeclarationName:/

TypeImportOnDemandDeclaration ::= TypeImportOnDemandDeclarationName ';'
/.$putCase consumeImportDeclaration(); $break ./
/:$readableName TypeImportOnDemandDeclaration:/

TypeImportOnDemandDeclarationName ::= 'import' Name '.' RejectTypeAnnotations '*'
/.$putCase consumeTypeImportOnDemandDeclarationName(); $break ./
/:$readableName TypeImportOnDemandDeclarationName:/

TypeDeclaration -> ClassDeclaration
TypeDeclaration -> InterfaceDeclaration
-- this declaration in part of a list od declaration and we will
-- use and optimized list length calculation process
-- thus we decrement the number while it will be incremend.....
TypeDeclaration ::= ';'
/. $putCase consumeEmptyTypeDeclaration(); $break ./
-----------------------------------------------
-- 1.5 feature
-----------------------------------------------
TypeDeclaration -> EnumDeclaration
TypeDeclaration -> AnnotationTypeDeclaration
-- Java 14 feature
TypeDeclaration -> RecordDeclaration
/:$readableName TypeDeclaration:/

--18.7 Only in the LALR(1) Grammar

Modifiers -> Modifier
Modifiers ::= Modifiers Modifier
/.$putCase consumeModifiers2(); $break ./
/:$readableName Modifiers:/

-- AspectJ Extension, introduced SimpleModifier to avoid shift/reduce conflict with PseudoTokens
SimpleModifier -> 'public'
SimpleModifier -> 'protected'
SimpleModifier -> 'private'
SimpleModifier -> 'static'
SimpleModifier -> 'abstract'
SimpleModifier -> 'final'
SimpleModifier -> 'native'
SimpleModifier -> 'non-sealed'
SimpleModifier -> RestrictedIdentifiersealed
SimpleModifier -> 'synchronized'
SimpleModifier -> 'transient'
SimpleModifier -> 'volatile'
SimpleModifier -> 'strictfp'
/:$readableName Modifiers:/
-- End AspectJ Extension, for LALR(1)
Modifier -> SimpleModifier
Modifier ::= Annotation
/.$putCase consumeAnnotationAsModifier(); $break ./
/:$readableName Modifier:/

-- AspectJ Extensions - main block
Header -> DeclareDeclaration
Header -> InterTypeMethodDeclaration
Header -> InterTypeFieldDeclaration
Header -> PointcutDeclaration
Header -> BasicAdviceDeclaration
Header -> AroundDeclaration

TypeDeclaration -> AspectDeclaration
Header -> AspectDeclaration

ClassMemberDeclaration -> AspectDeclaration
InterfaceMemberDeclaration -> AspectDeclaration


ClassMemberDeclaration -> PointcutDeclaration
InterfaceMemberDeclaration -> PointcutDeclaration

-- everthing else is only visible inside an aspect
AspectDeclaration ::= AspectHeader AspectBody
/.$putCase consumeAspectDeclaration(); $break ./
/:$readableName declaration:/
-- shortened readable name above works better in error recovery situations

AspectHeader ::= AspectHeaderName ClassHeaderExtendsopt ClassHeaderImplementsopt AspectHeaderRest
/.$putCase consumeAspectHeader(); $break ./
/:$readableName aspect header:/

AspectHeaderName -> AspectHeaderName1
/:$readableName aspect header:/

AspectHeaderName -> AspectHeaderName2
/:$readableName aspect header:/

AspectHeaderName ::= AspectHeaderName1 TypeParameters
/.$putCase consumeAspectHeaderNameWithTypeParameters(false); $break ./

-- (pr235505)
AspectHeaderName ::= AspectHeaderName2 TypeParameters
/.$putCase consumeAspectHeaderNameWithTypeParameters(true); $break ./

AspectHeaderName1 ::= Modifiersopt 'aspect' 'Identifier'
/.$putCase consumeAspectHeaderName(false); $break ./
/:$readableName aspect declaration:/

AspectHeaderName2 ::= Modifiersopt 'privileged' Modifiersopt  'aspect' 'Identifier'
/.$putCase consumeAspectHeaderName(true); $break ./
/:$readableName privileged aspect declaration:/

AspectHeaderRest ::= $empty

--[dominates TypePattern] [persingleton() | percflow(PCD) | perthis(PCD) | pertarget(PCD)]
AspectHeaderRest ::= AspectHeaderRestStart PseudoTokens
/.$putCase consumeAspectHeaderRest(); $break ./
/:$readableName per-clause:/

AspectHeaderRestStart ::= 'Identifier'
/.$putCase consumePseudoTokenIdentifier(); $break ./
/:$readableName per-clause:/

AspectBody ::= '{' AspectBodyDeclarationsopt '}'
/:$readableName body:/

AspectBodyDeclarations ::= AspectBodyDeclaration
AspectBodyDeclarations ::= AspectBodyDeclarations AspectBodyDeclaration
/.$putCase consumeClassBodyDeclarations(); $break ./
/:$readableName member declaration:/

AspectBodyDeclarationsopt ::= $empty
/.$putCase consumeEmptyClassBodyDeclarationsopt(); $break ./
/:$readableName empty aspect body:/

-- ??? why is NestedType here
AspectBodyDeclarationsopt ::= NestedType AspectBodyDeclarations
/.$putCase consumeClassBodyDeclarationsopt(); $break ./


AspectBodyDeclaration ::= ClassBodyDeclarationNoAroundMethod
/.$putCase consumeClassBodyDeclarationInAspect(); $break ./
/:$readableName aspect member declaration:/

--*****************************************
-- these rules are a copy of ClassBodyDeclaration rules, going down the member route until
-- we hit method declarations, at which point we disallow a method called around.
--*****************************************

ClassBodyDeclarationNoAroundMethod -> ClassMemberDeclarationNoAroundMethod
ClassBodyDeclarationNoAroundMethod -> StaticInitializer
ClassBodyDeclarationNoAroundMethod -> ConstructorDeclaration
--1.1 feature
ClassBodyDeclarationNoAroundMethod ::= Diet NestedMethod CreateInitializer Block
/.$putCase consumeClassBodyDeclaration(); $break ./
/:$readableName ClassBodyDeclarationNoAroundMethod:/

ClassMemberDeclarationNoAroundMethod -> PointcutDeclaration
ClassMemberDeclarationNoAroundMethod -> AspectDeclaration
ClassMemberDeclarationNoAroundMethod -> FieldDeclaration
ClassMemberDeclarationNoAroundMethod -> MethodDeclarationNoAround
--1.1 feature
ClassMemberDeclarationNoAroundMethod -> ClassDeclaration
--1.1 feature
ClassMemberDeclarationNoAroundMethod -> InterfaceDeclaration
ClassMemberDeclarationNoAroundMethod -> IntertypeClassDeclaration
-- 1.5 feature
ClassMemberDeclarationNoAroundMethod -> EnumDeclaration
ClassMemberDeclarationNoAroundMethod -> AnnotationTypeDeclaration
ClassMemberDeclarationNoAroundMethod -> RecordDeclaration
/:$readableName ClassMemberDeclaration:/

-- Empty declarations are not valid Java ClassMemberDeclarations.
-- However, since the current (2/14/97) Java compiler accepts them
-- (in fact, some of the official tests contain this erroneous
-- syntax)
ClassMemberDeclarationNoAroundMethod ::= ';'
/.$putCase consumeEmptyTypeDeclaration(); $break./

MethodDeclarationNoAround -> AbstractMethodDeclarationNoAround
MethodDeclarationNoAround ::= MethodHeaderNoAround MethodBody
/.$putCase // set to true to consume a method with a body
 consumeMethodDeclaration(true, false);  $break ./
/:$readableName MethodDeclarationNoAround:/

AbstractMethodDeclarationNoAround ::= MethodHeaderNoAround ';'
/.$putCase // set to false to consume a method without body
 consumeMethodDeclaration(false, false); $break ./
/:$readableName MethodDeclaration:/

MethodHeaderNoAround ::= MethodHeaderNameNoAround FormalParameterListopt MethodHeaderRightParen MethodHeaderExtendedDims MethodHeaderThrowsClauseopt
/.$putCase consumeMethodHeader(); $break ./
/:$readableName MethodDeclaration:/

MethodHeaderNameNoAround ::= Modifiersopt TypeParameters Type JavaIdentifierNoAround '('
/.$putCase consumeMethodHeaderNameWithTypeParameters(false); $break ./
MethodHeaderNameNoAround ::= Modifiersopt Type JavaIdentifierNoAround '('
/.$putCase consumeMethodHeaderName(false); $break ./
/:$readableName MethodHeaderName:/

--*****************************************
-- end copy of ClassBodyDeclaration rules *
--*****************************************

-- pointcuts and advice

PointcutDeclaration ::= PointcutHeader FormalParameterListopt MethodHeaderRightParen ';'
/.$putCase consumeEmptyPointcutDeclaration(); $break ./
/:$readableName pointcut declaration:/

PointcutDeclaration ::= PointcutHeader FormalParameterListopt MethodHeaderRightParen ':' PseudoTokens  ';'
/.$putCase consumePointcutDeclaration(); $break ./
/:$readableName pointcut declaration:/

PointcutHeader ::= Modifiersopt 'pointcut'  JavaIdentifier '('
/.$putCase consumePointcutHeader(); $break ./
/:$readableName pointcut declaration:/

AspectBodyDeclaration -> AroundDeclaration
AspectBodyDeclaration -> BasicAdviceDeclaration

AroundDeclaration ::= AroundHeader MethodBody
/.$putCase consumeAroundDeclaration(); $break ./
/:$readableName around advice:/

AroundHeader ::= AroundHeaderName FormalParameterListopt MethodHeaderRightParen MethodHeaderThrowsClauseopt ':' PseudoTokens
/.$putCase consumeAroundHeader(); $break ./
/:$readableName around advice header:/


-- no modifiers are actually allowed on around, but the grammar is happier this way
AroundHeaderName ::= Modifiersopt Type  'around' '('
/.$putCase consumeAroundHeaderName(); $break ./
/:$readableName [modifiers] <return-type> around ( :/

-- Recovery rule for around with no return (pr64222)
AroundHeaderName ::= Modifiersopt 'around' '('
/.$putCase consumeAroundHeaderNameMissingReturnType(); $break ./
/:$readableName [modifiers] around (:/

BasicAdviceDeclaration ::= BasicAdviceHeader MethodBody
/.$putCase consumeBasicAdviceDeclaration(); $break ./
/:$readableName AdviceDeclaration:/


BasicAdviceHeader -> BeforeAdviceHeader
BasicAdviceHeader -> AfterAdviceHeader
/:$readableName advice header:/

BeforeAdviceHeader ::= BeforeAdviceHeaderName FormalParameterListopt MethodHeaderRightParen MethodHeaderThrowsClauseopt ':' PseudoTokens
/.$putCase consumeBasicAdviceHeader(); $break ./
/:$readableName AdviceHeader:/

AfterAdviceHeader ::= AfterAdviceHeaderName FormalParameterListopt MethodHeaderRightParen ExtraParamopt MethodHeaderThrowsClauseopt ':' PseudoTokens
/.$putCase consumeBasicAdviceHeader(); $break ./
/:$readableName AdviceHeader:/

BeforeAdviceHeaderName ::= Modifiersopt 'before' '('
/.$putCase consumeBasicAdviceHeaderName(false); $break ./
/:$readableName AdviceHeaderName:/

AfterAdviceHeaderName ::= Modifiersopt 'after' '('
/.$putCase consumeBasicAdviceHeaderName(true); $break ./
/:$readableName AdviceHeaderName:/

ExtraParamopt ::= 'Identifier' '(' FormalParameter ')'
/.$putCase consumeExtraParameterWithFormal(); $break ./
/:$readableName ExtraParam:/

ExtraParamopt ::= 'Identifier' '(' ')'
/.$putCase consumeExtraParameterNoFormal(); $break ./
/:$readableName ExtraParam:/

-- deprecated, but were probably stuck with it now
ExtraParamopt ::= 'Identifier'
/.$putCase consumeExtraParameterNoFormal(); $break ./
/:$readableName ExtraParam:/

ExtraParamopt ::= $empty


-- intertype declarations

OnType ::= JavaIdentifier
/.$putCase consumeZeroTypeAnnotations(); $break ./
OnType ::= OnType '.' JavaIdentifier
/.$putCase consumeZeroTypeAnnotations(); consumeQualifiedName(); $break ./
/:$readableName QualifiedName:/

AspectBodyDeclaration -> InterTypeMethodDeclaration
AspectBodyDeclaration -> InterTypeConstructorDeclaration
AspectBodyDeclaration -> InterTypeFieldDeclaration

InterTypeMethodDeclaration -> AbstractInterTypeMethodDeclaration
InterTypeMethodDeclaration ::= InterTypeMethodHeader MethodBody
/.$putCase // set to true to consume a method with a body
  consumeInterTypeMethodDeclaration(true);  $break ./
/:$readableName inter-type method declaration:/

InterTypeMethodHeader ::= InterTypeMethodHeaderName FormalParameterListopt MethodHeaderRightParen MethodHeaderExtendedDims MethodHeaderThrowsClauseopt
/.$putCase consumeInterTypeMethodHeader(); $break ./
/:$readableName inter-type method declaration header:/

InterTypeMethodHeaderName ::= Modifiersopt Type OnType '.' JavaIdentifier '('
/.$putCase consumeInterTypeMethodHeaderName(false,false); $break ./

-- Recovery Rule
InterTypeMethodHeaderName ::= Modifiersopt Type OnType '.' '*' '.' JavaIdentifier '('
/.$putCase consumeInterTypeMethodHeaderNameIllegallyUsingTypePattern("*"); $break ./

-- Recovery Rule
InterTypeMethodHeaderName ::= Modifiersopt Type OnType '+' '.' JavaIdentifier '('
/.$putCase consumeInterTypeMethodHeaderNameIllegallyUsingTypePattern("+"); $break ./

InterTypeMethodHeaderName ::= Modifiersopt Type OnType TypeParametersAsReference '.' JavaIdentifier '('
/.$putCase consumeInterTypeMethodHeaderName(false,true); $break ./
/:$readableName inter-type method declaration header:/

InterTypeMethodHeaderName ::= Modifiersopt TypeParameters Type OnType '.' JavaIdentifier '('
/.$putCase consumeInterTypeMethodHeaderName(true,false); $break ./

InterTypeMethodHeaderName ::= Modifiersopt TypeParameters Type OnType TypeParametersAsReference '.' JavaIdentifier '('
/.$putCase consumeInterTypeMethodHeaderName(true,true); $break ./

AbstractInterTypeMethodDeclaration ::= InterTypeMethodHeader ';'
/.$putCase // set to false to consume a method without body
  consumeInterTypeMethodDeclaration(false); $break ./
/:$readableName abstract inter-type method declaration:/

TypeParametersAsReference ::= TypeParameters
/.$putCase convertTypeParametersToSingleTypeReferences(); $break ./
/:$readableName type parameter list:/

InterTypeConstructorDeclaration ::= InterTypeConstructorHeader MethodBody
/.$putCase // set to true to consume a method with a body
  consumeInterTypeConstructorDeclaration();  $break ./
/:$readableName inter-type constructor declaration:/

InterTypeConstructorHeader ::= InterTypeConstructorHeaderName FormalParameterListopt MethodHeaderRightParen MethodHeaderThrowsClauseopt
/.$putCase consumeInterTypeConstructorHeader(); $break ./
/:$readableName inter-type constructor declaration header:/

-- using Name instead of OnType to make jikespg happier
InterTypeConstructorHeaderName ::= Modifiersopt Name '.' 'new' '('
/.$putCase consumeInterTypeConstructorHeaderName(false,false); $break ./
/:$readableName inter-type constructor declaration header:/

-- Recovery Rule
InterTypeConstructorHeaderName ::= Modifiersopt Name '.' '*' '.' 'new' '('
/.$putCase consumeInterTypeConstructorHeaderNameIllegallyUsingTypePattern("*"); $break ./
/:$readableName inter-type constructor declaration header:/

-- Recovery Rule
InterTypeConstructorHeaderName ::= Modifiersopt Name '+' '.' 'new' '('
/.$putCase consumeInterTypeConstructorHeaderNameIllegallyUsingTypePattern("+"); $break ./
/:$readableName inter-type constructor declaration header:/

InterTypeConstructorHeaderName ::= Modifiersopt TypeParameters Name '.' 'new' '('
/.$putCase consumeInterTypeConstructorHeaderName(true,false); $break ./

InterTypeConstructorHeaderName ::= Modifiersopt GenericType '.' 'new' '('
/.$putCase consumeInterTypeConstructorHeaderName(false,true); $break ./

InterTypeConstructorHeaderName ::= Modifiersopt TypeParameters GenericType '.' 'new' '('
/.$putCase consumeInterTypeConstructorHeaderName(true,true); $break ./

InterTypeFieldDeclaration ::= InterTypeFieldHeader InterTypeFieldBody ';'
/.$putCase consumeInterTypeFieldDeclaration(); $break ./
/:$readableName inter-type field declaration:/

InterTypeFieldHeader ::= Modifiersopt Type OnType '.' JavaIdentifier
/.$putCase consumeInterTypeFieldHeader(false); $break ./
/:$readableName inter-type field declaration header:/

-- Recovery Rule
InterTypeFieldHeader ::= Modifiersopt Type OnType '.' '*' '.' JavaIdentifier
/.$putCase consumeInterTypeFieldHeaderIllegallyAttemptingToUseATypePattern("*"); $break ./
/:$readableName inter-type field declaration header:/

-- Recovery Rule
InterTypeFieldHeader ::= Modifiersopt Type OnType '+' '.' JavaIdentifier
/.$putCase consumeInterTypeFieldHeaderIllegallyAttemptingToUseATypePattern("+"); $break ./
/:$readableName inter-type field declaration header:/

InterTypeFieldHeader ::= Modifiersopt Type OnType TypeParametersAsReference '.' JavaIdentifier
/.$putCase consumeInterTypeFieldHeader(true); $break ./

--InterTypeFieldDeclaration ::= Modifiersopt Type OnType '.' ITDFieldVariableDeclarator ';'
--/.$putCase consumeInterTypeFieldDeclaration(); $break ./
--
--InterTypeFieldDeclaration ::= Modifiersopt Type OnType TypeParameters '.' ITDFieldVariableDeclarator ';'
--/.$putCase consumeInterTypeFieldDeclarationWithTypeParameters(); $break ./
--
--ITDFieldVariableDeclarator ::= JavaIdentifier EnterITDVariable InterTypeFieldBody
--/:$readableName ITDFieldVariableDeclarator:/
--
--EnterITDVariable ::= $empty
--/.$putCase consumeEnterITDVariable(); $break ./
--/:$readableName EnterITDVariable:/

InterTypeFieldBody ::=  $empty
/.$putCase consumeExitITDVariableWithoutInitializer(); $break ./

InterTypeFieldBody ::= '=' ForceNoDiet VariableInitializer RestoreDiet
/.$putCase consumeExitITDVariableWithInitializer(); $break ./
/:$readableName field initialization expression:/

-- declares (more fun than a pcd)
AspectBodyDeclaration -> DeclareDeclaration

DeclareDeclaration ::= DeclareHeader PseudoTokens ';'
/.$putCase consumeDeclareDeclaration(); $break ./
/:$readableName declare statement:/

DeclareHeader ::= 'declare' 'Identifier' ':'
/.$putCase consumeDeclareHeader(); $break ./
/:$readableName declare [error | warning | parents | soft | precedence]:/


-- for declare annotation support
DeclareDeclaration ::= DeclareAnnotationHeader PseudoTokensNoColon ':' Annotations ';'
/.$putCase consumeDeclareAnnotation(' '); $break ./

DeclareDeclaration ::= DeclareAnnotationHeader PseudoTokensNoColon ':' '+' Annotations ';'
/.$putCase consumeDeclareAnnotation('+'); $break ./

DeclareDeclaration ::= DeclareAnnotationHeader PseudoTokensNoColon ':' '-' Annotations ';'
/.$putCase consumeDeclareAnnotation('-'); $break ./

Annotations -> Annotation
Annotations ::= Annotations Annotation
/:$readableName Annotations:/

DeclareAnnotationHeader ::= 'declare' '@' 'Identifier' ':'
/.$putCase consumeDeclareAnnotationHeader(); $break ./
/:$readableName declare @AnnotationName:/

-- the joy of pcds
PseudoTokens ::= PseudoToken
PseudoTokens ::= ColonPseudoToken
/:$readableName type pattern or pointcut expression:/

PseudoTokens ::= PseudoTokens ColonPseudoToken
/.$putCase consumePseudoTokens(); $break ./

PseudoTokens ::= PseudoTokens PseudoToken
/.$putCase consumePseudoTokens(); $break ./


PseudoTokensNoColon ::= PseudoToken
PseudoTokensNoColon ::= PseudoTokensNoColon PseudoToken
/.$putCase consumePseudoTokens(); $break ./
/:$readableName allowable token in pointcut or type pattern:/


ColonPseudoToken ::= ':'
/.$putCase consumePseudoToken(":"); $break ./
/:$readableName any allowable token in pointcut or type pattern, except ':':/

-- On source level 21 (preview), '_' is no longer scanned as an identifier but as a token UNDERSCORE.
-- To avoid a compile error, explicitly parse the token as an identifier. See upstream commit 8398f6c121.
PseudoToken ::= '_'
/.$putCase consumePseudoTokenIdentifier(); $break ./

PseudoToken ::= JavaIdentifier
/.$putCase consumePseudoTokenIdentifier(); $break ./
/:$readableName allowable token in pointcut or type pattern:/


PseudoToken ::= '('
/.$putCase consumePseudoToken("("); $break ./

PseudoToken ::= ')'
/.$putCase consumePseudoToken(")"); $break ./

PseudoToken ::= '.'
/.$putCase consumePseudoToken("."); $break ./
PseudoToken ::= '*'
/.$putCase consumePseudoToken("*"); $break ./

PseudoToken ::= '+'
/.$putCase consumePseudoToken("+"); $break ./

PseudoToken ::= '='
/.$putCase consumePseudoToken("="); $break ./

PseudoToken ::= '&&'
/.$putCase consumePseudoToken("&&"); $break ./

PseudoToken ::= '||'
/.$putCase consumePseudoToken("||"); $break ./

PseudoToken ::= '!'
/.$putCase consumePseudoToken("!"); $break ./

PseudoToken ::= ','
/.$putCase consumePseudoToken(","); $break ./

PseudoToken ::= '['
/.$putCase consumePseudoToken("["); $break ./

PseudoToken ::= ']'
/.$putCase consumePseudoToken("]"); $break ./

PseudoToken ::= '@'
/.$putCase consumePseudoToken("@"); $break ./

PseudoToken ::= '...'
/.$putCase consumePseudoToken("..."); $break ./

PseudoToken ::= '?'
/.$putCase consumePseudoToken("?"); $break ./

PseudoToken ::= '<'
/.$putCase consumePseudoToken("<"); $break ./

PseudoToken ::= '>'
/.$putCase consumePseudoToken(">"); $break ./

PseudoToken ::= '>>'
/.$putCase consumePseudoToken(">>"); $break ./

PseudoToken ::= '>>>'
/.$putCase consumePseudoToken(">>>"); $break ./

PseudoToken ::= '&'
/.$putCase consumePseudoToken("&"); $break ./

PseudoToken ::= '!='
/.$putCase consumePseudoToken("!="); $break ./

PseudoToken ::= PrimitiveType
/.$putCase consumePseudoTokenPrimitiveType(); $break ./

PseudoToken ::= SimpleModifier
/.$putCase consumePseudoTokenModifier(); $break ./

PseudoToken ::= Literal
/.$putCase consumePseudoTokenLiteral(); $break ./


PseudoToken ::= 'this'
/.$putCase consumePseudoToken("this", 1, true); $break ./

PseudoToken ::= 'class'
/.$putCase consumePseudoToken("class", 1, true); $break ./

PseudoToken ::= 'super'
/.$putCase consumePseudoToken("super", 1, true); $break ./


-- special handling for if
PseudoToken ::= 'if' '(' Expression ')'
/.$putCase consumePseudoTokenIf(); $break ./

PseudoToken ::= 'assert'
/.$putCase consumePseudoToken("assert", 1, true); $break ./

PseudoToken ::= 'import'
/.$putCase consumePseudoToken("import", 1, true); $break ./

PseudoToken ::= 'package'
/.$putCase consumePseudoToken("package", 1, true); $break ./

PseudoToken ::= 'throw'
/.$putCase consumePseudoToken("throw", 1, true); $break ./

PseudoToken ::= 'new'
/.$putCase consumePseudoToken("new", 1, true); $break ./

PseudoToken ::= 'do'
/.$putCase consumePseudoToken("do", 1, true); $break ./

PseudoToken ::= 'for'
/.$putCase consumePseudoToken("for", 1, true); $break ./

PseudoToken ::= 'switch'
/.$putCase consumePseudoToken("switch", 1, true); $break ./

PseudoToken ::= 'try'
/.$putCase consumePseudoToken("try", 1, true); $break ./

PseudoToken ::= 'while'
/.$putCase consumePseudoToken("while", 1, true); $break ./

PseudoToken ::= 'break'
/.$putCase consumePseudoToken("break", 1, true); $break ./

PseudoToken ::= 'continue'
/.$putCase consumePseudoToken("continue", 1, true); $break ./

PseudoToken ::= 'return'
/.$putCase consumePseudoToken("return", 1, true); $break ./

PseudoToken ::= 'case'
/.$putCase consumePseudoToken("case", 1, true); $break ./

PseudoToken ::= 'catch'
/.$putCase consumePseudoToken("catch", 0, true); $break ./

PseudoToken ::= 'instanceof'
/.$putCase consumePseudoToken("instanceof", 0, true); $break ./

PseudoToken ::= 'else'
/.$putCase consumePseudoToken("else", 0, true); $break ./

PseudoToken ::= 'extends'
/.$putCase consumePseudoToken("extends", 0, true); $break ./

PseudoToken ::= 'finally'
/.$putCase consumePseudoToken("finally", 0, true); $break ./

PseudoToken ::= 'implements'
/.$putCase consumePseudoToken("implements", 0, true); $break ./

PseudoToken ::= 'throws'
/.$putCase consumePseudoToken("throws", 0, true); $break ./

-- add all other keywords as identifiers
-- End AspectJ Extensions main block


--18.8 Productions from 8: Class Declarations
--ClassModifier ::=
--      'abstract'
--    | 'final'
--    | 'public'
--    | 'non-sealed'
--18.8.1 Productions from 8.1: Class Declarations

ClassDeclaration ::= ClassHeader ClassBody
/.$putCase consumeClassDeclaration(); $break ./
/:$readableName ClassDeclaration:/

IntertypeClassDeclaration ::= IntertypeClassHeader ClassBody
/.$putCase consumeIntertypeClassDeclaration(); $break ./
/:$readableName IntertypeClassDeclaration:/

IntertypeClassHeader ::= IntertypeClassHeaderName ClassHeaderExtendsopt ClassHeaderImplementsopt
/.$putCase consumeIntertypeClassHeader(); $break ./
/:$readableName IntertypeClassHeader:/

IntertypeClassHeaderName ::= IntertypeClassHeaderName1 TypeParameters
/.$putCase consumeIntertypeTypeHeaderNameWithTypeParameters(); $break ./

IntertypeClassHeaderName -> IntertypeClassHeaderName1
/:$readableName IntertypeClassHeaderName:/

IntertypeClassHeaderName1 ::= Modifiersopt 'class' OnType '.' JavaIdentifier
/.$putCase consumeIntertypeClassHeaderName(false); $break ./

-- TODO: Rename 'InterType*' to 'Intertype*'? Without renaming, Jikespg says:
--   The following Non-Terminal is useless: InterTypeClassHeaderName1
-- But after renaming, it says instead:
--   Reduce/reduce conflict on "LESS" between rule 410 and 303
--   Reduce/reduce conflict on "LESS" between rule 405 and 304
InterTypeClassHeaderName1 ::= Modifiersopt 'class' OnType TypeParametersAsReference '.' JavaIdentifier
/.$putCase consumeIntertypeClassHeaderName(true); $break ./
/:$readableName IntertypeClassHeader:/

ClassHeader ::= ClassHeaderName ClassHeaderExtendsopt ClassHeaderImplementsopt ClassHeaderPermittedSubclassesopt
/.$putCase consumeClassHeader(); $break ./
/:$readableName ClassHeader:/

-----------------------------------------------
-- 1.5 features : generics
-----------------------------------------------
ClassHeaderName ::= ClassHeaderName1 TypeParameters
/.$putCase consumeTypeHeaderNameWithTypeParameters(); $break ./

ClassHeaderName -> ClassHeaderName1
/:$readableName ClassHeaderName:/

ClassHeaderName1 ::= Modifiersopt 'class' JavaIdentifier -- AspectJ Extension, was 'Identifier'
/.$putCase consumeClassHeaderName1(); $break ./
/:$readableName ClassHeaderName:/

ClassHeaderExtends ::= 'extends' ClassType
/.$putCase consumeClassHeaderExtends(); $break ./
/:$readableName ClassHeaderExtends:/

ClassHeaderImplements ::= 'implements' InterfaceTypeList
/.$putCase consumeClassHeaderImplements(); $break ./
/:$readableName ClassHeaderImplements:/

InterfaceTypeList -> InterfaceType
InterfaceTypeList ::= InterfaceTypeList ',' InterfaceType
/.$putCase consumeInterfaceTypeList(); $break ./
/:$readableName InterfaceTypeList:/

InterfaceType ::= ClassOrInterfaceType
/.$putCase consumeInterfaceType(); $break ./
/:$readableName InterfaceType:/

ClassBody ::= '{' ClassBodyDeclarationsopt '}'
/:$readableName ClassBody:/
/:$no_statements_recovery:/

ClassBodyDeclarations ::= ClassBodyDeclaration
ClassBodyDeclarations ::= ClassBodyDeclarations ClassBodyDeclaration
/.$putCase consumeClassBodyDeclarations(); $break ./
/:$readableName ClassBodyDeclarations:/

ClassBodyDeclaration -> ClassMemberDeclaration
ClassBodyDeclaration -> StaticInitializer
ClassBodyDeclaration -> ConstructorDeclaration
--1.1 feature
ClassBodyDeclaration ::= Diet NestedMethod CreateInitializer Block
/.$putCase consumeClassBodyDeclaration(); $break ./
/:$readableName ClassBodyDeclaration:/

Diet ::= $empty
/.$putCase consumeDiet(); $break./
/:$readableName Diet:/

Initializer ::= Diet NestedMethod CreateInitializer Block
/.$putCase consumeClassBodyDeclaration(); $break ./
/:$readableName Initializer:/

CreateInitializer ::= $empty
/.$putCase consumeCreateInitializer(); $break./
/:$readableName CreateInitializer:/

ClassMemberDeclaration -> FieldDeclaration
ClassMemberDeclaration -> MethodDeclaration
--1.1 feature
ClassMemberDeclaration -> ClassDeclaration
--1.1 feature
ClassMemberDeclaration -> InterfaceDeclaration
-- 1.5 feature
ClassMemberDeclaration -> EnumDeclaration
ClassMemberDeclaration -> AnnotationTypeDeclaration
-- Java 14 feature
ClassMemberDeclaration -> RecordDeclaration
/:$readableName ClassMemberDeclaration:/

-- Empty declarations are not valid Java ClassMemberDeclarations.
-- However, since the current (2/14/97) Java compiler accepts them
-- (in fact, some of the official tests contain this erroneous
-- syntax)
ClassMemberDeclaration ::= ';'
/.$putCase consumeEmptyTypeDeclaration(); $break./

GenericMethodDeclaration -> MethodDeclaration
GenericMethodDeclaration -> ConstructorDeclaration
/:$readableName GenericMethodDeclaration:/

--18.8.2 Productions from 8.3: Field Declarations
--VariableModifier ::=
--      'public'
--    | 'protected'
--    | 'private'
--    | 'static'
--    | 'final'
--    | 'transient'
--    | 'volatile'

FieldDeclaration ::= Modifiersopt Type VariableDeclarators ';'
/.$putCase consumeFieldDeclaration(); $break ./
/:$readableName FieldDeclaration:/

VariableDeclarators -> VariableDeclarator
VariableDeclarators ::= VariableDeclarators ',' VariableDeclarator
/.$putCase consumeVariableDeclarators(); $break ./
/:$readableName VariableDeclarators:/
/:$recovery_template JavaIdentifier:/ -- AspectJ Extension, was Identifier

VariableDeclarator ::= VariableDeclaratorId EnterVariable ExitVariableWithoutInitialization
VariableDeclarator ::= VariableDeclaratorId EnterVariable '=' ForceNoDiet VariableInitializer RestoreDiet ExitVariableWithInitialization
/:$readableName VariableDeclarator:/
/:$recovery_template JavaIdentifier:/ -- AspectJ Extension, was Identifier

EnterVariable ::= $empty
/.$putCase consumeEnterVariable(); $break ./
/:$readableName EnterVariable:/

ExitVariableWithInitialization ::= $empty
/.$putCase consumeExitVariableWithInitialization(); $break ./
/:$readableName ExitVariableWithInitialization:/

ExitVariableWithoutInitialization ::= $empty
/.$putCase consumeExitVariableWithoutInitialization(); $break ./
/:$readableName ExitVariableWithoutInitialization:/

ForceNoDiet ::= $empty
/.$putCase consumeForceNoDiet(); $break ./
/:$readableName ForceNoDiet:/
RestoreDiet ::= $empty
/.$putCase consumeRestoreDiet(); $break ./
/:$readableName RestoreDiet:/

VariableDeclaratorId ::= JavaIdentifier Dimsopt -- AspectJ extension : was 'Identifier'
/:$readableName VariableDeclaratorId:/
/:$recovery_template JavaIdentifier:/ -- AspectJ Extension, was Identifier
VariableDeclaratorId ::= '_'
/.$putCase consumeUnnamedVariable(); $break ./

VariableInitializer -> Expression
VariableInitializer -> ArrayInitializer
/:$readableName VariableInitializer:/
/:$recovery_template JavaIdentifier:/ -- AspectJ Extension, was Identifier

--18.8.3 Productions from 8.4: Method Declarations
--MethodModifier ::=
--      'public'
--    | 'protected'
--    | 'private'
--    | 'static'
--    | 'abstract'
--    | 'final'
--    | 'native'
--    | 'synchronized'
--

MethodDeclaration -> AbstractMethodDeclaration
MethodDeclaration ::= MethodHeader MethodBody
/.$putCase // set to true to consume a method with a body
 consumeMethodDeclaration(true, false); $break ./
/:$readableName MethodDeclaration:/

MethodDeclaration ::= DefaultMethodHeader MethodBody
/.$putCase // set to true to consume a method with a body
 consumeMethodDeclaration(true, true); $break ./
/:$readableName MethodDeclaration:/

AbstractMethodDeclaration ::= MethodHeader ';'
/.$putCase // set to false to consume a method without body
 consumeMethodDeclaration(false, false); $break ./
/:$readableName MethodDeclaration:/

MethodHeader ::= MethodHeaderName FormalParameterListopt MethodHeaderRightParen MethodHeaderExtendedDims MethodHeaderThrowsClauseopt
/.$putCase consumeMethodHeader(); $break ./
/:$readableName MethodDeclaration:/

DefaultMethodHeader ::= DefaultMethodHeaderName FormalParameterListopt MethodHeaderRightParen MethodHeaderExtendedDims MethodHeaderThrowsClauseopt
/.$putCase consumeMethodHeader(); $break ./
/:$readableName MethodDeclaration:/

MethodHeaderName ::= Modifiersopt TypeParameters Type JavaIdentifier '(' -- AspectJ Extension, was 'Identifier'
/.$putCase consumeMethodHeaderNameWithTypeParameters(false); $break ./
MethodHeaderName ::= Modifiersopt Type JavaIdentifier '('  -- AspectJ Extension, was 'Identifier'
/.$putCase consumeMethodHeaderName(false); $break ./
/:$readableName MethodHeaderName:/

DefaultMethodHeaderName ::= ModifiersWithDefault TypeParameters Type 'Identifier' '('
/.$putCase consumeMethodHeaderNameWithTypeParameters(false); $break ./
DefaultMethodHeaderName ::= ModifiersWithDefault Type 'Identifier' '('
/.$putCase consumeMethodHeaderName(false); $break ./
/:$readableName MethodHeaderName:/

ModifiersWithDefault ::= Modifiersopt 'default' Modifiersopt
/.$putCase consumePushCombineModifiers(); $break ./
/:$readableName Modifiers:/
/:$compliance 1.8:/

MethodHeaderRightParen ::= ')'
/.$putCase consumeMethodHeaderRightParen(); $break ./
/:$readableName ):/
/:$recovery_template ):/

MethodHeaderExtendedDims ::= Dimsopt
/.$putCase consumeMethodHeaderExtendedDims(); $break ./
/:$readableName MethodHeaderExtendedDims:/

MethodHeaderThrowsClause ::= 'throws' ClassTypeList
/.$putCase consumeMethodHeaderThrowsClause(); $break ./
/:$readableName MethodHeaderThrowsClause:/

ConstructorHeader ::= ConstructorHeaderName FormalParameterListopt MethodHeaderRightParen MethodHeaderThrowsClauseopt
/.$putCase consumeConstructorHeader(); $break ./
/:$readableName ConstructorDeclaration:/

ConstructorHeaderName ::= Modifiersopt TypeParameters 'Identifier' '('
/.$putCase consumeConstructorHeaderNameWithTypeParameters(); $break ./
ConstructorHeaderName ::= Modifiersopt 'Identifier' '('
/.$putCase consumeConstructorHeaderName(); $break ./
/:$readableName ConstructorHeaderName:/

-- AspectJ Extension
ConstructorHeaderName ::=  Modifiersopt 'aspect' '('  -- makes aspect harder
/.$putCase consumeConstructorHeaderName(); $break ./
-- End AspectJ Extension

FormalParameterList -> FormalParameter
FormalParameterList ::= FormalParameterList ',' FormalParameter
/.$putCase consumeFormalParameterList(); $break ./
/:$readableName FormalParameterList:/

--1.1 feature
FormalParameter ::= Modifiersopt Type VariableDeclaratorIdOrThis
/.$putCase consumeFormalParameter(false); $break ./
FormalParameter ::= Modifiersopt Type PushZeroTypeAnnotations '...' VariableDeclaratorIdOrThis
/.$putCase consumeFormalParameter(true); $break ./
/:$compliance 1.5:/
FormalParameter ::= Modifiersopt Type @308... TypeAnnotations '...' VariableDeclaratorIdOrThis
/.$putCase consumeFormalParameter(true); $break ./
/:$readableName FormalParameter:/
/:$compliance 1.8:/
/:$recovery_template JavaIdentifier JavaIdentifier:/ -- AspectJ Extension, was Identifier Identifier

CatchFormalParameter ::= Modifiersopt CatchType VariableDeclaratorId
/.$putCase consumeCatchFormalParameter(); $break ./
/:$readableName FormalParameter:/
/:$recovery_template JavaIdentifier JavaIdentifier:/ -- AspectJ Extension, was Identifier Identifier

CatchType ::= UnionType
/.$putCase consumeCatchType(); $break ./
/:$readableName CatchType:/

UnionType ::= Type
/.$putCase consumeUnionTypeAsClassType(); $break ./
UnionType ::= UnionType '|' Type
/.$putCase consumeUnionType(); $break ./
/:$readableName UnionType:/
/:$compliance 1.7:/

ClassTypeList -> ClassTypeElt
ClassTypeList ::= ClassTypeList ',' ClassTypeElt
/.$putCase consumeClassTypeList(); $break ./
/:$readableName ClassTypeList:/

ClassTypeElt ::= ClassType
/.$putCase consumeClassTypeElt(); $break ./
/:$readableName ClassType:/

MethodBody ::= NestedMethod '{' BlockStatementsopt '}'
/.$putCase consumeMethodBody(); $break ./
/:$readableName MethodBody:/
/:$no_statements_recovery:/

NestedMethod ::= $empty
/.$putCase consumeNestedMethod(); $break ./
/:$readableName NestedMethod:/

--18.8.4 Productions from 8.5: Static Initializers

StaticInitializer ::= StaticOnly Block
/.$putCase consumeStaticInitializer(); $break./
/:$readableName StaticInitializer:/

StaticOnly ::= 'static'
/.$putCase consumeStaticOnly(); $break ./
/:$readableName StaticOnly:/

--18.8.5 Productions from 8.6: Constructor Declarations
--ConstructorModifier ::=
--      'public'
--    | 'protected'
--    | 'private'
--
--
ConstructorDeclaration ::= ConstructorHeader MethodBody
/.$putCase consumeConstructorDeclaration() ; $break ./
-- These rules are added to be able to parse constructors with no body
ConstructorDeclaration ::= ConstructorHeader ';'
/.$putCase consumeInvalidConstructorDeclaration() ; $break ./
/:$readableName ConstructorDeclaration:/

-- the rules ExplicitConstructorInvocationopt has been expanded
-- in the rule below in order to make the grammar lalr(1).

ExplicitConstructorInvocation ::= 'this' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocation(0, THIS_CALL); $break ./

ExplicitConstructorInvocation ::= OnlyTypeArguments 'this' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocationWithTypeArguments(0,THIS_CALL); $break ./

ExplicitConstructorInvocation ::= 'super' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocation(0,SUPER_CALL); $break ./

ExplicitConstructorInvocation ::= OnlyTypeArguments 'super' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocationWithTypeArguments(0,SUPER_CALL); $break ./

--1.1 feature
ExplicitConstructorInvocation ::= Primary '.' 'super' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocation(1, SUPER_CALL); $break ./

ExplicitConstructorInvocation ::= Primary '.' OnlyTypeArguments 'super' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocationWithTypeArguments(1, SUPER_CALL); $break ./

--1.1 feature
ExplicitConstructorInvocation ::= Name '.' 'super' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocation(2, SUPER_CALL); $break ./

ExplicitConstructorInvocation ::= Name '.' OnlyTypeArguments 'super' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocationWithTypeArguments(2, SUPER_CALL); $break ./

--1.1 feature
ExplicitConstructorInvocation ::= Primary '.' 'this' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocation(1, THIS_CALL); $break ./

ExplicitConstructorInvocation ::= Primary '.' OnlyTypeArguments 'this' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocationWithTypeArguments(1, THIS_CALL); $break ./

--1.1 feature
ExplicitConstructorInvocation ::= Name '.' 'this' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocation(2, THIS_CALL); $break ./

ExplicitConstructorInvocation ::= Name '.' OnlyTypeArguments 'this' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocationWithTypeArguments(2, THIS_CALL); $break ./
/:$readableName ExplicitConstructorInvocation:/

--18.9 Productions from 9: Interface Declarations

--18.9.1 Productions from 9.1: Interface Declarations
--InterfaceModifier ::=
--      'public'
--    | 'abstract'
--    | 'non-sealed'
--
InterfaceDeclaration ::= InterfaceHeader InterfaceBody
/.$putCase consumeInterfaceDeclaration(); $break ./
/:$readableName InterfaceDeclaration:/

InterfaceHeader ::= InterfaceHeaderName InterfaceHeaderExtendsopt InterfaceHeaderPermittedSubClassesAndSubInterfacesopt
/.$putCase consumeInterfaceHeader(); $break ./
/:$readableName InterfaceHeader:/

-----------------------------------------------
-- 1.5 features : generics
-----------------------------------------------
InterfaceHeaderName ::= InterfaceHeaderName1 TypeParameters
/.$putCase consumeTypeHeaderNameWithTypeParameters(); $break ./

InterfaceHeaderName -> InterfaceHeaderName1
/:$readableName InterfaceHeaderName:/

InterfaceHeaderName1 ::= Modifiersopt interface JavaIdentifier -- AspectJ Extension, was Identifier
/.$putCase consumeInterfaceHeaderName1(); $break ./
/:$readableName InterfaceHeaderName:/

InterfaceHeaderExtends ::= 'extends' InterfaceTypeList
/.$putCase consumeInterfaceHeaderExtends(); $break ./
/:$readableName InterfaceHeaderExtends:/

InterfaceBody ::= '{' InterfaceMemberDeclarationsopt '}'
/:$readableName InterfaceBody:/

InterfaceMemberDeclarations -> InterfaceMemberDeclaration
InterfaceMemberDeclarations ::= InterfaceMemberDeclarations InterfaceMemberDeclaration
/.$putCase consumeInterfaceMemberDeclarations(); $break ./
/:$readableName InterfaceMemberDeclarations:/

--same as for class members
InterfaceMemberDeclaration ::= ';'
/.$putCase consumeEmptyTypeDeclaration(); $break ./
/:$readableName InterfaceMemberDeclaration:/


InterfaceMemberDeclaration -> ConstantDeclaration
InterfaceMemberDeclaration ::= DefaultMethodHeader MethodBody
/:$compliance 1.8:/
/.$putCase consumeInterfaceMethodDeclaration(false); $break ./
InterfaceMemberDeclaration ::= MethodHeader MethodBody
/.$putCase consumeInterfaceMethodDeclaration(false); $break ./
/:$readableName InterfaceMemberDeclaration:/
-- the next rule is illegal but allows to give a more canonical error message from inside consumeInterfaceMethodDeclaration():
InterfaceMemberDeclaration ::= DefaultMethodHeader ';'
/:$compliance 1.8:/
/.$putCase consumeInterfaceMethodDeclaration(true); $break ./

-- These rules are added to be able to parse constructors inside interface and then report a relevent error message
InvalidConstructorDeclaration ::= ConstructorHeader MethodBody
/.$putCase consumeInvalidConstructorDeclaration(true); $break ./
InvalidConstructorDeclaration ::= ConstructorHeader ';'
/.$putCase consumeInvalidConstructorDeclaration(false); $break ./
/:$readableName InvalidConstructorDeclaration:/

-- These rules are added to be able to parse initializers inside an interface and then report a relevent error message (bug 212713)
InvalidInitializer -> StaticInitializer
InvalidInitializer -> Initializer
/:$readableName InvalidInitializer:/


InterfaceMemberDeclaration -> AbstractMethodDeclaration
InterfaceMemberDeclaration -> InvalidConstructorDeclaration
InterfaceMemberDeclaration -> InvalidInitializer
--1.1 feature
InterfaceMemberDeclaration -> ClassDeclaration
--1.1 feature
InterfaceMemberDeclaration -> InterfaceDeclaration
InterfaceMemberDeclaration -> EnumDeclaration
InterfaceMemberDeclaration -> AnnotationTypeDeclaration
InterfaceMemberDeclaration -> RecordDeclaration
/:$readableName InterfaceMemberDeclaration:/

-----------------------------------------------
-- 14 feature : record type
-----------------------------------------------

RecordDeclaration ::= RecordHeaderPart RecordBody
/.$putCase consumeRecordDeclaration(); $break ./
/:$readableName RecordDeclaration:/
/:$compliance 14:/

RecordHeaderPart ::= RecordHeaderName RecordHeader ClassHeaderImplementsopt
/.$putCase consumeRecordHeaderPart(); $break ./
/:$readableName RecordHeaderPart:/
/:$compliance 14:/

RecordHeaderName ::= RecordHeaderName1 TypeParameters
/.$putCase consumeRecordHeaderNameWithTypeParameters(); $break ./
/:$compliance 14:/

RecordHeaderName -> RecordHeaderName1
/:$readableName RecordHeaderName:/
/:$compliance 14:/

RecordHeaderName1 ::= Modifiersopt RestrictedIdentifierrecord JavaIdentifier -- AspectJ extension, was 'Identifier'
/.$putCase consumeRecordHeaderName1(); $break ./
/:$readableName RecordHeaderName:/
/:$compliance 14:/

RecordComponentHeaderRightParen ::= ')'
/.$putCase consumeRecordComponentHeaderRightParen(); $break ./
/:$readableName ):/
/:$recovery_template ):/
/:$compliance 14:/

RecordHeader ::= '(' RecordComponentsopt RecordComponentHeaderRightParen
/.$putCase consumeRecordHeader(); $break ./
/:$readableName RecordHeader:/
/:$compliance 14:/

RecordComponentsopt ::= $empty
/.$putCase consumeRecordComponentsopt(); $break ./
RecordComponentsopt -> RecordComponents
/:$readableName RecordComponentsopt:/
/:$compliance 14:/

RecordComponents -> RecordComponent
RecordComponents ::= RecordComponents ',' RecordComponent
/.$putCase consumeRecordComponents(); $break ./
/:$readableName RecordComponents:/
/:$compliance 14:/

RecordComponent -> VariableArityRecordComponent
RecordComponent ::= Modifiersopt Type VariableDeclaratorId
/.$putCase consumeRecordComponent(false); $break ./
/:$readableName RecordComponent:/
/:$compliance 14:/

VariableArityRecordComponent ::= Modifiersopt Type PushZeroTypeAnnotations '...' VariableDeclaratorId
/.$putCase consumeRecordComponent(true); $break ./
/:$readableName VariableArityRecordComponent:/
/:$compliance 14:/

VariableArityRecordComponent ::= Modifiersopt Type @308... TypeAnnotations '...' VariableDeclaratorId
/.$putCase consumeRecordComponent(true); $break ./
/:$readableName VariableArityRecordComponent:/
/:$compliance 14:/
/:$recovery_template JavaIdentifier JavaIdentifier:/  -- AspectJ extension, was Identifier Identifier

RecordBody ::= '{' RecordBodyDeclarationopt '}'
/.$putCase consumeRecordBody(); $break ./
/:$readableName RecordBody:/
/:$compliance 14:/

RecordBodyDeclarationopt ::= $empty
/.$putCase consumeEmptyRecordBodyDeclaration(); $break ./
RecordBodyDeclarationopt -> RecordBodyDeclarations
/:$readableName RecordBodyDeclarationopt:/
/:$compliance 14:/

RecordBodyDeclarations ::= RecordBodyDeclaration
RecordBodyDeclarations ::= RecordBodyDeclarations RecordBodyDeclaration
/.$putCase consumeRecordBodyDeclarations(); $break ./
/:$readableName RecordBodyDeclarations:/
/:$compliance 14:/

RecordBodyDeclaration ::=  ClassBodyDeclaration
/.$putCase consumeRecordBodyDeclaration(); $break ./
RecordBodyDeclaration ::=  CompactConstructorDeclaration
/.$putCase consumeRecordBodyDeclaration(); $break ./
/:$readableName RecordBodyDeclaration:/
/:$compliance 14:/

CompactConstructorDeclaration ::= CompactConstructorHeader MethodBody
/.$putCase consumeCompactConstructorDeclaration(); $break ./
/:$readableName CompactConstructorDeclaration:/
/:$compliance 14:/

CompactConstructorHeader ::= CompactConstructorHeaderName MethodHeaderThrowsClauseopt
/.$putCase consumeCompactConstructorHeader(); $break ./
/:$readableName CompactConstructorDeclaration:/
/:$compliance 14:/

CompactConstructorHeaderName ::= Modifiersopt JavaIdentifier -- AspectJ was 'Identifier'
/.$putCase consumeCompactConstructorHeaderName(); $break ./
CompactConstructorHeaderName ::= Modifiersopt TypeParameters JavaIdentifier -- AspectJ was 'Identifier'
/.$putCase consumeCompactConstructorHeaderNameWithTypeParameters(); $break ./
/:$readableName CompactConstructorHeaderName:/
/:$compliance 14:/

-----------------------------------------------
-- 14 feature : end of record type
-----------------------------------------------

-----------------------------------------------
-- 16 feature : instanceof pattern matching
-----------------------------------------------

InstanceofExpression -> RelationalExpression
InstanceofExpression ::= InstanceofExpression InstanceofRHS
/.$putCase consumeInstanceOfExpression(); $break ./
/:$readableName Expression:/

InstanceofRHS -> InstanceofClassic
InstanceofRHS -> InstanceofPattern
/.$putCase consumeInstanceOfRHS(); $break ./
/:$readableName Expression:/

InstanceofClassic ::= 'instanceof' Modifiersopt Type
/.$putCase consumeInstanceOfClassic(); $break ./
/:$readableName InstanceofClassic:/

InstanceofPattern ::=  'instanceof' Pattern
/.$putCase consumeInstanceofPattern(); $break ./
/:$readableName InstanceofPattern:/


Pattern -> TypePattern
Pattern -> RecordPattern
/.$putCase consumePattern(); $break ./
/:$readableName Pattern:/

TypePattern ::= Modifiersopt Type JavaIdentifier  -- AspectJ extension, was 'Identifier'
/.$putCase consumeTypePattern(); $break ./
/:$readableName TypePattern:/
TypePattern ::= Modifiersopt Type '_'
/.$putCase consumeTypePattern(); $break ./
/:$readableName TypePattern:/
/:$compliance 21:/

-----------------------------------------------
-- 16 feature : end of instanceof pattern matching
-----------------------------------------------

-----------------------------------------------
-- 20 preview feature : record patterns
-----------------------------------------------

RecordPattern ::= Modifiersopt ReferenceType PushLPAREN ComponentPatternListopt PushRPAREN
/.$putCase consumeRecordPattern(); $break ./
/:$readableName RecordPattern:/
/:$compliance 20:/

ComponentPatternListopt ::=  $empty
/.$putCase consumePatternListopt(); $break ./
/:$readableName ComponentPatternListopt:/
/:$compliance 20:/

ComponentPatternListopt -> ComponentPatternList
/:$readableName PatternListopt:/
/:$compliance 20:/

ComponentPatternList -> ComponentPattern
ComponentPatternList ::= ComponentPatternList ',' ComponentPattern
/.$putCase consumePatternList();  $break ./
/:$readableName ComponentPatternList:/
/:$compliance 20:/

ComponentPattern -> Pattern
ComponentPattern -> UnnamedPattern
/:$compliance 21:/

-----------------------------------------------
-- 20 preview feature : end of record patterns
-----------------------------------------------
-----------------------------------------------
-- 21 preview feature : String templates
-----------------------------------------------

PrimaryNoNewArray -> StringTemplateExpression

TemplateArgument -> StringLiteral
TemplateArgument -> TextBlock
TemplateArgument -> StringTemplate
TemplateArgument -> TextBlockTemplate

StringTemplateExpression ::= Name '.' TemplateArgument
/.$putCase consumeTemplateExpressionWithName(); $break ./
/:$readableName TemplateExpression:/
/:$compliance 21:/

StringTemplateExpression ::= Primary '.' TemplateArgument
/.$putCase consumeTemplateExpressionWithPrimary(); $break ./
/:$readableName TemplateExpression:/
/:$compliance 21:/

--TemplateProcessor ::= Expression
--/:$compliance 21:/

-----------------------------------------------
-- 21 preview feature : end of String templates
-----------------------------------------------

UnnamedPattern ::= '_'
/.$putCase consumeUnnamedPattern(); $break ./
/:$readableName UnnamedPattern:/
/:$compliance 21:/

ConstantDeclaration -> FieldDeclaration
/:$readableName ConstantDeclaration:/

PushLeftBrace ::= $empty
/.$putCase consumePushLeftBrace(); $break ./
/:$readableName PushLeftBrace:/

ArrayInitializer ::= '{' PushLeftBrace ,opt '}'
/.$putCase consumeEmptyArrayInitializer(); $break ./
ArrayInitializer ::= '{' PushLeftBrace VariableInitializers '}'
/.$putCase consumeArrayInitializer(); $break ./
ArrayInitializer ::= '{' PushLeftBrace VariableInitializers , '}'
/.$putCase consumeArrayInitializer(); $break ./
/:$readableName ArrayInitializer:/
/:$recovery_template JavaIdentifier:/ -- AspectJ extension, was Identifier

VariableInitializers ::= VariableInitializer
VariableInitializers ::= VariableInitializers ',' VariableInitializer
/.$putCase consumeVariableInitializers(); $break ./
/:$readableName VariableInitializers:/

Block ::= OpenBlock '{' BlockStatementsopt '}'
/.$putCase consumeBlock(); $break ./
/:$readableName Block:/

OpenBlock ::= $empty
/.$putCase consumeOpenBlock() ; $break ./
/:$readableName OpenBlock:/

BlockStatements ::= BlockStatement
/.$putCase consumeBlockStatement() ; $break ./
/:$readableName BlockStatements:/
BlockStatements ::= BlockStatements BlockStatement
/.$putCase consumeBlockStatements() ; $break ./
/:$readableName BlockStatements:/

-- Production name hardcoded in parser. Must be ::= and not ->
BlockStatementopt ::= BlockStatementopt0
/:$readableName BlockStatementopt:/
BlockStatementopt0 -> $empty
BlockStatementopt0 -> BlockStatement
/:$readableName BlockStatementopt0:/

BlockStatement -> LocalVariableDeclarationStatement
BlockStatement -> Statement
--1.1 feature
BlockStatement -> ClassDeclaration
BlockStatement -> RecordDeclaration
BlockStatement ::= InterfaceDeclaration
/.$putCase consumeInvalidInterfaceDeclaration(); $break ./
/:$readableName BlockStatement:/
BlockStatement ::= AnnotationTypeDeclaration
/.$putCase consumeInvalidAnnotationTypeDeclaration(); $break ./
/:$readableName BlockStatement:/
BlockStatement ::= EnumDeclaration
/.$putCase consumeInvalidEnumDeclaration(); $break ./
/:$readableName BlockStatement:/

LocalVariableDeclarationStatement ::= LocalVariableDeclaration ';'
/.$putCase consumeLocalVariableDeclarationStatement(); $break ./
/:$readableName LocalVariableDeclarationStatement:/

LocalVariableDeclaration ::= Type PushModifiers VariableDeclarators
/.$putCase consumeLocalVariableDeclaration(); $break ./
-- 1.1 feature
-- The modifiers part of this rule makes the grammar more permissive.
-- The only modifier here is final. We put Modifiers to allow multiple modifiers
-- This will require to check the validity of the modifier
LocalVariableDeclaration ::= Modifiers Type PushRealModifiers VariableDeclarators
/.$putCase consumeLocalVariableDeclaration(); $break ./
/:$readableName LocalVariableDeclaration:/

PushModifiers ::= $empty
/.$putCase consumePushModifiers(); $break ./
/:$readableName PushModifiers:/

PushModifiersForHeader ::= $empty
/.$putCase consumePushModifiersForHeader(); $break ./
/:$readableName PushModifiersForHeader:/

PushRealModifiers ::= $empty
/.$putCase consumePushRealModifiers(); $break ./
/:$readableName PushRealModifiers:/

Statement -> StatementWithoutTrailingSubstatement
Statement -> LabeledStatement
Statement -> IfThenStatement
Statement -> IfThenElseStatement
Statement -> WhileStatement
Statement -> ForStatement
-----------------------------------------------
-- 1.5 feature
-----------------------------------------------
Statement -> EnhancedForStatement
/:$readableName Statement:/
/:$recovery_template ;:/

StatementNoShortIf -> StatementWithoutTrailingSubstatement
StatementNoShortIf -> LabeledStatementNoShortIf
StatementNoShortIf -> IfThenElseStatementNoShortIf
StatementNoShortIf -> WhileStatementNoShortIf
StatementNoShortIf -> ForStatementNoShortIf
-----------------------------------------------
-- 1.5 feature
-----------------------------------------------
StatementNoShortIf -> EnhancedForStatementNoShortIf
/:$readableName Statement:/

StatementWithoutTrailingSubstatement -> AssertStatement
StatementWithoutTrailingSubstatement -> Block
StatementWithoutTrailingSubstatement -> EmptyStatement
StatementWithoutTrailingSubstatement -> ExpressionStatement
StatementWithoutTrailingSubstatement -> SwitchStatement
StatementWithoutTrailingSubstatement -> DoStatement
StatementWithoutTrailingSubstatement -> BreakStatement
StatementWithoutTrailingSubstatement -> ContinueStatement
StatementWithoutTrailingSubstatement -> ReturnStatement
StatementWithoutTrailingSubstatement -> SynchronizedStatement
StatementWithoutTrailingSubstatement -> ThrowStatement
StatementWithoutTrailingSubstatement -> TryStatement
StatementWithoutTrailingSubstatement -> TryStatementWithResources
StatementWithoutTrailingSubstatement -> YieldStatement
/:$readableName Statement:/

EmptyStatement ::= ';'
/.$putCase consumeEmptyStatement(); $break ./
/:$readableName EmptyStatement:/

LabeledStatement ::= Label ':' Statement
/.$putCase consumeStatementLabel() ; $break ./
/:$readableName LabeledStatement:/

LabeledStatementNoShortIf ::= Label ':' StatementNoShortIf
/.$putCase consumeStatementLabel() ; $break ./
/:$readableName LabeledStatement:/

Label ::= 'JavaIdentifier' -- AspectJ Extension, was 'Identifier'
/.$putCase consumeLabel() ; $break ./
/:$readableName Label:/

ExpressionStatement ::= StatementExpression ';'
/. $putCase consumeExpressionStatement(); $break ./
ExpressionStatement ::= ExplicitConstructorInvocation
/:$readableName Statement:/

StatementExpression ::= Assignment
StatementExpression ::= PreIncrementExpression
StatementExpression ::= PreDecrementExpression
StatementExpression ::= PostIncrementExpression
StatementExpression ::= PostDecrementExpression
StatementExpression ::= MethodInvocation
StatementExpression ::= ClassInstanceCreationExpression
/:$readableName Expression:/

PostExpressionInSwitchStatement ::= $empty
/.$putCase consumePostExpressionInSwitch(true); $break ./

PostExpressionInSwitchExpression ::= $empty
/.$putCase consumePostExpressionInSwitch(false); $break ./

PostExpressionInIf ::= $empty
/.$putCase consumePostExpressionInIf(); $break ./

PostExpressionInWhile ::= $empty
/.$putCase consumePostExpressionInWhile(); $break ./

IfThenStatement ::= 'if' '(' Expression ')' PostExpressionInIf Statement
/.$putCase consumeStatementIfNoElse(); $break ./
/:$readableName IfStatement:/

IfThenElseStatement ::= 'if' '(' Expression ')' PostExpressionInIf StatementNoShortIf 'else' Statement
/.$putCase consumeStatementIfWithElse(); $break ./
/:$readableName IfStatement:/

IfThenElseStatementNoShortIf ::= 'if' '(' Expression ')' PostExpressionInIf StatementNoShortIf 'else' StatementNoShortIf
/.$putCase consumeStatementIfWithElse(); $break ./
/:$readableName IfStatement:/

SwitchStatement ::= 'switch' '(' Expression ')' PostExpressionInSwitchStatement OpenBlock SwitchBlock
/.$putCase consumeStatementSwitch() ; $break ./
/:$readableName SwitchStatement:/

SwitchBlock ::= '{' '}'
/.$putCase consumeEmptySwitchBlock() ; $break ./

SwitchBlock ::= '{' SwitchBlockStatements '}'
SwitchBlock ::= '{' SwitchLabels '}'
SwitchBlock ::= '{' SwitchBlockStatements SwitchLabels '}'
/.$putCase consumeSwitchBlock() ; $break ./
/:$readableName SwitchBlock:/

SwitchBlockStatements -> SwitchBlockStatement
SwitchBlockStatements ::= SwitchBlockStatements SwitchBlockStatement
/.$putCase consumeSwitchBlockStatements() ; $break ./
/:$readableName SwitchBlockStatements:/

SwitchBlockStatement -> SwitchLabeledRule
SwitchBlockStatement ::= SwitchLabels BlockStatements
/.$putCase consumeSwitchBlockStatement() ; $break ./
/:$readableName SwitchBlockStatement:/

SwitchLabels -> SwitchLabel
SwitchLabels ::= SwitchLabels SwitchLabel
/.$putCase consumeSwitchLabels() ; $break ./
/:$readableName SwitchLabels:/

SwitchLabel ::= SwitchLabelCaseLhs ':'
/. $putCase consumeCaseLabel(); $break ./

SwitchLabel ::= 'default' ':'
/. $putCase consumeDefaultLabel(); $break ./
/:$readableName SwitchLabel:/

-- BEGIN SwitchExpression (JEP 325) --

UnaryExpressionNotPlusMinus -> SwitchExpression
UnaryExpressionNotPlusMinus_NotName -> SwitchExpression

SwitchExpression ::= 'switch' '(' Expression ')' PostExpressionInSwitchExpression OpenBlock SwitchBlock
/.$putCase consumeSwitchExpression() ; $break ./
/:$readableName SwitchExpression:/

SwitchLabeledRule ::= SwitchLabeledExpression
SwitchLabeledRule ::= SwitchLabeledBlock
SwitchLabeledRule ::= SwitchLabeledThrowStatement
/. $putCase consumeSwitchLabeledRule(); $break ./
/:$readableName SwitchLabeledRule:/

SwitchLabeledExpression ::= SwitchLabelExpr Expression ';'
/. $putCase consumeSwitchLabeledExpression(); $break ./
/:$readableName SwitchLabeledExpression:/

SwitchLabeledBlock ::= SwitchLabelExpr Block
/. $putCase consumeSwitchLabeledBlock(); $break ./
/:$readableName SwitchLabeledBlock:/

SwitchLabeledThrowStatement ::= SwitchLabelExpr ThrowExpression ';'
/. $putCase consumeSwitchLabeledThrowStatement(); $break ./
/:$readableName SwitchLabeledThrowStatement:/

SwitchLabelExpr ::= 'default'  '->'
/. $putCase consumeDefaultLabelExpr(); $break ./
/:$readableName SwitchLabelDefaultExpr:/

SwitchLabelExpr ::= SwitchLabelCaseLhs BeginCaseExpr '->'
/. $putCase consumeCaseLabelExpr(); $break ./
/:$readableName SwitchLabelExpr:/

SwitchLabelCaseLhs ::= 'case' CaseLabelElements
/. $putCase consumeSwitchLabelCaseLhs(); $break ./
/:$readableName SwitchLabelCaseLhs:/

-- END SwitchExpression (JEP 325) --

CaseLabelElements -> CaseLabelElement
CaseLabelElements ::= CaseLabelElements ',' CaseLabelElement
/.$putCase consumeCaseLabelElements(); $break ./
/:$readableName CaseLabelElements:/

-- Production name hardcoded in parser. Must be ::= and not -> (need to hook at cCLE)
CaseLabelElement ::= ConstantExpression
/.$putCase consumeCaseLabelElement(CaseLabelKind.CASE_EXPRESSION); $break ./
/:$readableName CaseLabelElement:/

 -- following 'null' in CASE_EXPRESSION - passes through existing grammar
 -- CaseLabelElement ->  'null'

CaseLabelElement ::= 'default'
/.$putCase consumeCaseLabelElement(CaseLabelKind.CASE_DEFAULT); $break ./
/:$readableName CaseLabelElement:/

CaseLabelElement ::= CaseLabelElementPattern
/.$putCase consumeCaseLabelElement(CaseLabelKind.CASE_PATTERN); $break ./
/:$readableName CaseLabelElement:/

CaseLabelElement ::=  CaseLabelElementPattern Guard
/.$putCase consumeCaseLabelElement(CaseLabelKind.CASE_PATTERN); $break ./
/:$readableName CaseLabelElement:/

CaseLabelElementPattern ::= BeginCaseElement Pattern
/.$putCase consumeCaseLabelElementPattern(); $break ./
/:$readableName CaseLabelElementPattern:/

Guard ::= RestrictedIdentifierWhen Expression
/.$putCase consumeGuard(); $break ./
/:$readableName Guard:/
/:$compliance 19:/

YieldStatement ::= RestrictedIdentifierYield Expression ;
/.$putCase consumeStatementYield() ; $break ./
/:$readableName YieldStatement:/

WhileStatement ::= 'while' '(' Expression ')' PostExpressionInWhile Statement
/.$putCase consumeStatementWhile() ; $break ./
/:$readableName WhileStatement:/

WhileStatementNoShortIf ::= 'while' '(' Expression ')' PostExpressionInWhile StatementNoShortIf
/.$putCase consumeStatementWhile() ; $break ./
/:$readableName WhileStatement:/

DoStatement ::= 'do' Statement 'while' '(' Expression ')' ';'
/.$putCase consumeStatementDo() ; $break ./
/:$readableName DoStatement:/

ForStatement ::= 'for' '(' ForInitopt ';' Expressionopt ';' ForUpdateopt ')' Statement
/.$putCase consumeStatementFor() ; $break ./
/:$readableName ForStatement:/

ForStatementNoShortIf ::= 'for' '(' ForInitopt ';' Expressionopt ';' ForUpdateopt ')' StatementNoShortIf
/.$putCase consumeStatementFor() ; $break ./
/:$readableName ForStatement:/

--the minus one allows to avoid a stack-to-stack transfer
ForInit ::= StatementExpressionList
/.$putCase consumeForInit() ; $break ./
ForInit -> LocalVariableDeclaration
/:$readableName ForInit:/

ForUpdate -> StatementExpressionList
/:$readableName ForUpdate:/

StatementExpressionList -> StatementExpression
StatementExpressionList ::= StatementExpressionList ',' StatementExpression
/.$putCase consumeStatementExpressionList() ; $break ./
/:$readableName StatementExpressionList:/

-- 1.4 feature
AssertStatement ::= 'assert' Expression ';'
/.$putCase consumeSimpleAssertStatement() ; $break ./
/:$compliance 1.4:/

AssertStatement ::= 'assert' Expression ':' Expression ';'
/.$putCase consumeAssertStatement() ; $break ./
/:$readableName AssertStatement:/
/:$compliance 1.4:/

BreakStatement ::= 'break' ';'
/.$putCase consumeStatementBreak() ; $break ./

BreakStatement ::= 'break' Identifier ';'
/.$putCase consumeStatementBreakWithLabel() ; $break ./
/:$readableName BreakStatement:/

ContinueStatement ::= 'continue' ';'
/.$putCase consumeStatementContinue() ; $break ./

ContinueStatement ::= 'continue' Identifier ';'
/.$putCase consumeStatementContinueWithLabel() ; $break ./
/:$readableName ContinueStatement:/

ReturnStatement ::= 'return' Expressionopt ';'
/.$putCase consumeStatementReturn() ; $break ./
/:$readableName ReturnStatement:/

ThrowStatement ::= 'throw' Expression ';'
/.$putCase consumeStatementThrow(); $break ./
/:$readableName ThrowStatement:/

ThrowExpression ::= 'throw' Expression
/.$putCase consumeThrowExpression() ; $break ./
/:$readableName ThrowExpression:/

SynchronizedStatement ::= OnlySynchronized '(' Expression ')' Block
/.$putCase consumeStatementSynchronized(); $break ./
/:$readableName SynchronizedStatement:/

OnlySynchronized ::= 'synchronized'
/.$putCase consumeOnlySynchronized(); $break ./
/:$readableName OnlySynchronized:/

TryStatement ::= 'try' TryBlock Catches
/.$putCase consumeStatementTry(false, false); $break ./
TryStatement ::= 'try' TryBlock Catchesopt Finally
/.$putCase consumeStatementTry(true, false); $break ./
/:$readableName TryStatement:/

TryStatementWithResources ::= 'try' ResourceSpecification TryBlock Catchesopt
/.$putCase consumeStatementTry(false, true); $break ./
TryStatementWithResources ::= 'try' ResourceSpecification TryBlock Catchesopt Finally
/.$putCase consumeStatementTry(true, true); $break ./
/:$readableName TryStatementWithResources:/
/:$compliance 1.7:/

ResourceSpecification ::= '(' Resources ;opt ')'
/.$putCase consumeResourceSpecification(); $break ./
/:$readableName ResourceSpecification:/
/:$compliance 1.7:/

;opt ::= $empty
/.$putCase consumeResourceOptionalTrailingSemiColon(false); $break ./
;opt ::= ';'
/.$putCase consumeResourceOptionalTrailingSemiColon(true); $break ./
/:$readableName ;:/
/:$compliance 1.7:/

Resources ::= Resource
/.$putCase consumeSingleResource(); $break ./
Resources ::= Resources TrailingSemiColon Resource
/.$putCase consumeMultipleResources(); $break ./
/:$readableName Resources:/
/:$compliance 1.7:/

TrailingSemiColon ::= ';'
/.$putCase consumeResourceOptionalTrailingSemiColon(true); $break ./
/:$readableName ;:/
/:$compliance 1.7:/

Resource ::= Type PushModifiers VariableDeclaratorId EnterVariable '=' ForceNoDiet VariableInitializer RestoreDiet ExitVariableWithInitialization
/.$putCase consumeResourceAsLocalVariableDeclaration(); $break ./
/:$readableName Resource:/
/:$compliance 1.7:/

Resource ::= Modifiers Type PushRealModifiers VariableDeclaratorId EnterVariable '=' ForceNoDiet VariableInitializer RestoreDiet ExitVariableWithInitialization
/.$putCase consumeResourceAsLocalVariableDeclaration(); $break ./
/:$readableName Resource:/
/:$compliance 1.7:/

Resource ::= Name
/.$putCase consumeResourceAsLocalVariable(); $break ./
/:$readableName Resource:/
/:$compliance 1.9:/

Resource ::= 'this'
/.$putCase consumeResourceAsThis(); $break ./
/:$readableName Resource:/
/:$compliance 1.9:/

Resource ::= FieldAccess
/.$putCase consumeResourceAsFieldAccess(); $break ./
/:$readableName Resource:/
/:$compliance 1.9:/

TryBlock ::= Block ExitTryBlock
/:$readableName Block:/

ExitTryBlock ::= $empty
/.$putCase consumeExitTryBlock(); $break ./
/:$readableName ExitTryBlock:/

Catches -> CatchClause
Catches ::= Catches CatchClause
/.$putCase consumeCatches(); $break ./
/:$readableName Catches:/

CatchClause ::= 'catch' '(' CatchFormalParameter ')' Block
/.$putCase consumeStatementCatch() ; $break ./
/:$readableName CatchClause:/

Finally ::= 'finally' Block
/:$readableName Finally:/
/:$recovery_template finally { }:/

--18.12 Productions from 14: Expressions

--for source positioning purpose
PushLPAREN ::= '('
/.$putCase consumeLeftParen(); $break ./
/:$readableName (:/
/:$recovery_template (:/
PushRPAREN ::= ')'
/.$putCase consumeRightParen(); $break ./
/:$readableName ):/
/:$recovery_template ):/

Primary -> PrimaryNoNewArray
Primary -> ArrayCreationWithArrayInitializer
Primary -> ArrayCreationWithoutArrayInitializer
/:$readableName Expression:/

PrimaryNoNewArray -> Literal
PrimaryNoNewArray ::= 'this'
/.$putCase consumePrimaryNoNewArrayThis(); $break ./

PrimaryNoNewArray ::= PushLPAREN Expression_NotName PushRPAREN
/.$putCase consumePrimaryNoNewArray(); $break ./

PrimaryNoNewArray ::= PushLPAREN Name PushRPAREN
/.$putCase consumePrimaryNoNewArrayWithName(); $break ./

-- AspectJ Extension: Cannot replace 'Name' in previous rule with 'NameOrAj', because then the grammar would no longer
-- be LALR(1). Therefore, we have to duplicate the rule and change 'Name' to 'AjName'.
PrimaryNoNewArray ::= PushLPAREN AjName PushRPAREN
/.$putCase consumePrimaryNoNewArrayWithName(); $break ./
-- End AspectJ Extension

PrimaryNoNewArray -> ClassInstanceCreationExpression
PrimaryNoNewArray -> FieldAccess
--1.1 feature
PrimaryNoNewArray ::= Name '.' 'this'
/.$putCase consumePrimaryNoNewArrayNameThis(); $break ./

QualifiedSuperReceiver ::= Name '.' 'super'
/.$putCase consumeQualifiedSuperReceiver(); $break ./

--1.1 feature
--PrimaryNoNewArray ::= Type '.' 'class'
--inline Type in the previous rule in order to make the grammar LL1 instead
-- of LL2. The result is the 3 next rules.

PrimaryNoNewArray ::= Name '.' 'class'
/.$putCase consumePrimaryNoNewArrayName(); $break ./

PrimaryNoNewArray ::= Name Dims '.' 'class'
/.$putCase consumePrimaryNoNewArrayArrayType(); $break ./

PrimaryNoNewArray ::= PrimitiveType Dims '.' 'class'
/.$putCase consumePrimaryNoNewArrayPrimitiveArrayType(); $break ./

PrimaryNoNewArray ::= PrimitiveType '.' 'class'
/.$putCase consumePrimaryNoNewArrayPrimitiveType(); $break ./

PrimaryNoNewArray -> MethodInvocation
PrimaryNoNewArray -> ArrayAccess

-----------------------------------------------------------------------
--                   Start of rules for JSR 335
-----------------------------------------------------------------------

PrimaryNoNewArray -> LambdaExpression
PrimaryNoNewArray -> ReferenceExpression
/:$readableName Expression:/

-- Production name hardcoded in parser. Must be ::= and not ->
ReferenceExpressionTypeArgumentsAndTrunk ::= ReferenceExpressionTypeArgumentsAndTrunk0
/:$readableName ReferenceExpressionTypeArgumentsAndTrunk:/

ReferenceExpressionTypeArgumentsAndTrunk0 ::= OnlyTypeArguments Dimsopt
/.$putCase consumeReferenceExpressionTypeArgumentsAndTrunk(false); $break ./
/:$compliance 1.8:/
ReferenceExpressionTypeArgumentsAndTrunk0 ::= OnlyTypeArguments '.' ClassOrInterfaceType Dimsopt
/.$putCase consumeReferenceExpressionTypeArgumentsAndTrunk(true); $break ./
/:$readableName ReferenceExpressionTypeArgumentsAndTrunk:/
/:$compliance 1.8:/

ReferenceExpression ::= PrimitiveType Dims '::' NonWildTypeArgumentsopt IdentifierOrNew
/.$putCase consumeReferenceExpressionTypeForm(true); $break ./
/:$compliance 1.8:/

ReferenceExpression ::= Name Dimsopt '::' NonWildTypeArgumentsopt IdentifierOrNew
/.$putCase consumeReferenceExpressionTypeForm(false); $break ./
/:$compliance 1.8:/

-- BeginTypeArguments is a synthetic token the scanner concocts to help disambiguate
-- between '<' as an operator and '<' in '<' TypeArguments '>'
ReferenceExpression ::= Name BeginTypeArguments ReferenceExpressionTypeArgumentsAndTrunk '::' NonWildTypeArgumentsopt IdentifierOrNew
/.$putCase consumeReferenceExpressionGenericTypeForm(); $break ./
/:$compliance 1.8:/

ReferenceExpression ::= Primary '::' NonWildTypeArgumentsopt Identifier
/.$putCase consumeReferenceExpressionPrimaryForm(); $break ./
/:$compliance 1.8:/
ReferenceExpression ::= QualifiedSuperReceiver '::' NonWildTypeArgumentsopt Identifier
/.$putCase consumeReferenceExpressionPrimaryForm(); $break ./
/:$compliance 1.8:/
ReferenceExpression ::= 'super' '::' NonWildTypeArgumentsopt Identifier
/.$putCase consumeReferenceExpressionSuperForm(); $break ./
/:$readableName ReferenceExpression:/
/:$compliance 1.8:/

NonWildTypeArgumentsopt ::= $empty
/.$putCase consumeEmptyTypeArguments(); $break ./
NonWildTypeArgumentsopt -> OnlyTypeArguments
/:$readableName NonWildTypeArgumentsopt:/
/:$compliance 1.8:/

IdentifierOrNew ::= 'Identifier'
/.$putCase consumeIdentifierOrNew(false); $break ./
IdentifierOrNew ::= 'new'
/.$putCase consumeIdentifierOrNew(true); $break ./
/:$readableName IdentifierOrNew:/
/:$compliance 1.8:/

LambdaExpression ::= LambdaParameters '->' LambdaBody
/.$putCase consumeLambdaExpression(); $break ./
/:$readableName LambdaExpression:/
/:$compliance 1.8:/

NestedLambda ::= $empty
/.$putCase consumeNestedLambda(); $break ./
/:$readableName NestedLambda:/

LambdaParameters ::= '_' NestedLambda
/.$putCase consumeTypeElidedLambdaParameter(false); $break ./
/:$readableName TypeElidedUnnamedFormalParameter:/
/:$compliance 21:/

LambdaParameters ::= Identifier NestedLambda
/.$putCase consumeTypeElidedLambdaParameter(false); $break ./
/:$readableName TypeElidedFormalParameter:/
/:$compliance 1.8:/

-- to make the grammar LALR(1), the scanner transforms the input string to
-- contain synthetic tokens to signal start of lambda parameter list.
LambdaParameters -> BeginLambda NestedLambda LambdaParameterList
/:$readableName LambdaParameters:/
/:$compliance 1.8:/

-- Production name hardcoded in parser. Must be ::= and not ->
ParenthesizedLambdaParameterList ::= LambdaParameterList
/:$readableName ParenthesizedLambdaParameterList:/

LambdaParameterList -> PushLPAREN FormalParameterListopt PushRPAREN
LambdaParameterList -> PushLPAREN TypeElidedFormalParameterList PushRPAREN
/:$readableName LambdaParameterList:/
/:$compliance 1.8:/

TypeElidedFormalParameterList -> TypeElidedFormalParameter
TypeElidedFormalParameterList ::= TypeElidedFormalParameterList ',' TypeElidedFormalParameter
/.$putCase consumeFormalParameterList(); $break ./
/:$readableName TypeElidedFormalParameterList:/
/:$compliance 1.8:/

-- to work around a shift reduce conflict, we accept Modifiersopt prefixed
-- identifier - downstream phases should reject input strings with modifiers.
TypeElidedFormalParameter ::= Modifiersopt Identifier
/.$putCase consumeTypeElidedLambdaParameter(true); $break ./
/:$readableName TypeElidedFormalParameter:/
/:$compliance 1.8:/

TypeElidedFormalParameter ::= '_'
/.$putCase consumeBracketedTypeElidedUnderscoreLambdaParameter(); $break ./
/:$readableName TypeElidedFormalParameter:/
/:$compliance 21:/

-- A lambda body of the form x is really '{' return x; '}'
LambdaBody -> ElidedLeftBraceAndReturn Expression ElidedSemicolonAndRightBrace
LambdaBody -> Block
/:$readableName LambdaBody:/
/:$compliance 1.8:/

ElidedLeftBraceAndReturn ::= $empty
/.$putCase consumeElidedLeftBraceAndReturn(); $break ./
/:$readableName ElidedLeftBraceAndReturn:/
/:$compliance 1.8:/

-----------------------------------------------------------------------
--                   End of rules for JSR 335
-----------------------------------------------------------------------

--1.1 feature
--
-- In Java 1.0 a ClassBody could not appear at all in a
-- ClassInstanceCreationExpression.
--

AllocationHeader ::= 'new' ClassType '(' ArgumentListopt ')'
/.$putCase consumeAllocationHeader(); $break ./
/:$readableName AllocationHeader:/

ClassInstanceCreationExpression ::= 'new' OnlyTypeArguments ClassType EnterInstanceCreationArgumentList '(' ArgumentListopt ')' UnqualifiedClassBodyopt
/.$putCase consumeClassInstanceCreationExpressionWithTypeArguments(); $break ./

ClassInstanceCreationExpression ::= 'new' ClassType EnterInstanceCreationArgumentList '(' ArgumentListopt ')' UnqualifiedClassBodyopt
/.$putCase consumeClassInstanceCreationExpression(); $break ./
--1.1 feature

ClassInstanceCreationExpression ::= Primary '.' 'new' OnlyTypeArguments ClassType EnterInstanceCreationArgumentList '(' ArgumentListopt ')' QualifiedClassBodyopt
/.$putCase consumeClassInstanceCreationExpressionQualifiedWithTypeArguments() ; $break ./

ClassInstanceCreationExpression ::= Primary '.' 'new' ClassType EnterInstanceCreationArgumentList '(' ArgumentListopt ')' QualifiedClassBodyopt
/.$putCase consumeClassInstanceCreationExpressionQualified() ; $break ./

--1.1 feature
ClassInstanceCreationExpression ::= ClassInstanceCreationExpressionName ClassType EnterInstanceCreationArgumentList '(' ArgumentListopt ')' QualifiedClassBodyopt
/.$putCase consumeClassInstanceCreationExpressionQualified() ; $break ./
/:$readableName ClassInstanceCreationExpression:/

ClassInstanceCreationExpression ::= ClassInstanceCreationExpressionName OnlyTypeArguments ClassType EnterInstanceCreationArgumentList '(' ArgumentListopt ')' QualifiedClassBodyopt
/.$putCase consumeClassInstanceCreationExpressionQualifiedWithTypeArguments() ; $break ./
/:$readableName ClassInstanceCreationExpression:/

EnterInstanceCreationArgumentList ::= $empty
/.$putCase consumeEnterInstanceCreationArgumentList(); $break ./
/:$readableName EnterInstanceCreationArgumentList:/

ClassInstanceCreationExpressionName ::= Name '.' 'new'
/.$putCase consumeClassInstanceCreationExpressionName() ; $break ./
/:$readableName ClassInstanceCreationExpressionName:/

UnqualifiedClassBodyopt ::= $empty --test made using null as contents
/.$putCase consumeClassBodyopt(); $break ./
UnqualifiedClassBodyopt ::= UnqualifiedEnterAnonymousClassBody ClassBody
/:$readableName ClassBody:/
/:$no_statements_recovery:/

UnqualifiedEnterAnonymousClassBody ::= $empty
/.$putCase consumeEnterAnonymousClassBody(false); $break ./
/:$readableName EnterAnonymousClassBody:/

QualifiedClassBodyopt ::= $empty --test made using null as contents
/.$putCase consumeClassBodyopt(); $break ./
QualifiedClassBodyopt ::= QualifiedEnterAnonymousClassBody ClassBody
/:$readableName ClassBody:/
/:$no_statements_recovery:/

QualifiedEnterAnonymousClassBody ::= $empty
/.$putCase consumeEnterAnonymousClassBody(true); $break ./
/:$readableName EnterAnonymousClassBody:/

ArgumentList ::= Expression
ArgumentList ::= ArgumentList ',' Expression
/.$putCase consumeArgumentList(); $break ./
/:$readableName ArgumentList:/

ArrayCreationHeader ::= 'new' PrimitiveType DimWithOrWithOutExprs
/.$putCase consumeArrayCreationHeader(); $break ./

ArrayCreationHeader ::= 'new' ClassOrInterfaceType DimWithOrWithOutExprs
/.$putCase consumeArrayCreationHeader(); $break ./
/:$readableName ArrayCreationHeader:/

ArrayCreationWithoutArrayInitializer ::= 'new' PrimitiveType DimWithOrWithOutExprs
/.$putCase consumeArrayCreationExpressionWithoutInitializer(); $break ./
/:$readableName ArrayCreationWithoutArrayInitializer:/

ArrayCreationWithArrayInitializer ::= 'new' PrimitiveType DimWithOrWithOutExprs ArrayInitializer
/.$putCase consumeArrayCreationExpressionWithInitializer(); $break ./
/:$readableName ArrayCreationWithArrayInitializer:/

ArrayCreationWithoutArrayInitializer ::= 'new' ClassOrInterfaceType DimWithOrWithOutExprs
/.$putCase consumeArrayCreationExpressionWithoutInitializer(); $break ./

ArrayCreationWithArrayInitializer ::= 'new' ClassOrInterfaceType DimWithOrWithOutExprs ArrayInitializer
/.$putCase consumeArrayCreationExpressionWithInitializer(); $break ./

DimWithOrWithOutExprs ::= DimWithOrWithOutExpr
DimWithOrWithOutExprs ::= DimWithOrWithOutExprs DimWithOrWithOutExpr
/.$putCase consumeDimWithOrWithOutExprs(); $break ./
/:$readableName Dimensions:/

DimWithOrWithOutExpr ::= TypeAnnotationsopt '[' Expression ']'
DimWithOrWithOutExpr ::= TypeAnnotationsopt '[' ']'
/. $putCase consumeDimWithOrWithOutExpr(); $break ./
/:$readableName Dimension:/
-- -----------------------------------------------

Dims ::= DimsLoop
/. $putCase consumeDims(); $break ./
/:$readableName Dimensions:/
DimsLoop -> OneDimLoop
DimsLoop ::= DimsLoop OneDimLoop
/:$readableName Dimensions:/
OneDimLoop ::= '[' ']'
/. $putCase consumeOneDimLoop(false); $break ./
OneDimLoop ::= TypeAnnotations '[' ']'
/:$compliance 1.8:/
/. $putCase consumeOneDimLoop(true); $break ./
/:$readableName Dimension:/

FieldAccess ::= Primary '.' JavaIdentifier -- AspectJ Extension, was 'Identifier'
/.$putCase consumeFieldAccess(false); $break ./

FieldAccess ::= 'super' '.' JavaIdentifier -- AspectJ Extension, was 'Identifier'
/.$putCase consumeFieldAccess(true); $break ./
/:$readableName FieldAccess:/

FieldAccess ::= QualifiedSuperReceiver '.' JavaIdentifier -- AspectJ Extension, was 'Identifier'
/.$putCase consumeFieldAccess(false); $break ./
/:$readableName FieldAccess:/

MethodInvocation ::= NameOrAj '(' ArgumentListopt ')' -- AspectJ Extension, was Name
/.$putCase consumeMethodInvocationName(); $break ./

MethodInvocation ::= Name '.' OnlyTypeArguments JavaIdentifier '(' ArgumentListopt ')'  -- AspectJ Extension, 'Identifier'
/.$putCase consumeMethodInvocationNameWithTypeArguments(); $break ./

MethodInvocation ::= Primary '.' OnlyTypeArguments JavaIdentifier '(' ArgumentListopt ')' -- AspectJ Extension, 'Identifier'
/.$putCase consumeMethodInvocationPrimaryWithTypeArguments(); $break ./

MethodInvocation ::= Primary '.' JavaIdentifier '(' ArgumentListopt ')' -- AspectJ Extension, 'Identifier'
/.$putCase consumeMethodInvocationPrimary(); $break ./

MethodInvocation ::= QualifiedSuperReceiver '.' JavaIdentifier '(' ArgumentListopt ')' -- AspectJ JavaIdentifier was 'Identifier'
/.$putCase consumeMethodInvocationPrimary(); $break ./

MethodInvocation ::= QualifiedSuperReceiver '.' OnlyTypeArguments JavaIdentifier '(' ArgumentListopt ')' -- AspectJ JavaIdentifier was 'Identifier'
/.$putCase consumeMethodInvocationPrimaryWithTypeArguments(); $break ./

MethodInvocation ::= 'super' '.' OnlyTypeArguments JavaIdentifier '(' ArgumentListopt ')' -- AspectJ Extension, 'Identifier'
/.$putCase consumeMethodInvocationSuperWithTypeArguments(); $break ./

MethodInvocation ::= 'super' '.' JavaIdentifier '(' ArgumentListopt ')' -- AspectJ Extension, 'Identifier'
/.$putCase consumeMethodInvocationSuper(); $break ./
/:$readableName MethodInvocation:/

-- (pr159268)
ArrayAccess ::= Name '[' Expression ']'
/.$putCase consumeArrayAccess(true); $break ./
ArrayAccess ::= AjName '[' Expression ']'
/.$putCase consumeArrayAccess(true); $break ./
ArrayAccess ::= PrimaryNoNewArray '[' Expression ']'
/.$putCase consumeArrayAccess(false); $break ./
ArrayAccess ::= ArrayCreationWithArrayInitializer '[' Expression ']'
/.$putCase consumeArrayAccess(false); $break ./
/:$readableName ArrayAccess:/

PostfixExpression -> Primary
PostfixExpression ::= NameOrAj -- AspectJ Extension, was Name
/.$putCase consumePostfixExpression(); $break ./
PostfixExpression -> PostIncrementExpression
PostfixExpression -> PostDecrementExpression
/:$readableName Expression:/

PostIncrementExpression ::= PostfixExpression '++'
/.$putCase consumeUnaryExpression(OperatorIds.PLUS,true); $break ./
/:$readableName PostIncrementExpression:/

PostDecrementExpression ::= PostfixExpression '--'
/.$putCase consumeUnaryExpression(OperatorIds.MINUS,true); $break ./
/:$readableName PostDecrementExpression:/

--for source managment purpose
PushPosition ::= $empty
 /.$putCase consumePushPosition(); $break ./
/:$readableName PushPosition:/

UnaryExpression -> PreIncrementExpression
UnaryExpression -> PreDecrementExpression
UnaryExpression ::= '+' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.PLUS); $break ./
UnaryExpression ::= '-' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.MINUS); $break ./
UnaryExpression -> UnaryExpressionNotPlusMinus
/:$readableName Expression:/

PreIncrementExpression ::= '++' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.PLUS,false); $break ./
/:$readableName PreIncrementExpression:/

PreDecrementExpression ::= '--' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.MINUS,false); $break ./
/:$readableName PreDecrementExpression:/

UnaryExpressionNotPlusMinus -> PostfixExpression
UnaryExpressionNotPlusMinus ::= '~' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.TWIDDLE); $break ./
UnaryExpressionNotPlusMinus ::= '!' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.NOT); $break ./
UnaryExpressionNotPlusMinus -> CastExpression
/:$readableName Expression:/

CastExpression ::= PushLPAREN PrimitiveType Dimsopt AdditionalBoundsListOpt PushRPAREN InsideCastExpression UnaryExpression
/.$putCase consumeCastExpressionWithPrimitiveType(); $break ./
CastExpression ::= PushLPAREN Name OnlyTypeArgumentsForCastExpression Dimsopt AdditionalBoundsListOpt PushRPAREN InsideCastExpression UnaryExpressionNotPlusMinus
/.$putCase consumeCastExpressionWithGenericsArray(); $break ./
CastExpression ::= PushLPAREN Name OnlyTypeArgumentsForCastExpression '.' ClassOrInterfaceType Dimsopt AdditionalBoundsListOpt PushRPAREN InsideCastExpressionWithQualifiedGenerics UnaryExpressionNotPlusMinus
/.$putCase consumeCastExpressionWithQualifiedGenericsArray(); $break ./
CastExpression ::= PushLPAREN Name PushRPAREN InsideCastExpressionLL1 UnaryExpressionNotPlusMinus
/.$putCase consumeCastExpressionLL1(); $break ./
CastExpression ::=  BeginIntersectionCast PushLPAREN CastNameAndBounds PushRPAREN InsideCastExpressionLL1WithBounds UnaryExpressionNotPlusMinus
/.$putCase consumeCastExpressionLL1WithBounds(); $break ./
CastExpression ::= PushLPAREN Name Dims AdditionalBoundsListOpt PushRPAREN InsideCastExpression UnaryExpressionNotPlusMinus
/.$putCase consumeCastExpressionWithNameArray(); $break ./
/:$readableName CastExpression:/

AdditionalBoundsListOpt ::= $empty
/.$putCase consumeZeroAdditionalBounds(); $break ./
/:$readableName AdditionalBoundsListOpt:/
AdditionalBoundsListOpt -> AdditionalBoundList
/:$compliance 1.8:/
/:$readableName AdditionalBoundsListOpt:/

-- Production name hardcoded in parser. Must be ::= and not ->
ParenthesizedCastNameAndBounds ::= '(' CastNameAndBounds ')'
/:$readableName ParenthesizedCastNameAndBounds:/

CastNameAndBounds -> Name AdditionalBoundList
/:$compliance 1.8:/
/:$readableName CastNameAndBounds:/

OnlyTypeArgumentsForCastExpression ::= OnlyTypeArguments
/.$putCase consumeOnlyTypeArgumentsForCastExpression(); $break ./
/:$readableName TypeArguments:/

InsideCastExpression ::= $empty
/.$putCase consumeInsideCastExpression(); $break ./
/:$readableName InsideCastExpression:/
InsideCastExpressionLL1 ::= $empty
/.$putCase consumeInsideCastExpressionLL1(); $break ./
/:$readableName InsideCastExpression:/
InsideCastExpressionLL1WithBounds ::= $empty
/.$putCase consumeInsideCastExpressionLL1WithBounds (); $break ./
/:$readableName InsideCastExpression:/
InsideCastExpressionWithQualifiedGenerics ::= $empty
/.$putCase consumeInsideCastExpressionWithQualifiedGenerics(); $break ./
/:$readableName InsideCastExpression:/

MultiplicativeExpression -> UnaryExpression
MultiplicativeExpression ::= MultiplicativeExpression '*' UnaryExpression
/.$putCase consumeBinaryExpression(OperatorIds.MULTIPLY); $break ./
MultiplicativeExpression ::= MultiplicativeExpression '/' UnaryExpression
/.$putCase consumeBinaryExpression(OperatorIds.DIVIDE); $break ./
MultiplicativeExpression ::= MultiplicativeExpression '%' UnaryExpression
/.$putCase consumeBinaryExpression(OperatorIds.REMAINDER); $break ./
/:$readableName Expression:/

AdditiveExpression -> MultiplicativeExpression
AdditiveExpression ::= AdditiveExpression '+' MultiplicativeExpression
/.$putCase consumeBinaryExpression(OperatorIds.PLUS); $break ./
AdditiveExpression ::= AdditiveExpression '-' MultiplicativeExpression
/.$putCase consumeBinaryExpression(OperatorIds.MINUS); $break ./
/:$readableName Expression:/

ShiftExpression -> AdditiveExpression
ShiftExpression ::= ShiftExpression '<<' AdditiveExpression
/.$putCase consumeBinaryExpression(OperatorIds.LEFT_SHIFT); $break ./
ShiftExpression ::= ShiftExpression '>>' AdditiveExpression
/.$putCase consumeBinaryExpression(OperatorIds.RIGHT_SHIFT); $break ./
ShiftExpression ::= ShiftExpression '>>>' AdditiveExpression
/.$putCase consumeBinaryExpression(OperatorIds.UNSIGNED_RIGHT_SHIFT); $break ./
/:$readableName Expression:/

RelationalExpression -> ShiftExpression
RelationalExpression ::= RelationalExpression '<' ShiftExpression
/.$putCase consumeBinaryExpression(OperatorIds.LESS); $break ./
RelationalExpression ::= RelationalExpression '>' ShiftExpression
/.$putCase consumeBinaryExpression(OperatorIds.GREATER); $break ./
RelationalExpression ::= RelationalExpression '<=' ShiftExpression
/.$putCase consumeBinaryExpression(OperatorIds.LESS_EQUAL); $break ./
RelationalExpression ::= RelationalExpression '>=' ShiftExpression
/.$putCase consumeBinaryExpression(OperatorIds.GREATER_EQUAL); $break ./
/:$readableName Expression:/

EqualityExpression -> InstanceofExpression
EqualityExpression ::= EqualityExpression '==' InstanceofExpression
/.$putCase consumeEqualityExpression(OperatorIds.EQUAL_EQUAL); $break ./
EqualityExpression ::= EqualityExpression '!=' InstanceofExpression
/.$putCase consumeEqualityExpression(OperatorIds.NOT_EQUAL); $break ./
/:$readableName Expression:/

AndExpression -> EqualityExpression
AndExpression ::= AndExpression '&' EqualityExpression
/.$putCase consumeBinaryExpression(OperatorIds.AND); $break ./
/:$readableName Expression:/

ExclusiveOrExpression -> AndExpression
ExclusiveOrExpression ::= ExclusiveOrExpression '^' AndExpression
/.$putCase consumeBinaryExpression(OperatorIds.XOR); $break ./
/:$readableName Expression:/

InclusiveOrExpression -> ExclusiveOrExpression
InclusiveOrExpression ::= InclusiveOrExpression '|' ExclusiveOrExpression
/.$putCase consumeBinaryExpression(OperatorIds.OR); $break ./
/:$readableName Expression:/

ConditionalAndExpression -> InclusiveOrExpression
ConditionalAndExpression ::= ConditionalAndExpression '&&' InclusiveOrExpression
/.$putCase consumeBinaryExpression(OperatorIds.AND_AND); $break ./
/:$readableName Expression:/

ConditionalOrExpression -> ConditionalAndExpression
ConditionalOrExpression ::= ConditionalOrExpression '||' ConditionalAndExpression
/.$putCase consumeBinaryExpression(OperatorIds.OR_OR); $break ./
/:$readableName Expression:/

ConditionalExpression -> ConditionalOrExpression
ConditionalExpression ::= ConditionalOrExpression '?' Expression ':' ConditionalExpression
/.$putCase consumeConditionalExpression(OperatorIds.QUESTIONCOLON) ; $break ./
/:$readableName Expression:/

AssignmentExpression -> ConditionalExpression
AssignmentExpression -> Assignment
/:$readableName Expression:/
/:$recovery_template JavaIdentifier:/ -- AspectJ extension, was Identifier

Assignment ::= PostfixExpression AssignmentOperator AssignmentExpression
/.$putCase consumeAssignment(); $break ./
/:$readableName Assignment:/

-- this rule is added to parse an array initializer in a assigment and then report a syntax error knowing the exact senario
InvalidArrayInitializerAssignement ::= PostfixExpression AssignmentOperator ArrayInitializer
/:$readableName ArrayInitializerAssignment:/
/:$recovery:/
Assignment ::= InvalidArrayInitializerAssignement
/.$putcase ignoreExpressionAssignment();$break ./
/:$recovery:/

AssignmentOperator ::= '='
/.$putCase consumeAssignmentOperator(EQUAL); $break ./
AssignmentOperator ::= '*='
/.$putCase consumeAssignmentOperator(MULTIPLY); $break ./
AssignmentOperator ::= '/='
/.$putCase consumeAssignmentOperator(DIVIDE); $break ./
AssignmentOperator ::= '%='
/.$putCase consumeAssignmentOperator(REMAINDER); $break ./
AssignmentOperator ::= '+='
/.$putCase consumeAssignmentOperator(PLUS); $break ./
AssignmentOperator ::= '-='
/.$putCase consumeAssignmentOperator(MINUS); $break ./
AssignmentOperator ::= '<<='
/.$putCase consumeAssignmentOperator(LEFT_SHIFT); $break ./
AssignmentOperator ::= '>>='
/.$putCase consumeAssignmentOperator(RIGHT_SHIFT); $break ./
AssignmentOperator ::= '>>>='
/.$putCase consumeAssignmentOperator(UNSIGNED_RIGHT_SHIFT); $break ./
AssignmentOperator ::= '&='
/.$putCase consumeAssignmentOperator(AND); $break ./
AssignmentOperator ::= '^='
/.$putCase consumeAssignmentOperator(XOR); $break ./
AssignmentOperator ::= '|='
/.$putCase consumeAssignmentOperator(OR); $break ./
/:$readableName AssignmentOperator:/
/:$recovery_template =:/

-- For handling lambda expressions, we need to know when a full Expression
-- has been reduced.
Expression ::= AssignmentExpression
/.$putCase consumeExpression(); $break ./
/:$readableName Expression:/
/:$recovery_template JavaIdentifier:/ -- AspectJ extension, was Identifier

-- The following rules are for optional nonterminals.
--
ClassHeaderExtendsopt ::= $empty
ClassHeaderExtendsopt -> ClassHeaderExtends
/:$readableName ClassHeaderExtends:/

Expressionopt ::= $empty
/.$putCase consumeEmptyExpression(); $break ./
Expressionopt -> Expression
/:$readableName Expression:/

ConstantExpression -> Expression
/:$readableName ConstantExpression:/

---------------------------------------------------------------------------------------
--
-- The rules below are for optional terminal symbols.  An optional comma,
-- is only used in the context of an array initializer - It is a
-- "syntactic sugar" that otherwise serves no other purpose. By contrast,
-- an optional identifier is used in the definition of a break and
-- continue statement. When the identifier does not appear, a NULL
-- is produced. When the identifier is present, the user should use the
-- corresponding TOKEN(i) method. See break statement as an example.
--
---------------------------------------------------------------------------------------

,opt -> $empty
,opt -> ,
/:$readableName ,:/

ClassBodyDeclarationsopt ::= $empty
/.$putCase consumeEmptyClassBodyDeclarationsopt(); $break ./
ClassBodyDeclarationsopt ::= NestedType ClassBodyDeclarations
/.$putCase consumeClassBodyDeclarationsopt(); $break ./
/:$readableName ClassBodyDeclarations:/

Modifiersopt ::= $empty
/. $putCase consumeDefaultModifiers(); $break ./
Modifiersopt ::= Modifiers
/.$putCase consumeModifiers(); $break ./
/:$readableName Modifiers:/

BlockStatementsopt ::= $empty
/.$putCase consumeEmptyBlockStatementsopt(); $break ./
BlockStatementsopt -> BlockStatements
/:$readableName BlockStatements:/

Dimsopt ::= $empty
/. $putCase consumeEmptyDimsopt(); $break ./
Dimsopt -> Dims
/:$readableName Dimensions:/

ArgumentListopt ::= $empty
/. $putCase consumeEmptyArgumentListopt(); $break ./
ArgumentListopt -> ArgumentList
/:$readableName ArgumentList:/

MethodHeaderThrowsClauseopt ::= $empty
MethodHeaderThrowsClauseopt -> MethodHeaderThrowsClause
/:$readableName MethodHeaderThrowsClause:/

FormalParameterListopt ::= $empty
/.$putcase consumeFormalParameterListopt(); $break ./
FormalParameterListopt -> FormalParameterList
/:$readableName FormalParameterList:/

ClassHeaderImplementsopt ::= $empty
ClassHeaderImplementsopt -> ClassHeaderImplements
/:$readableName ClassHeaderImplements:/

ClassHeaderPermittedSubclassesopt ::= $empty
ClassHeaderPermittedSubclassesopt -> ClassHeaderPermittedSubclasses
/:$readableName ClassHeaderPermittedSubclasses:/
/:$compliance 15:/

-- Production name hardcoded in parser. Must be ::= and not ->
PermittedSubclasses ::= ClassTypeList
/:$readableName PermittedSubclasses:/

ClassHeaderPermittedSubclasses ::= RestrictedIdentifierpermits ClassTypeList
/.$putCase consumeClassHeaderPermittedSubclasses(); $break ./
/:$readableName ClassHeaderPermittedSubclasses:/
/:$compliance 15:/

InterfaceHeaderPermittedSubClassesAndSubInterfacesopt ::= $empty
InterfaceHeaderPermittedSubClassesAndSubInterfacesopt -> InterfaceHeaderPermittedSubClassesAndSubInterfaces
/:$readableName InterfaceHeaderPermittedSubClassesAndSubInterfaces:/
/:$compliance 15:/

InterfaceHeaderPermittedSubClassesAndSubInterfaces ::= RestrictedIdentifierpermits ClassTypeList
/.$putCase consumeInterfaceHeaderPermittedSubClassesAndSubInterfaces(); $break ./
/:$readableName InterfaceHeaderPermittedSubClassesAndSubInterfaces:/
/:$compliance 15:/

InterfaceMemberDeclarationsopt ::= $empty
/. $putCase consumeEmptyInterfaceMemberDeclarationsopt(); $break ./
InterfaceMemberDeclarationsopt ::= NestedType InterfaceMemberDeclarations
/. $putCase consumeInterfaceMemberDeclarationsopt(); $break ./
/:$readableName InterfaceMemberDeclarations:/

NestedType ::= $empty
/.$putCase consumeNestedType(); $break./
/:$readableName NestedType:/

ForInitopt ::= $empty
/. $putCase consumeEmptyForInitopt(); $break ./
ForInitopt -> ForInit
/:$readableName ForInit:/

ForUpdateopt ::= $empty
/. $putCase consumeEmptyForUpdateopt(); $break ./
ForUpdateopt -> ForUpdate
/:$readableName ForUpdate:/

InterfaceHeaderExtendsopt ::= $empty
InterfaceHeaderExtendsopt -> InterfaceHeaderExtends
/:$readableName InterfaceHeaderExtends:/

Catchesopt ::= $empty
/. $putCase consumeEmptyCatchesopt(); $break ./
Catchesopt -> Catches
/:$readableName Catches:/

-----------------------------------------------
-- 1.5 features : enum type
-----------------------------------------------
EnumDeclaration ::= EnumHeader EnumBody
/. $putCase consumeEnumDeclaration(); $break ./
/:$readableName EnumDeclaration:/

EnumHeader ::= EnumHeaderName ClassHeaderImplementsopt
/. $putCase consumeEnumHeader(); $break ./
/:$readableName EnumHeader:/

EnumHeaderName ::= Modifiersopt 'enum' JavaIdentifier -- AspectJ Extension, was Identifier
/. $putCase consumeEnumHeaderName(); $break ./
/:$compliance 1.5:/
EnumHeaderName ::= Modifiersopt 'enum' JavaIdentifier TypeParameters -- AspectJ Extension, was Identifier
/. $putCase consumeEnumHeaderNameWithTypeParameters(); $break ./
/:$readableName EnumHeaderName:/
/:$compliance 1.5:/

EnumBody ::= '{' EnumBodyDeclarationsopt '}'
/. $putCase consumeEnumBodyNoConstants(); $break ./
EnumBody ::= '{' ',' EnumBodyDeclarationsopt '}'
/. $putCase consumeEnumBodyNoConstants(); $break ./
EnumBody ::= '{' EnumConstants ',' EnumBodyDeclarationsopt '}'
/. $putCase consumeEnumBodyWithConstants(); $break ./
EnumBody ::= '{' EnumConstants EnumBodyDeclarationsopt '}'
/. $putCase consumeEnumBodyWithConstants(); $break ./
/:$readableName EnumBody:/

EnumConstants -> EnumConstant
EnumConstants ::= EnumConstants ',' EnumConstant
/.$putCase consumeEnumConstants(); $break ./
/:$readableName EnumConstants:/

EnumConstantHeaderName ::= Modifiersopt Identifier
/.$putCase consumeEnumConstantHeaderName(); $break ./
/:$readableName EnumConstantHeaderName:/

EnumConstantHeader ::= EnumConstantHeaderName ForceNoDiet Argumentsopt RestoreDiet
/.$putCase consumeEnumConstantHeader(); $break ./
/:$readableName EnumConstantHeader:/

EnumConstant ::= EnumConstantHeader ForceNoDiet ClassBody RestoreDiet
/.$putCase consumeEnumConstantWithClassBody(); $break ./
EnumConstant ::= EnumConstantHeader
/.$putCase consumeEnumConstantNoClassBody(); $break ./
/:$readableName EnumConstant:/

Arguments ::= '(' ArgumentListopt ')'
/.$putCase consumeArguments(); $break ./
/:$readableName Arguments:/

Argumentsopt ::= $empty
/.$putCase consumeEmptyArguments(); $break ./
Argumentsopt -> Arguments
/:$readableName Argumentsopt:/

EnumDeclarations ::= ';' ClassBodyDeclarationsopt
/.$putCase consumeEnumDeclarations(); $break ./
/:$readableName EnumDeclarations:/

EnumBodyDeclarationsopt ::= $empty
/.$putCase consumeEmptyEnumDeclarations(); $break ./
EnumBodyDeclarationsopt -> EnumDeclarations
/:$readableName EnumBodyDeclarationsopt:/

-----------------------------------------------
-- 1.5 features : enhanced for statement
-----------------------------------------------
EnhancedForStatement ::= EnhancedForStatementHeader Statement
/.$putCase consumeEnhancedForStatement(); $break ./
/:$readableName EnhancedForStatement:/

EnhancedForStatementNoShortIf ::= EnhancedForStatementHeader StatementNoShortIf
/.$putCase consumeEnhancedForStatement(); $break ./
/:$readableName EnhancedForStatementNoShortIf:/

EnhancedForStatementHeaderInit ::= 'for' '(' Type PushModifiers VariableDeclaratorId
/.$putCase consumeEnhancedForStatementHeaderInit(false); $break ./
/:$readableName EnhancedForStatementHeaderInit:/

EnhancedForStatementHeaderInit ::= 'for' '(' Modifiers Type PushRealModifiers VariableDeclaratorId
/.$putCase consumeEnhancedForStatementHeaderInit(true); $break ./
/:$readableName EnhancedForStatementHeaderInit:/

EnhancedForStatementHeader ::= EnhancedForStatementHeaderInit ':' Expression ')'
/.$putCase consumeEnhancedForStatementHeader(); $break ./
/:$readableName EnhancedForStatementHeader:/
/:$compliance 1.5:/

-----------------------------------------------
-- 1.5 features : static imports
-----------------------------------------------
SingleStaticImportDeclaration ::= SingleStaticImportDeclarationName ';'
/.$putCase consumeImportDeclaration(); $break ./
/:$readableName SingleStaticImportDeclaration:/

SingleStaticImportDeclarationName ::= 'import' 'static' Name RejectTypeAnnotations
/.$putCase consumeSingleStaticImportDeclarationName(); $break ./
/:$readableName SingleStaticImportDeclarationName:/
/:$compliance 1.5:/

StaticImportOnDemandDeclaration ::= StaticImportOnDemandDeclarationName ';'
/.$putCase consumeImportDeclaration(); $break ./
/:$readableName StaticImportOnDemandDeclaration:/

StaticImportOnDemandDeclarationName ::= 'import' 'static' Name '.' RejectTypeAnnotations '*'
/.$putCase consumeStaticImportOnDemandDeclarationName(); $break ./
/:$readableName StaticImportOnDemandDeclarationName:/
/:$compliance 1.5:/

-----------------------------------------------
-- 1.5 features : generics
-----------------------------------------------
TypeArguments ::= '<' TypeArgumentList1
/.$putCase consumeTypeArguments(); $break ./
/:$readableName TypeArguments:/
/:$compliance 1.5:/

OnlyTypeArguments ::= '<' TypeArgumentList1
/.$putCase consumeOnlyTypeArguments(); $break ./
/:$readableName TypeArguments:/
/:$compliance 1.5:/

TypeArgumentList1 -> TypeArgument1
/:$compliance 1.5:/
TypeArgumentList1 ::= TypeArgumentList ',' TypeArgument1
/.$putCase consumeTypeArgumentList1(); $break ./
/:$readableName TypeArgumentList1:/
/:$compliance 1.5:/

TypeArgumentList -> TypeArgument
/:$compliance 1.5:/
TypeArgumentList ::= TypeArgumentList ',' TypeArgument
/.$putCase consumeTypeArgumentList(); $break ./
/:$readableName TypeArgumentList:/
/:$compliance 1.5:/

TypeArgument ::= ReferenceType
/.$putCase consumeTypeArgument(); $break ./
/:$compliance 1.5:/
TypeArgument -> Wildcard
/:$readableName TypeArgument:/
/:$compliance 1.5:/

TypeArgument1 -> ReferenceType1
/:$compliance 1.5:/
TypeArgument1 -> Wildcard1
/:$readableName TypeArgument1:/
/:$compliance 1.5:/

ReferenceType1 ::= ReferenceType '>'
/.$putCase consumeReferenceType1(); $break ./
/:$compliance 1.5:/
ReferenceType1 ::= ClassOrInterface '<' TypeArgumentList2
/.$putCase consumeTypeArgumentReferenceType1(); $break ./
/:$readableName ReferenceType1:/
/:$compliance 1.5:/

TypeArgumentList2 -> TypeArgument2
/:$compliance 1.5:/
TypeArgumentList2 ::= TypeArgumentList ',' TypeArgument2
/.$putCase consumeTypeArgumentList2(); $break ./
/:$readableName TypeArgumentList2:/
/:$compliance 1.5:/

TypeArgument2 -> ReferenceType2
/:$compliance 1.5:/
TypeArgument2 -> Wildcard2
/:$readableName TypeArgument2:/
/:$compliance 1.5:/

ReferenceType2 ::= ReferenceType '>>'
/.$putCase consumeReferenceType2(); $break ./
/:$compliance 1.5:/
ReferenceType2 ::= ClassOrInterface '<' TypeArgumentList3
/.$putCase consumeTypeArgumentReferenceType2(); $break ./
/:$readableName ReferenceType2:/
/:$compliance 1.5:/

TypeArgumentList3 -> TypeArgument3
TypeArgumentList3 ::= TypeArgumentList ',' TypeArgument3
/.$putCase consumeTypeArgumentList3(); $break ./
/:$readableName TypeArgumentList3:/
/:$compliance 1.5:/

TypeArgument3 -> ReferenceType3
TypeArgument3 -> Wildcard3
/:$readableName TypeArgument3:/
/:$compliance 1.5:/

ReferenceType3 ::= ReferenceType '>>>'
/.$putCase consumeReferenceType3(); $break ./
/:$readableName ReferenceType3:/
/:$compliance 1.5:/

Wildcard ::= TypeAnnotationsopt '?'
/.$putCase consumeWildcard(); $break ./
/:$compliance 1.5:/
Wildcard ::= TypeAnnotationsopt '?' WildcardBounds
/.$putCase consumeWildcardWithBounds(); $break ./
/:$readableName Wildcard:/
/:$compliance 1.5:/

WildcardBounds ::= 'extends' ReferenceType
/.$putCase consumeWildcardBoundsExtends(); $break ./
/:$compliance 1.5:/
WildcardBounds ::= 'super' ReferenceType
/.$putCase consumeWildcardBoundsSuper(); $break ./
/:$readableName WildcardBounds:/
/:$compliance 1.5:/

Wildcard1 ::= TypeAnnotationsopt '?' '>'
/.$putCase consumeWildcard1(); $break ./
/:$compliance 1.5:/
Wildcard1 ::= TypeAnnotationsopt '?' WildcardBounds1
/.$putCase consumeWildcard1WithBounds(); $break ./
/:$readableName Wildcard1:/
/:$compliance 1.5:/

WildcardBounds1 ::= 'extends' ReferenceType1
/.$putCase consumeWildcardBounds1Extends(); $break ./
/:$compliance 1.5:/
WildcardBounds1 ::= 'super' ReferenceType1
/.$putCase consumeWildcardBounds1Super(); $break ./
/:$readableName WildcardBounds1:/
/:$compliance 1.5:/

Wildcard2 ::= TypeAnnotationsopt '?' '>>'
/.$putCase consumeWildcard2(); $break ./
/:$compliance 1.5:/
Wildcard2 ::= TypeAnnotationsopt '?' WildcardBounds2
/.$putCase consumeWildcard2WithBounds(); $break ./
/:$readableName Wildcard2:/
/:$compliance 1.5:/

WildcardBounds2 ::= 'extends' ReferenceType2
/.$putCase consumeWildcardBounds2Extends(); $break ./
/:$compliance 1.5:/
WildcardBounds2 ::= 'super' ReferenceType2
/.$putCase consumeWildcardBounds2Super(); $break ./
/:$readableName WildcardBounds2:/
/:$compliance 1.5:/

Wildcard3 ::= TypeAnnotationsopt '?' '>>>'
/.$putCase consumeWildcard3(); $break ./
/:$compliance 1.5:/
Wildcard3 ::= TypeAnnotationsopt '?' WildcardBounds3
/.$putCase consumeWildcard3WithBounds(); $break ./
/:$readableName Wildcard3:/
/:$compliance 1.5:/

WildcardBounds3 ::= 'extends' ReferenceType3
/.$putCase consumeWildcardBounds3Extends(); $break ./
/:$compliance 1.5:/
WildcardBounds3 ::= 'super' ReferenceType3
/.$putCase consumeWildcardBounds3Super(); $break ./
/:$readableName WildcardBound3:/
/:$compliance 1.5:/

TypeParameterHeader ::= TypeAnnotationsopt JavaIdentifier -- AspectJ Extension, was 'Identifier'
/.$putCase consumeTypeParameterHeader(); $break ./
/:$readableName TypeParameter:/
/:$compliance 1.5:/

TypeParameters ::= '<' TypeParameterList1
/.$putCase consumeTypeParameters(); $break ./
/:$readableName TypeParameters:/
/:$compliance 1.5:/

TypeParameterList -> TypeParameter
/:$compliance 1.5:/
TypeParameterList ::= TypeParameterList ',' TypeParameter
/.$putCase consumeTypeParameterList(); $break ./
/:$readableName TypeParameterList:/
/:$compliance 1.5:/

TypeParameter -> TypeParameterHeader
/:$compliance 1.5:/
TypeParameter ::= TypeParameterHeader 'extends' ReferenceType
/.$putCase consumeTypeParameterWithExtends(); $break ./
/:$compliance 1.5:/
TypeParameter ::= TypeParameterHeader 'extends' ReferenceType AdditionalBoundList
/.$putCase consumeTypeParameterWithExtendsAndBounds(); $break ./
/:$readableName TypeParameter:/
/:$compliance 1.5:/

AdditionalBoundList -> AdditionalBound
/:$compliance 1.5:/
AdditionalBoundList ::= AdditionalBoundList AdditionalBound
/.$putCase consumeAdditionalBoundList(); $break ./
/:$readableName AdditionalBoundList:/

AdditionalBound ::= '&' ReferenceType
/.$putCase consumeAdditionalBound(); $break ./
/:$readableName AdditionalBound:/
/:$compliance 1.5:/

TypeParameterList1 -> TypeParameter1
/:$compliance 1.5:/
TypeParameterList1 ::= TypeParameterList ',' TypeParameter1
/.$putCase consumeTypeParameterList1(); $break ./
/:$readableName TypeParameterList1:/
/:$compliance 1.5:/

TypeParameter1 ::= TypeParameterHeader '>'
/.$putCase consumeTypeParameter1(); $break ./
/:$compliance 1.5:/
TypeParameter1 ::= TypeParameterHeader 'extends' ReferenceType1
/.$putCase consumeTypeParameter1WithExtends(); $break ./
/:$compliance 1.5:/
TypeParameter1 ::= TypeParameterHeader 'extends' ReferenceType AdditionalBoundList1
/.$putCase consumeTypeParameter1WithExtendsAndBounds(); $break ./
/:$readableName TypeParameter1:/
/:$compliance 1.5:/

AdditionalBoundList1 -> AdditionalBound1
/:$compliance 1.5:/
AdditionalBoundList1 ::= AdditionalBoundList AdditionalBound1
/.$putCase consumeAdditionalBoundList1(); $break ./
/:$readableName AdditionalBoundList1:/
/:$compliance 1.5:/

AdditionalBound1 ::= '&' ReferenceType1
/.$putCase consumeAdditionalBound1(); $break ./
/:$readableName AdditionalBound1:/
/:$compliance 1.5:/

-------------------------------------------------
-- Duplicate rules to remove ambiguity for (x) --
-------------------------------------------------
PostfixExpression_NotName -> Primary
PostfixExpression_NotName -> PostIncrementExpression
PostfixExpression_NotName -> PostDecrementExpression
/:$readableName Expression:/

UnaryExpression_NotName -> PreIncrementExpression
UnaryExpression_NotName -> PreDecrementExpression
UnaryExpression_NotName ::= '+' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.PLUS); $break ./
UnaryExpression_NotName ::= '-' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.MINUS); $break ./
UnaryExpression_NotName -> UnaryExpressionNotPlusMinus_NotName
/:$readableName Expression:/

UnaryExpressionNotPlusMinus_NotName -> PostfixExpression_NotName
UnaryExpressionNotPlusMinus_NotName ::= '~' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.TWIDDLE); $break ./
UnaryExpressionNotPlusMinus_NotName ::= '!' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.NOT); $break ./
UnaryExpressionNotPlusMinus_NotName -> CastExpression
/:$readableName Expression:/

MultiplicativeExpression_NotName -> UnaryExpression_NotName
MultiplicativeExpression_NotName ::= MultiplicativeExpression_NotName '*' UnaryExpression
/.$putCase consumeBinaryExpression(OperatorIds.MULTIPLY); $break ./
MultiplicativeExpression_NotName ::= NameOrAj '*' UnaryExpression  -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.MULTIPLY); $break ./
MultiplicativeExpression_NotName ::= MultiplicativeExpression_NotName '/' UnaryExpression
/.$putCase consumeBinaryExpression(OperatorIds.DIVIDE); $break ./
MultiplicativeExpression_NotName ::= NameOrAj '/' UnaryExpression  -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.DIVIDE); $break ./
MultiplicativeExpression_NotName ::= MultiplicativeExpression_NotName '%' UnaryExpression
/.$putCase consumeBinaryExpression(OperatorIds.REMAINDER); $break ./
MultiplicativeExpression_NotName ::= NameOrAj '%' UnaryExpression -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.REMAINDER); $break ./
/:$readableName Expression:/

AdditiveExpression_NotName -> MultiplicativeExpression_NotName
AdditiveExpression_NotName ::= AdditiveExpression_NotName '+' MultiplicativeExpression
/.$putCase consumeBinaryExpression(OperatorIds.PLUS); $break ./
AdditiveExpression_NotName ::= NameOrAj '+' MultiplicativeExpression -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.PLUS); $break ./
AdditiveExpression_NotName ::= AdditiveExpression_NotName '-' MultiplicativeExpression
/.$putCase consumeBinaryExpression(OperatorIds.MINUS); $break ./
AdditiveExpression_NotName ::= NameOrAj '-' MultiplicativeExpression -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.MINUS); $break ./
/:$readableName Expression:/

ShiftExpression_NotName -> AdditiveExpression_NotName
ShiftExpression_NotName ::= ShiftExpression_NotName '<<' AdditiveExpression
/.$putCase consumeBinaryExpression(OperatorIds.LEFT_SHIFT); $break ./
ShiftExpression_NotName ::= NameOrAj '<<' AdditiveExpression -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.LEFT_SHIFT); $break ./
ShiftExpression_NotName ::= ShiftExpression_NotName '>>' AdditiveExpression
/.$putCase consumeBinaryExpression(OperatorIds.RIGHT_SHIFT); $break ./
ShiftExpression_NotName ::= NameOrAj '>>' AdditiveExpression -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.RIGHT_SHIFT); $break ./
ShiftExpression_NotName ::= ShiftExpression_NotName '>>>' AdditiveExpression
/.$putCase consumeBinaryExpression(OperatorIds.UNSIGNED_RIGHT_SHIFT); $break ./
ShiftExpression_NotName ::= NameOrAj '>>>' AdditiveExpression -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.UNSIGNED_RIGHT_SHIFT); $break ./
/:$readableName Expression:/

RelationalExpression_NotName -> ShiftExpression_NotName
RelationalExpression_NotName ::= ShiftExpression_NotName '<' ShiftExpression
/.$putCase consumeBinaryExpression(OperatorIds.LESS); $break ./
RelationalExpression_NotName ::= Name '<' ShiftExpression
/.$putCase consumeBinaryExpressionWithName(OperatorIds.LESS); $break ./
RelationalExpression_NotName ::= ShiftExpression_NotName '>' ShiftExpression
/.$putCase consumeBinaryExpression(OperatorIds.GREATER); $break ./
RelationalExpression_NotName ::= NameOrAj '>' ShiftExpression -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.GREATER); $break ./
RelationalExpression_NotName ::= RelationalExpression_NotName '<=' ShiftExpression
/.$putCase consumeBinaryExpression(OperatorIds.LESS_EQUAL); $break ./
RelationalExpression_NotName ::= NameOrAj '<=' ShiftExpression -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.LESS_EQUAL); $break ./
RelationalExpression_NotName ::= RelationalExpression_NotName '>=' ShiftExpression
/.$putCase consumeBinaryExpression(OperatorIds.GREATER_EQUAL); $break ./
RelationalExpression_NotName ::= NameOrAj '>=' ShiftExpression -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.GREATER_EQUAL); $break ./
/:$readableName Expression:/

InstanceofExpression_NotName -> RelationalExpression_NotName
InstanceofExpression_NotName ::= NameOrAj InstanceofRHS -- AspectJ Extension was Name
/.$putCase consumeInstanceOfExpressionWithName(); $break ./
InstanceofExpression_NotName ::= InstanceofExpression_NotName InstanceofRHS
/.$putCase consumeInstanceOfExpression(); $break ./
/:$readableName Expression:/

EqualityExpression_NotName -> InstanceofExpression_NotName
EqualityExpression_NotName ::= EqualityExpression_NotName '==' InstanceofExpression
/.$putCase consumeEqualityExpression(OperatorIds.EQUAL_EQUAL); $break ./
EqualityExpression_NotName ::= NameOrAj '==' InstanceofExpression -- AspectJ Extension was Name
/.$putCase consumeEqualityExpressionWithName(OperatorIds.EQUAL_EQUAL); $break ./
EqualityExpression_NotName ::= EqualityExpression_NotName '!=' InstanceofExpression
/.$putCase consumeEqualityExpression(OperatorIds.NOT_EQUAL); $break ./
EqualityExpression_NotName ::= NameOrAj '!=' InstanceofExpression -- AspectJ Extension was Name
/.$putCase consumeEqualityExpressionWithName(OperatorIds.NOT_EQUAL); $break ./
/:$readableName Expression:/

AndExpression_NotName -> EqualityExpression_NotName
AndExpression_NotName ::= AndExpression_NotName '&' EqualityExpression
/.$putCase consumeBinaryExpression(OperatorIds.AND); $break ./
AndExpression_NotName ::= NameOrAj '&' EqualityExpression -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.AND); $break ./
/:$readableName Expression:/

ExclusiveOrExpression_NotName -> AndExpression_NotName
ExclusiveOrExpression_NotName ::= ExclusiveOrExpression_NotName '^' AndExpression
/.$putCase consumeBinaryExpression(OperatorIds.XOR); $break ./
ExclusiveOrExpression_NotName ::= NameOrAj '^' AndExpression -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.XOR); $break ./
/:$readableName Expression:/

InclusiveOrExpression_NotName -> ExclusiveOrExpression_NotName
InclusiveOrExpression_NotName ::= InclusiveOrExpression_NotName '|' ExclusiveOrExpression
/.$putCase consumeBinaryExpression(OperatorIds.OR); $break ./
InclusiveOrExpression_NotName ::= NameOrAj '|' ExclusiveOrExpression -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.OR); $break ./
/:$readableName Expression:/

ConditionalAndExpression_NotName -> InclusiveOrExpression_NotName
ConditionalAndExpression_NotName ::= ConditionalAndExpression_NotName '&&' InclusiveOrExpression
/.$putCase consumeBinaryExpression(OperatorIds.AND_AND); $break ./
ConditionalAndExpression_NotName ::= NameOrAj '&&' InclusiveOrExpression -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.AND_AND); $break ./
/:$readableName Expression:/

ConditionalOrExpression_NotName -> ConditionalAndExpression_NotName
ConditionalOrExpression_NotName ::= ConditionalOrExpression_NotName '||' ConditionalAndExpression
/.$putCase consumeBinaryExpression(OperatorIds.OR_OR); $break ./
ConditionalOrExpression_NotName ::= NameOrAj '||' ConditionalAndExpression -- AspectJ Extension was Name
/.$putCase consumeBinaryExpressionWithName(OperatorIds.OR_OR); $break ./
/:$readableName Expression:/

ConditionalExpression_NotName -> ConditionalOrExpression_NotName
ConditionalExpression_NotName ::= ConditionalOrExpression_NotName '?' Expression ':' ConditionalExpression
/.$putCase consumeConditionalExpression(OperatorIds.QUESTIONCOLON) ; $break ./
ConditionalExpression_NotName ::= NameOrAj '?' Expression ':' ConditionalExpression -- AspectJ Extension was Name
/.$putCase consumeConditionalExpressionWithName(OperatorIds.QUESTIONCOLON) ; $break ./
/:$readableName Expression:/

AssignmentExpression_NotName -> ConditionalExpression_NotName
AssignmentExpression_NotName -> Assignment
/:$readableName Expression:/

Expression_NotName -> AssignmentExpression_NotName
/:$readableName Expression:/
-----------------------------------------------
-- 1.5 features : end of generics
-----------------------------------------------
-----------------------------------------------
-- 1.5 features : annotation - Metadata feature jsr175
-----------------------------------------------
AnnotationTypeDeclarationHeaderName ::= Modifiers '@' PushRealModifiers interface JavaIdentifier -- AspectJ extension, was Identifier
/.$putCase consumeAnnotationTypeDeclarationHeaderName() ; $break ./
/:$compliance 1.5:/
AnnotationTypeDeclarationHeaderName ::= Modifiers '@' PushRealModifiers interface JavaIdentifier TypeParameters -- AspectJ JavaIdentifier was Identifier
/.$putCase consumeAnnotationTypeDeclarationHeaderNameWithTypeParameters() ; $break ./
/:$compliance 1.5:/
AnnotationTypeDeclarationHeaderName ::= '@' PushModifiersForHeader interface JavaIdentifier TypeParameters -- AspectJ JavaIdentifier was Identifier
/.$putCase consumeAnnotationTypeDeclarationHeaderNameWithTypeParameters() ; $break ./
/:$compliance 1.5:/
AnnotationTypeDeclarationHeaderName ::= '@' PushModifiersForHeader interface JavaIdentifier -- AspectJ JavaIdentifier was Identifier
/.$putCase consumeAnnotationTypeDeclarationHeaderName() ; $break ./
/:$readableName AnnotationTypeDeclarationHeaderName:/
/:$compliance 1.5:/

AnnotationTypeDeclarationHeader ::= AnnotationTypeDeclarationHeaderName ClassHeaderExtendsopt ClassHeaderImplementsopt
/.$putCase consumeAnnotationTypeDeclarationHeader() ; $break ./
/:$readableName AnnotationTypeDeclarationHeader:/
/:$compliance 1.5:/

AnnotationTypeDeclaration ::= AnnotationTypeDeclarationHeader AnnotationTypeBody
/.$putCase consumeAnnotationTypeDeclaration() ; $break ./
/:$readableName AnnotationTypeDeclaration:/
/:$compliance 1.5:/

AnnotationTypeBody ::= '{' AnnotationTypeMemberDeclarationsopt '}'
/:$readableName AnnotationTypeBody:/
/:$compliance 1.5:/

AnnotationTypeMemberDeclarationsopt ::= $empty
/.$putCase consumeEmptyAnnotationTypeMemberDeclarationsopt() ; $break ./
/:$compliance 1.5:/
AnnotationTypeMemberDeclarationsopt ::= NestedType AnnotationTypeMemberDeclarations
/.$putCase consumeAnnotationTypeMemberDeclarationsopt() ; $break ./
/:$readableName AnnotationTypeMemberDeclarations:/
/:$compliance 1.5:/

AnnotationTypeMemberDeclarations -> AnnotationTypeMemberDeclaration
/:$compliance 1.5:/
AnnotationTypeMemberDeclarations ::= AnnotationTypeMemberDeclarations AnnotationTypeMemberDeclaration
/.$putCase consumeAnnotationTypeMemberDeclarations() ; $break ./
/:$readableName AnnotationTypeMemberDeclarations:/
/:$compliance 1.5:/

AnnotationMethodHeaderName ::= Modifiersopt TypeParameters Type 'JavaIdentifier' '(' -- AspectJ JavaIdentifier was Identifier
/.$putCase consumeMethodHeaderNameWithTypeParameters(true); $break ./
AnnotationMethodHeaderName ::= Modifiersopt Type 'JavaIdentifier' '(' -- AspectJ JavaIdentifier was Identifier
/.$putCase consumeMethodHeaderName(true); $break ./
/:$readableName MethodHeaderName:/
/:$compliance 1.5:/

AnnotationMethodHeaderDefaultValueopt ::= $empty
/.$putCase consumeEmptyMethodHeaderDefaultValue() ; $break ./
/:$readableName MethodHeaderDefaultValue:/
/:$compliance 1.5:/
AnnotationMethodHeaderDefaultValueopt ::= DefaultValue
/.$putCase consumeMethodHeaderDefaultValue(); $break ./
/:$readableName MethodHeaderDefaultValue:/
/:$compliance 1.5:/

AnnotationMethodHeader ::= AnnotationMethodHeaderName FormalParameterListopt MethodHeaderRightParen MethodHeaderExtendedDims AnnotationMethodHeaderDefaultValueopt
/.$putCase consumeMethodHeader(); $break ./
/:$readableName AnnotationMethodHeader:/
/:$compliance 1.5:/

AnnotationTypeMemberDeclaration ::= AnnotationMethodHeader ';'
/.$putCase consumeAnnotationTypeMemberDeclaration() ; $break ./
/:$compliance 1.5:/
AnnotationTypeMemberDeclaration -> ConstantDeclaration
/:$compliance 1.5:/
AnnotationTypeMemberDeclaration -> ConstructorDeclaration
/:$compliance 1.5:/
AnnotationTypeMemberDeclaration -> TypeDeclaration
/:$readableName AnnotationTypeMemberDeclaration:/
/:$compliance 1.5:/

DefaultValue ::= 'default' MemberValue
/:$readableName DefaultValue:/
/:$compliance 1.5:/

Annotation -> NormalAnnotation
/:$compliance 1.5:/
Annotation -> MarkerAnnotation
/:$compliance 1.5:/
Annotation -> SingleMemberAnnotation
/:$readableName Annotation:/
/:$compliance 1.5:/

AnnotationName ::= '@' UnannotatableNameOrAj -- AspectJ: was UnannotatableName
/.$putCase consumeAnnotationName() ; $break ./
/:$readableName AnnotationName:/
/:$compliance 1.5:/
/:$recovery_template @ Identifier:/

NormalAnnotation ::= AnnotationName '(' MemberValuePairsopt ')'
/.$putCase consumeNormalAnnotation(false) ; $break ./
/:$readableName NormalAnnotation:/
/:$compliance 1.5:/

MemberValuePairsopt ::= $empty
/.$putCase consumeEmptyMemberValuePairsopt() ; $break ./
/:$compliance 1.5:/
MemberValuePairsopt -> MemberValuePairs
/:$readableName MemberValuePairsopt:/
/:$compliance 1.5:/

MemberValuePairs -> MemberValuePair
/:$compliance 1.5:/
MemberValuePairs ::= MemberValuePairs ',' MemberValuePair
/.$putCase consumeMemberValuePairs() ; $break ./
/:$readableName MemberValuePairs:/
/:$compliance 1.5:/

MemberValuePair ::= SimpleNameOrAj '=' EnterMemberValue MemberValue ExitMemberValue  -- AspectJ Extension was SimpleName
/.$putCase consumeMemberValuePair() ; $break ./
/:$readableName MemberValuePair:/
/:$compliance 1.5:/

EnterMemberValue ::= $empty
/.$putCase consumeEnterMemberValue() ; $break ./
/:$readableName EnterMemberValue:/
/:$compliance 1.5:/

ExitMemberValue ::= $empty
/.$putCase consumeExitMemberValue() ; $break ./
/:$readableName ExitMemberValue:/
/:$compliance 1.5:/

MemberValue -> ConditionalExpression_NotName
/:$compliance 1.5:/
MemberValue ::= NameOrAj -- AspectJ Extension, was Name
/.$putCase consumeMemberValueAsName() ; $break ./
/:$compliance 1.5:/
MemberValue -> Annotation
/:$compliance 1.5:/
MemberValue -> MemberValueArrayInitializer
/:$readableName MemberValue:/
/:$recovery_template JavaIdentifier:/ -- AspectJ extension, was Identifier
/:$compliance 1.5:/

MemberValueArrayInitializer ::= EnterMemberValueArrayInitializer '{' PushLeftBrace MemberValues ',' '}'
/.$putCase consumeMemberValueArrayInitializer() ; $break ./
/:$compliance 1.5:/
MemberValueArrayInitializer ::= EnterMemberValueArrayInitializer '{' PushLeftBrace MemberValues '}'
/.$putCase consumeMemberValueArrayInitializer() ; $break ./
/:$compliance 1.5:/
MemberValueArrayInitializer ::= EnterMemberValueArrayInitializer '{' PushLeftBrace ',' '}'
/.$putCase consumeEmptyMemberValueArrayInitializer() ; $break ./
/:$compliance 1.5:/
MemberValueArrayInitializer ::= EnterMemberValueArrayInitializer '{' PushLeftBrace '}'
/.$putCase consumeEmptyMemberValueArrayInitializer() ; $break ./
/:$readableName MemberValueArrayInitializer:/
/:$compliance 1.5:/

EnterMemberValueArrayInitializer ::= $empty
/.$putCase consumeEnterMemberValueArrayInitializer() ; $break ./
/:$readableName EnterMemberValueArrayInitializer:/
/:$compliance 1.5:/

MemberValues -> MemberValue
/:$compliance 1.5:/
MemberValues ::= MemberValues ',' MemberValue
/.$putCase consumeMemberValues() ; $break ./
/:$readableName MemberValues:/
/:$compliance 1.5:/

MarkerAnnotation ::= AnnotationName
/.$putCase consumeMarkerAnnotation(false) ; $break ./
/:$readableName MarkerAnnotation:/
/:$compliance 1.5:/

SingleMemberAnnotationMemberValue ::= MemberValue
/.$putCase consumeSingleMemberAnnotationMemberValue() ; $break ./
/:$readableName MemberValue:/
/:$compliance 1.5:/

SingleMemberAnnotation ::= AnnotationName '(' SingleMemberAnnotationMemberValue ')'
/.$putCase consumeSingleMemberAnnotation(false) ; $break ./
/:$readableName SingleMemberAnnotation:/
/:$compliance 1.5:/
--------------------------------------
-- 1.5 features : end of annotation --
--------------------------------------

-----------------------------------
-- 1.5 features : recovery rules --
-----------------------------------
RecoveryMethodHeaderName ::= Modifiersopt TypeParameters Type 'JavaIdentifierNoAround' '(' -- AspectJ (JavaIdentifierNoAround was Identifier)
/.$putCase consumeRecoveryMethodHeaderNameWithTypeParameters(); $break ./
/:$compliance 1.5:/
RecoveryMethodHeaderName ::= Modifiersopt Type 'JavaIdentifierNoAround' '(' -- AspectJ (JavaIdentifierNoAround was Identifier)
/.$putCase consumeRecoveryMethodHeaderName(); $break ./
/:$readableName MethodHeaderName:/
RecoveryMethodHeaderName ::= ModifiersWithDefault TypeParameters Type 'JavaIdentifierNoAround' '(' -- AspectJ (JavaIdentifierNoAround was Identifier)
/.$putCase consumeRecoveryMethodHeaderNameWithTypeParameters(); $break ./
/:$compliance 1.5:/
RecoveryMethodHeaderName ::= ModifiersWithDefault Type 'JavaIdentifierNoAround' '(' -- AspectJ (JavaIdentifierNoAround was Identifier)
/.$putCase consumeRecoveryMethodHeaderName(); $break ./
/:$readableName MethodHeaderName:/

RecoveryMethodHeader ::= RecoveryMethodHeaderName FormalParameterListopt MethodHeaderRightParen MethodHeaderExtendedDims AnnotationMethodHeaderDefaultValueopt
/.$putCase consumeMethodHeader(); $break ./
RecoveryMethodHeader ::= RecoveryMethodHeaderName FormalParameterListopt MethodHeaderRightParen MethodHeaderExtendedDims MethodHeaderThrowsClause
/.$putCase consumeMethodHeader(); $break ./
/:$readableName MethodHeader:/
-----------------------------------
-- 1.5 features : recovery rules --
-----------------------------------

/.	}
}./

$names

PLUS_PLUS ::=    '++'
MINUS_MINUS ::=    '--'
EQUAL_EQUAL ::=    '=='
LESS_EQUAL ::=    '<='
GREATER_EQUAL ::=    '>='
NOT_EQUAL ::=    '!='
LEFT_SHIFT ::=    '<<'
RIGHT_SHIFT ::=    '>>'
UNSIGNED_RIGHT_SHIFT ::=    '>>>'
PLUS_EQUAL ::=    '+='
MINUS_EQUAL ::=    '-='
MULTIPLY_EQUAL ::=    '*='
DIVIDE_EQUAL ::=    '/='
AND_EQUAL ::=    '&='
OR_EQUAL ::=    '|='
XOR_EQUAL ::=    '^='
REMAINDER_EQUAL ::=    '%='
LEFT_SHIFT_EQUAL ::=    '<<='
RIGHT_SHIFT_EQUAL ::=    '>>='
UNSIGNED_RIGHT_SHIFT_EQUAL ::=    '>>>='
OR_OR ::=    '||'
AND_AND ::=    '&&'
PLUS ::=    '+'
MINUS ::=    '-'
NOT ::=    '!'
REMAINDER ::=    '%'
XOR ::=    '^'
AND ::=    '&'
MULTIPLY ::=    '*'
OR ::=    '|'
TWIDDLE ::=    '~'
DIVIDE ::=    '/'
GREATER ::=    '>'
LESS ::=    '<'
LPAREN ::=    '('
RPAREN ::=    ')'
LBRACE ::=    '{'
RBRACE ::=    '}'
LBRACKET ::=    '['
RBRACKET ::=    ']'
SEMICOLON ::=    ';'
QUESTION ::=    '?'
COLON ::=    ':'
COMMA ::=    ','
DOT ::=    '.'
EQUAL ::=    '='
AT ::=    '@'
AT308 ::= '@'
AT308DOTDOTDOT ::= '@'
ELLIPSIS ::=    '...'
ARROW ::= '->'
COLON_COLON ::= '::'
UNDERSCORE ::= '_'

$end
-- need a carriage return after the $end
