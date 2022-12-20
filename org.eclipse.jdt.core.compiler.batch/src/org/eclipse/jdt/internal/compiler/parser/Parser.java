/* *******************************************************************
 * Copyright (c) 2002,2003 Palo Alto Research Center, Incorporated (PARC).
 *               2013, contributors
 * All rights reserved.
 * This program and the accompanying materials are made available
 * under the terms of the Eclipse Public License v 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/org/documents/epl-2.0/EPL-2.0.txt
 *
 * Contributors:
 *     PARC     initial implementation
 *     Adrian Colyer refactored for use in org.eclipse.jdt.core package
 * ******************************************************************/
package org.eclipse.jdt.internal.compiler.parser;

import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.internal.compiler.CompilationResult;
import org.eclipse.jdt.internal.compiler.ast.*;
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileConstants;
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions;
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter;
import org.eclipse.jdt.internal.compiler.problem.ProblemSeverities;

import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.List;

// AspectJ Extension - this whole class is an AspectJ extension to the parser
public class Parser extends TheOriginalJDTParserClass {

	private static final String ASPECTJ_DECLARATION_FACTORY = "org.aspectj.ajdt.internal.compiler.parser.DeclarationFactory";
	private static IDeclarationFactory declarationFactory;

	static {
		try{
			initTables(Parser.class);
			declarationFactory = (IDeclarationFactory) Class.forName(ASPECTJ_DECLARATION_FACTORY).newInstance();
		} catch(java.io.IOException ex){
			throw new ExceptionInInitializerError(ex.getMessage());
		} catch (InstantiationException ex) {
			throw new ExceptionInInitializerError(ex.getMessage());
		} catch (IllegalAccessException ex) {
			throw new ExceptionInInitializerError(ex.getMessage());
		} catch (ClassNotFoundException ex) {
			System.err.println("Warning: AspectJ declaration factory class not found on classpath");
			//throw new ExceptionInInitializerError(ex.getMessage());
		}
	}

	public interface IDeclarationFactory {
			MessageSend createProceed(MessageSend m);
			TypeDeclaration createAspect(CompilationResult result);
			void setPrivileged(TypeDeclaration aspectDecl, boolean isPrivileged);
			void setPerClauseFrom(TypeDeclaration aspectDecl, ASTNode pseudoTokens, Parser parser);
			void setDominatesPatternFrom(TypeDeclaration aspectDecl, ASTNode pseudoTokens, Parser parser);
			ASTNode createPseudoTokensFrom(ASTNode[] tokens, CompilationResult result);
			MethodDeclaration createMethodDeclaration(CompilationResult result);
			ConstructorDeclaration createConstructorDeclaration(CompilationResult result);
			MethodDeclaration createPointcutDeclaration(CompilationResult result);
			MethodDeclaration createAroundAdviceDeclaration(CompilationResult result);
			MethodDeclaration createAfterAdviceDeclaration(CompilationResult result);
			MethodDeclaration createBeforeAdviceDeclaration(CompilationResult result);
			ASTNode createPointcutDesignator(Parser parser, ASTNode pseudoTokens);
			void setPointcutDesignatorOnAdvice(MethodDeclaration adviceDecl, ASTNode des);
			void setPointcutDesignatorOnPointcut(MethodDeclaration adviceDecl, ASTNode des);
			void setExtraArgument(MethodDeclaration adviceDeclaration, Argument arg);
			boolean isAfterAdvice(MethodDeclaration adviceDecl);
			void setAfterThrowingAdviceKind(MethodDeclaration adviceDecl);
			void setAfterReturningAdviceKind(MethodDeclaration adviceDecl);
			MethodDeclaration createDeclareDeclaration(CompilationResult result, ASTNode pseudoTokens, Parser parser);
			MethodDeclaration createDeclareAnnotationDeclaration(CompilationResult result, ASTNode pseudoTokens, Annotation annotation, Parser parser,char kind);
			MethodDeclaration createInterTypeFieldDeclaration(CompilationResult result, TypeReference onType);
			MethodDeclaration createInterTypeMethodDeclaration(CompilationResult result);
			MethodDeclaration createInterTypeConstructorDeclaration(CompilationResult result);
			void setSelector(MethodDeclaration interTypeDecl, char[] selector);
			void setDeclaredModifiers(MethodDeclaration interTypeDecl, int modifiers);
			void setInitialization(MethodDeclaration itdFieldDecl, Expression initialization);
			void setOnType(MethodDeclaration interTypeDecl, TypeReference onType);
			ASTNode createPseudoToken(Parser parser, String value, boolean isIdentifier);
			ASTNode createIfPseudoToken(Parser parser, Expression expr);
			void setLiteralKind(ASTNode pseudoToken, String string);
			boolean shouldTryToRecover(ASTNode node);
			TypeDeclaration createIntertypeMemberClassDeclaration(CompilationResult compilationResult);
			void setOnType(TypeDeclaration interTypeDecl, TypeReference onType);
	}

//	public final static void initAjTables(Class parserClass)
//		throws java.io.IOException {
//
//		final String prefix = FILEPREFIX;
//		int i = 0;
//		lhsStatic = readTable(parserClass, prefix + (++i) + ".rsc"); //$NON-NLS-1$
//		char[] chars = readTable(parserClass, prefix + (++i) + ".rsc"); //$NON-NLS-1$
//		check_tableStatic = new short[chars.length];
//		for (int c = chars.length; c-- > 0;) {
//			check_tableStatic[c] = (short) (chars[c] - 32768);
//		}
//		asbStatic = readTable(parserClass, prefix + (++i) + ".rsc"); //$NON-NLS-1$
//		asrStatic = readTable(parserClass, prefix + (++i) + ".rsc"); //$NON-NLS-1$
//		symbol_indexStatic = readTable(parserClass, prefix + (++i) + ".rsc"); //$NON-NLS-1$
//		actionStatic = lhsStatic;
//	}

	//positions , dimensions , .... (int stacks)
	protected int aspectIntPtr;
	protected int[] aspectIntStack;

	@Override
	public void initialize() {
		super.initialize();
		aspectIntPtr = -1;
		aspectIntStack = new int[4];
	}

	@Override
	public void initialize(boolean initializeNLS) {
		super.initialize(initializeNLS);
		aspectIntPtr = -1;
		aspectIntStack = new int[4];
	}

	@Override
	public void initializeScanner(){
		this.scanner = new Scanner(
			false /*comment*/,
			false /*whitespace*/,
			this.options.getSeverity(CompilerOptions.NonExternalizedString) != ProblemSeverities.Ignore /*nls*/,
			this.options.sourceLevel /*sourceLevel*/,
			this.options.taskTags/*taskTags*/,
			this.options.taskPriorities/*taskPriorities*/,
			this.options.isTaskCaseSensitive/*taskCaseSensitive*/,
			this.options.enablePreviewFeatures /*isPreviewEnabled*/);
	}

//*************New display debugging method
	private static final boolean AJ_DEBUG = false;

	void println(Object o) {
		if (AJ_DEBUG) System.out.println(o);
	}

	private void printStack(Object[] s, int p) {
		List list = Arrays.asList(s);
		System.out.println("  " + list.subList(0, p+1));
	}

	private void printStack(int[] s, int p) {
		StringBuffer buf = new StringBuffer("[");
		for (int i=0; i<p+1; i++) {
			if (i > 0) buf.append(", ");
			buf.append(Integer.toString(s[i]));
		}
		buf.append("]");
		System.out.println("  " + buf);
	}

	private void printStack(long[] s, int p) {
		StringBuffer buf = new StringBuffer("[");
		for (int i=0; i<p+1; i++) {
			if (i > 0) buf.append(", ");
			buf.append(Long.toString(s[i]));
		}
		buf.append("]");
		System.out.println("  " + buf);
	}

	private void printStack(char[][] s, int p) {
		StringBuffer buf = new StringBuffer("[");
		for (int i=0; i<p+1; i++) {
			if (i > 0) buf.append(", ");
			buf.append(new String(s[i]));
		}
		buf.append("]");
		System.out.println("  " + buf);
	}

	public void display() {
		if (!AJ_DEBUG) return;
		System.out.print("astStack: ");
		printStack(astStack, astPtr);
		System.out.print("astLengthStack: ");
		printStack(astLengthStack, astLengthPtr);

		System.out.print("expressionStack: ");
		printStack(expressionStack, expressionPtr);
		System.out.print("expressionLengthStack: ");
		printStack(expressionLengthStack, expressionLengthPtr);

		System.out.print("identifierStack: ");
		printStack(identifierStack, identifierPtr);
		System.out.print("identifierLengthStack: ");
		printStack(identifierLengthStack, identifierLengthPtr);
		System.out.print("identifierPositionStack: ");
		printStack(identifierPositionStack, identifierPtr);


		System.out.print("intStack:");
		printStack(intStack, intPtr);
		System.out.println();
	}



//************** Overriding behavior for standard Java rules
	@Override
	protected MethodDeclaration createMethodDeclaration(CompilationResult result) {
		return declarationFactory.createMethodDeclaration(result);
	}

	@Override
	protected ConstructorDeclaration createConstructorDeclaration(CompilationResult result) {
		return declarationFactory.createConstructorDeclaration(result);
	}

	@Override
	protected void consumeMethodInvocationName() {
		super.consumeMethodInvocationName();

		MessageSend m = (MessageSend)expressionStack[expressionPtr];
		if (CharOperation.equals(m.selector, "proceed".toCharArray())) {
			expressionStack[expressionPtr] = declarationFactory.createProceed(m);
		}
	}

	@Override
	protected void consumeToken(int type) {
		currentTokenStart = scanner.startPosition;
		super.consumeToken(type);
		switch (type) {
			case TokenNameaspect :  // pseudo keyword
				//aspectIntPtr = -1; //XXX  If we ever see a bug with aspects nested in aspects,
                //                   // this line is the culprit!
				pushOnAspectIntStack(this.scanner.currentPosition - 1);
				pushOnAspectIntStack(this.scanner.startPosition);
				// deliberate fall through...
			case TokenNameprivileged :  // pseudo keyword
			case TokenNamepointcut :  // pseudo keyword
			case TokenNamebefore :  // pseudo keyword
			case TokenNameafter :  // pseudo keyword
			case TokenNamearound :  // pseudo keyword
			case TokenNamedeclare :  // pseudo keyword
				pushIdentifier();
				flushCommentsDefinedPriorTo(currentTokenStart);
				scanner.commentPtr = -1;
				break;
		}
	}


//************New AspectJ rules
	protected void consumeAspectDeclaration() {
	    // AspectDeclaration ::= AspectHeader AspectBody
	    consumeClassDeclaration();
	    //??? post parsing step here
	}

	protected void consumeAspectHeader() {
	    // AspectHeader ::= AspectHeaderName ClassHeaderExtendsopt ClassHeaderImplementsopt AspectHeaderRest
		consumeClassHeader();
	}

	protected void consumeAspectHeaderName(boolean isPrivileged) {
		// (isPrivileged == false) -> AspectHeaderName ::= Modifiersopt 'aspect' 'Identifier'
		// (isPrivileged == true) -> AspectHeaderName ::= Modifiersopt 'privileged' Modifiersopt 'aspect' 'Identifier'
		TypeDeclaration aspectDecl = declarationFactory.createAspect(this.compilationUnit.compilationResult);
		if (this.nestedMethod[this.nestedType] == 0) {
			if (this.nestedType != 0) {
				aspectDecl.bits |= ASTNode.IsMemberType;
			}
		} else {
			// Record that the block has a declaration for local types
			aspectDecl.bits |= ASTNode.IsLocalType;
			markEnclosingMemberWithLocalType();
			blockReal();
		}

		println("aspect header name: ");
		this.display();

		//highlight the name of the type
		long pos = identifierPositionStack[identifierPtr];
		aspectDecl.sourceEnd = (int) pos;
		aspectDecl.sourceStart = (int) (pos >>> 32);
		aspectDecl.name = identifierStack[identifierPtr--];
		identifierLengthPtr--;

		//compute the declaration source too
		// 'class' and 'interface' push two int positions: the beginning of the class token and its end.
		// we want to keep the beginning position but get rid of the end position
		// it is only used for the ClassLiteralAccess positions.
		aspectDecl.declarationSourceStart = this.aspectIntStack[this.aspectIntPtr--];
		this.aspectIntPtr--; // remove the end position of the class token

		// pop the aspect pseudo-token
		eatIdentifier();


		// handle modifiers, only without privileged for now
		if (isPrivileged) {
			pos = eatIdentifier(); // eat the privileged
//			int end = (int) pos;
//		    int start = (int) (pos >>> 32);
		    declarationFactory.setPrivileged(aspectDecl,true);
			//problemReporter().signalError(start, end, "privileged is unimplemented in 1.1alpha1");
		}
		aspectDecl.modifiersSourceStart = intStack[intPtr--];
		aspectDecl.modifiers = intStack[intPtr--];
		if (isPrivileged) {
			aspectDecl.modifiersSourceStart = intStack[intPtr--];
			aspectDecl.modifiers |= intStack[intPtr--];
		}
		if (aspectDecl.modifiersSourceStart >= 0) {
			aspectDecl.declarationSourceStart = aspectDecl.modifiersSourceStart;
		}

		println("modifiers: " + aspectDecl.modifiers);

		// consume annotations
		int length;
		if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
			System.arraycopy(
				this.expressionStack,
				(this.expressionPtr -= length) + 1,
				aspectDecl.annotations = new Annotation[length],
				0,
				length);
		}

		aspectDecl.bodyStart = aspectDecl.sourceEnd + 1;
		pushOnAstStack(aspectDecl);

		listLength = 0; // will be updated when reading super-interfaces
		// recovery
		if (currentElement != null) {
			lastCheckPoint = aspectDecl.bodyStart;
			currentElement = currentElement.add(aspectDecl, 0);
			lastIgnoredToken = -1;
		}

        // Grab the javadoc
        aspectDecl.javadoc = this.javadoc;
        this.javadoc = null;

		this.display();
	}

	protected void consumeAspectHeaderNameWithTypeParameters(boolean isPriviliged) {
		TypeDeclaration typeDecl = (TypeDeclaration)this.astStack[this.astPtr];

		// consume type parameters
		int length = this.genericsLengthStack[this.genericsLengthPtr--];
		this.genericsPtr -= length;
		System.arraycopy(this.genericsStack, this.genericsPtr + 1, typeDecl.typeParameters = new TypeParameter[length], 0, length);

		typeDecl.bodyStart = typeDecl.typeParameters[length-1].declarationSourceEnd + 1;

		this.listTypeParameterLength = 0;

		if (this.currentElement != null) { // is recovering
			this.lastCheckPoint = typeDecl.bodyStart;
		}
	}

	private long eatIdentifier() {
		long pos = identifierPositionStack[identifierPtr];
		identifierPtr--;
		identifierLengthPtr--;
		return pos;
	}

	protected void consumeAspectHeaderRest() {
		//--[dominates TypePattern] [persingleton() | percflow(PCD) | perthis(PCD) | pertarget(PCD)]
		//AspectHeaderRest ::= AspectHeaderRestStart PseudoTokens
		concatNodeLists();
		this.display();
		ASTNode pseudoTokens = popPseudoTokens("{");
		println("pseudo: " + pseudoTokens);

		TypeDeclaration aspectDecl = (TypeDeclaration) astStack[astPtr];

		declarationFactory.setDominatesPatternFrom(aspectDecl,pseudoTokens,this);
		declarationFactory.setPerClauseFrom(aspectDecl,pseudoTokens,this);
		// XXX handle dominates
	}


	protected void consumePointcutDeclaration() {
		consumePointcutDesignatorOnDeclaration();
	}

	// AspectJ extension - accessor method for the currentTokenStart
	public int getCurrentTokenStart() {
		return currentTokenStart;
	}
	// End AspectJ extension

	protected void consumeEmptyPointcutDeclaration() {
		// AspectJ extension - set up some positions, required by AST support
		MethodDeclaration pcutDecl = (MethodDeclaration)astStack[astPtr];
		pcutDecl.bodyEnd = endStatementPosition;
		// End Aspectj Extension
		//??? set pcd to non-null
	}

	protected void consumePointcutHeader() {
		//PointcutDeclaration ::= Modifiersopt 'pointcut'  JavaIdentifier '('

		MethodDeclaration ret = declarationFactory.createPointcutDeclaration(compilationUnit.compilationResult);

		//the name
		long pos = identifierPositionStack[identifierPtr];
//		int sourceEnd = (int) pos;
		ret.sourceStart = (int) (pos >>> 32);
		ret.selector = identifierStack[identifierPtr--];
		identifierLengthPtr--;

        // Grab the javadoc
		ret.javadoc = this.javadoc;
        this.javadoc = null;

		// pop the 'pointcut' keyword
		eatIdentifier();

		// modifiers
		ret.declarationSourceStart = intStack[intPtr--];
		ret.modifiers = intStack[intPtr--];
		// consume annotations
		int length;
		if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
			System.arraycopy(
				this.expressionStack,
				(this.expressionPtr -= length) + 1,
				ret.annotations = new Annotation[length],
				0,
				length);
		}

		pushOnAstStack(ret);
	}



	protected void consumeAroundDeclaration() {
		// AroundDeclaration ::= AroundHeader MethodBody
		consumeMethodDeclaration(true,false);
	}

	protected void consumeAroundHeader() {
		consumePointcutDesignatorOnAdvice();
		resetModifiers(); // forget any modifiers encountered in the pointcut 263666
		consumeMethodHeader();
	}

	protected void consumeAroundHeaderName() {
		// AroundHeaderName ::= Modifiersopt Type  'around' '('

		MethodDeclaration adviceDecl = declarationFactory.createAroundAdviceDeclaration(compilationUnit.compilationResult);

		// skip the name of the advice
		long pos = eatIdentifier();
		adviceDecl.sourceStart = (int) (pos >>> 32);

		// but put in a placeholder name
        adviceDecl.selector = new char[] {'a','j','c','$','a','d','v','i','c','e'};

		TypeReference returnType = getTypeReference(intStack[intPtr--]);

		//modifiers
		adviceDecl.declarationSourceStart = intStack[intPtr--];
		adviceDecl.modifiers = intStack[intPtr--];

		adviceDecl.returnType = returnType;

		// consume annotations
		int length;
		if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
			System.arraycopy(
				this.expressionStack,
				(this.expressionPtr -= length) + 1,
				adviceDecl.annotations = new Annotation[length],
				0,
				length);
		}

        // Grab the javadoc
        adviceDecl.javadoc = this.javadoc;
        this.javadoc = null;

		//XXX get some locations right

		pushOnAstStack(adviceDecl);
	}

	protected void consumePointcutDesignatorOnAdvice() {
		ASTNode des = popPointcutDesignator("{");
		MethodDeclaration adviceDecl = (MethodDeclaration)astStack[astPtr];
		declarationFactory.setPointcutDesignatorOnAdvice(adviceDecl,des);
		adviceDecl.sourceEnd = 	des.sourceEnd;
		adviceDecl.bodyStart = des.sourceEnd+1;
	}

	protected void consumePointcutDesignatorOnDeclaration() {
		ASTNode des = popPointcutDesignator(";");
		MethodDeclaration pcutDecl = (MethodDeclaration)astStack[astPtr];
		declarationFactory.setPointcutDesignatorOnPointcut(pcutDecl,des);
		pcutDecl.sourceEnd = 	des.sourceEnd;
		pcutDecl.bodyStart = des.sourceEnd+1;
		pcutDecl.bodyEnd = endPosition;
		pcutDecl.declarationSourceEnd = flushCommentsDefinedPriorTo(endStatementPosition);
	}


	protected void consumeBasicAdviceDeclaration() {
		// BasicAdviceDeclaration ::= BasicAdviceHeader MethodBody
		consumeMethodDeclaration(true,false);
	}

	protected void consumeBasicAdviceHeader() {
		// BasicAdviceHeader ::= BasicAdviceHeaderName MethodHeaderParameters ExtraParamopt MethodHeaderThrowsClauseopt ':' PseudoTokens
		consumePointcutDesignatorOnAdvice();
		resetModifiers(); // forget any modifiers encountered in the pointcut 263666
		consumeMethodHeader();
	}


	protected void consumeBasicAdviceHeaderName(boolean isAfter) {
		// BasicAdviceHeaderName ::= 'before'|'after '('

		MethodDeclaration adviceDecl =
			(isAfter ? declarationFactory.createAfterAdviceDeclaration(compilationUnit.compilationResult) :
					  declarationFactory.createBeforeAdviceDeclaration(compilationUnit.compilationResult));

        // skip the name of the advice
		long pos = eatIdentifier();
		// but give a placeholder selector name
		adviceDecl.selector = new char[] {'a','j','c','$','a','d','v','i','c','e'};
        adviceDecl.sourceStart = (int) (pos >>> 32);

		//modifiers
		adviceDecl.declarationSourceStart = intStack[intPtr--];
		adviceDecl.modifiers = intStack[intPtr--];

		// consume annotations
		int length;
		if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
			System.arraycopy(
				this.expressionStack,
				(this.expressionPtr -= length) + 1,
				adviceDecl.annotations = new Annotation[length],
				0,
				length);
		}

        // Grab the javadoc
        adviceDecl.javadoc = this.javadoc;
        this.javadoc = null;

		//??? get more locations right

		pushOnAstStack(adviceDecl);
	}

	protected void consumeExtraParameterWithFormal() {
		Argument arg = (Argument)astStack[astPtr--];
		astLengthPtr--;

		declarationFactory.setExtraArgument((MethodDeclaration)astStack[astPtr],arg);

		consumeExtraParameterNoFormal();
	}


	protected void consumeExtraParameterNoFormal() {


	    long pos = identifierPositionStack[identifierPtr];
	    int end = (int) pos;
		int start = (int) (pos >>> 32);
	    char[] name = identifierStack[identifierPtr--];
	    identifierLengthPtr--;

	    //System.out.println("extra parameter: " + new String(name));

	    MethodDeclaration adviceDecl = (MethodDeclaration)astStack[astPtr];
	    if (declarationFactory.isAfterAdvice(adviceDecl)) {
	    	//XXX error, extra param makes no sense here
	    }

	    if (CharOperation.equals(name, "throwing".toCharArray())) {
	    	declarationFactory.setAfterThrowingAdviceKind(adviceDecl);
	    } else if (CharOperation.equals(name, "returning".toCharArray())) {
			declarationFactory.setAfterReturningAdviceKind(adviceDecl);
	    } else {
			problemReporter().parseError(
				start,
				end,
				currentToken,
				name,
				String.valueOf(name),
				new String[] {"throwing", "returning", ":"});
	    }
	}

	protected void consumeClassBodyDeclarationInAspect() { }


	protected void consumeDeclareDeclaration() {
		concatNodeLists();
		ASTNode tokens = popPseudoTokens(";");
		MethodDeclaration declareDecl = declarationFactory.createDeclareDeclaration(this.compilationUnit.compilationResult,tokens,this);
//		println("parsed declare: " + declare);
		display();
		pushOnAstStack(declareDecl);
	}


	protected void consumeDeclareAnnotation(char kind) {
		concatNodeLists();
		ASTNode tokens = popPseudoTokens(";");

		int length;
		Annotation[] annotations = new Annotation[1]; // there should only ever be one for us...
    	if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
    		System.arraycopy(
    			this.expressionStack,
    			(this.expressionPtr -= length) + 1,
    			annotations = new Annotation[length],
    			0,
    			length);
    	}

    	MethodDeclaration declareDecl = declarationFactory.createDeclareAnnotationDeclaration(this.compilationUnit.compilationResult,tokens,annotations[0],this,kind);
    	pushOnAstStack(declareDecl);
	}

	protected void consumeDeclareAnnotationHeader() {
		consumePseudoTokenIdentifier();  // name
		consumePseudoTokenIdentifier();  // declare
		swapAstStack();
		consumePseudoTokens();

		consumePseudoToken("@",0,false);
		swapAstStack();
		consumePseudoTokens();

		consumePseudoToken(":", 0, false);
		consumePseudoTokens();

		display();
	}

	protected void consumeDeclareHeader() {
		consumePseudoTokenIdentifier();  // name
		consumePseudoTokenIdentifier();  // declare
		swapAstStack();
		consumePseudoTokens();

		consumePseudoToken(":", 0, false);
		consumePseudoTokens();

//		println(">>>>>>>>>>>>>>>>>>>>>>>declare header");
		display();
	}

	protected void consumeInterTypeFieldHeader(boolean hasTypeParameters) {
//		println("about to consume field");
		this.display();

		long pos = identifierPositionStack[identifierPtr];
		int end = (int) pos;
		int start = (int) (pos >>> 32);
		char[] identifierName = identifierStack[identifierPtr--];
//		int extendedDimension = this.intStack[this.intPtr--];  // XXXX see consumeEnterVariable for what to do with this
		identifierLengthPtr--;

		if (hasTypeParameters) {
			pushOnGenericsIdentifiersLengthStack(this.identifierLengthStack[this.identifierLengthPtr]);
		} else {
			consumeClassOrInterfaceName();
		}
		TypeReference onType = getTypeReference(0);


		TypeReference returnType = getTypeReference(intStack[intPtr--]);
		this.display();

		int decSourceStart = intStack[intPtr--];
		int fieldModifiers = intStack[intPtr--];

		MethodDeclaration dec = declarationFactory.createInterTypeFieldDeclaration(
				this.compilationUnit.compilationResult,
				onType);

		dec.returnType = returnType;
		dec.sourceStart = start;
		dec.sourceEnd = end;
		declarationFactory.setSelector(dec,identifierName);
		dec.declarationSourceStart = decSourceStart;
		declarationFactory.setDeclaredModifiers(dec,fieldModifiers);
//		declarationFactory.setInitialization(dec,initialization);

		dec.bodyEnd = endPosition;
//		dec.declarationSourceEnd = flushCommentsDefinedPriorTo(endStatementPosition);

		// Grab the javadoc
        dec.javadoc = this.javadoc;
        this.javadoc = null;

    	// consume annotations
    	int length;
    	if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
    		System.arraycopy(
    			this.expressionStack,
    			(this.expressionPtr -= length) + 1,
    			dec.annotations = new Annotation[length],
    			0,
    			length);
    	}

		pushOnAstStack(dec);
		println("consumed field: " + dec);
		this.display();
}
	protected void consumeExitITDVariableWithoutInitializer() {
		MethodDeclaration itdDecl = (MethodDeclaration) this.astStack[this.astPtr];
		declarationFactory.setInitialization(itdDecl,null);
	}
	protected void consumeExitITDVariableWithInitializer() {
		this.expressionLengthPtr--;
		MethodDeclaration itdDecl = (MethodDeclaration) this.astStack[this.astPtr];
		Expression initialization = this.expressionStack[this.expressionPtr--];
		declarationFactory.setInitialization(itdDecl,initialization);
		// we need to update the declarationSourceEnd of the local variable declaration to the
		// source end position of the initialization expression
		itdDecl.declarationSourceEnd = initialization.sourceEnd;
	}

	protected void consumeInterTypeFieldDeclaration() {
		MethodDeclaration dec = (MethodDeclaration) this.astStack[this.astPtr];

		dec.bodyEnd = endPosition;
		dec.declarationSourceEnd = flushCommentsDefinedPriorTo(endStatementPosition);
	}

	protected void consumeInterTypeMethodDeclaration(boolean isNotAbstract) {
		consumeMethodDeclaration(isNotAbstract,false);
	}

	protected void consumeInterTypeMethodHeader() {
		consumeMethodHeader();
	}

	protected void consumeInterTypeConstructorDeclaration() {
		consumeMethodDeclaration(true,false);
	}

	protected void consumeInterTypeConstructorHeader() {
		consumeMethodHeader();
	}

	protected void consumeInterTypeMethodHeaderName(boolean hasMethodTypeParameters, boolean hasGenericTypeParameters) {
		//InterTypeMethodHeaderName ::= Modifiersopt Type OnType '.' JavaIdentifier '('
		this.display();
		MethodDeclaration md = declarationFactory.createInterTypeMethodDeclaration(
				this.compilationUnit.compilationResult);

		//identifier
		char[] name = identifierStack[identifierPtr];
		long selectorSource = identifierPositionStack[identifierPtr--];
		identifierLengthPtr--;


		//onType
		if (hasGenericTypeParameters) {
			pushOnGenericsIdentifiersLengthStack(this.identifierLengthStack[this.identifierLengthPtr]);
			//consumeClassOrInterfaceName();
		} else {
			consumeClassOrInterfaceName();
		}
		TypeReference onType = getTypeReference(0);

		//type
		md.returnType = getTypeReference(intStack[intPtr--]);

		// consume method type parameters
		if (hasMethodTypeParameters) {
			int tp_length = this.genericsLengthStack[this.genericsLengthPtr--];
			this.genericsPtr -= tp_length;
			System.arraycopy(this.genericsStack, this.genericsPtr + 1, md.typeParameters = new TypeParameter[tp_length], 0, tp_length);
		}

		declarationFactory.setOnType(md,onType);

		//modifiers
		md.declarationSourceStart = intStack[intPtr--];
		declarationFactory.setDeclaredModifiers(md,intStack[intPtr--]);

		//highlight starts at selector start
		md.sourceStart = (int) (selectorSource >>> 32);
		pushOnAstStack(md);
		md.sourceEnd = lParenPos;
		md.bodyStart = lParenPos + 1;
		declarationFactory.setSelector(md,name);
		listLength = 0;
		// initialize listLength before reading parameters/throws

 		// Grab the javadoc
        md.javadoc = this.javadoc;
        this.javadoc = null;

    	// consume annotations
    	int length;
    	if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
    		System.arraycopy(
    			this.expressionStack,
    			(this.expressionPtr -= length) + 1,
    			md.annotations = new Annotation[length],
    			0,
    			length);
    	}

		// recovery
		if (currentElement != null) {
			if (currentElement instanceof RecoveredType
				//|| md.modifiers != 0
				|| (scanner.getLineNumber(md.returnType.sourceStart)
					== scanner.getLineNumber(md.sourceStart))) {
				lastCheckPoint = md.bodyStart;
				currentElement = currentElement.add(md, 0);
				lastIgnoredToken = -1;
			} else {
				lastCheckPoint = md.sourceStart;
				restartRecovery = true;
			}
		}
	}

	protected void consumeInterTypeConstructorHeaderName(boolean hasConstructorTypeParameters, boolean hasTargetTypeParameters) {
		//InterTypeConstructorHeaderName ::= Modifiersopt Name '.' 'new' '('
		this.display();
		MethodDeclaration md = declarationFactory.createInterTypeConstructorDeclaration(
				this.compilationUnit.compilationResult);

		//identifier
//		md.selector = identifierStack[identifierPtr];
//		long selectorSource = identifierPositionStack[identifierPtr--];
////		identifierLengthPtr--;

		//onType
		if (!hasTargetTypeParameters) {
			consumeClassOrInterfaceName();
		}
		TypeReference onType = getTypeReference(0);
		declarationFactory.setOnType(md,onType);

		println("got onType: " + onType);
		this.display();

		intPtr--; // pop new info
		//type
		md.returnType = TypeReference.baseTypeReference(T_void, 0, null); //getTypeReference(intStack[intPtr--]);

		if (hasConstructorTypeParameters) {
			// consume type parameters
			int tp_length = this.genericsLengthStack[this.genericsLengthPtr--];
			this.genericsPtr -= tp_length;
			System.arraycopy(this.genericsStack, this.genericsPtr + 1, md.typeParameters = new TypeParameter[tp_length], 0, tp_length);
		}

		//modifiers
		md.declarationSourceStart = intStack[intPtr--];
		declarationFactory.setDeclaredModifiers(md,intStack[intPtr--]);
		//md.modifiers = intStack[intPtr--];

		// consume annotations
		int length;
		if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
			System.arraycopy(
				this.expressionStack,
				(this.expressionPtr -= length) + 1,
				md.annotations = new Annotation[length],
				0,
				length);
		}

		//highlight starts at selector start
		//md.sourceStart = (int) (selectorSource >>> 32);
		md.sourceStart = onType.sourceStart;
		pushOnAstStack(md);
		md.sourceEnd = lParenPos;
		md.bodyStart = lParenPos + 1;
		listLength = 0;
		// initialize listLength before reading parameters/throws

		declarationFactory.setSelector(md,
			(new String(CharOperation.concatWith(onType.getTypeName(), '_')) + "_new").toCharArray());


		// recovery
		if (currentElement != null) {
			if (currentElement instanceof RecoveredType
				//|| md.modifiers != 0
				//|| (scanner.getLineNumber(md.returnType.sourceStart)
				//	== scanner.getLineNumber(md.sourceStart))
				) {
				//lastCheckPoint = md.bodyStart;
				currentElement = currentElement.add(md, 0);
				lastIgnoredToken = -1;
			} else {
				lastCheckPoint = md.sourceStart;
				restartRecovery = true;
			}
		}
	}



//*********************************************************


	protected void consumePseudoToken(String value) {
		consumePseudoToken(value, 0, false);
	}

	protected void consumePseudoToken(
		String value,
		int popFromIntStack,
		boolean isIdentifier) {
		intPtr -= popFromIntStack;

		int start = currentTokenStart;
		int end = start + value.length() - 1;
		ASTNode tok = declarationFactory.createPseudoToken(this, value, isIdentifier);
		tok.sourceStart = start;
		tok.sourceEnd = end;
		pushOnAstStack(tok);
	}

	protected void consumePseudoTokenIdentifier() {
		long pos = identifierPositionStack[identifierPtr];
		int end = (int) pos;
		int start = (int) (pos >>> 32);
		char[] name = identifierStack[identifierPtr--];
		identifierLengthPtr--;

		ASTNode tok = declarationFactory.createPseudoToken(this, new String(name), true);
		tok.sourceStart = start;
		tok.sourceEnd = end;
		pushOnAstStack(tok);
	}

	protected void consumePseudoTokenIf() {
		//this.display();
		Expression expr = (Expression) expressionStack[expressionPtr--];
		expressionLengthPtr--;
		println("expr: " + expr);

		int start = intStack[intPtr--];
		ASTNode tok = declarationFactory.createIfPseudoToken(this, expr);
		tok.sourceStart = start;
		tok.sourceEnd = this.rParenPos;
		pushOnAstStack(tok);
	}

	protected void consumePseudoTokenLiteral() {
		Literal literal = (Literal) expressionStack[expressionPtr--];
		expressionLengthPtr--;
		//System.out.println("literal: " + new String(literal.source()));

		ASTNode tok = declarationFactory.createPseudoToken(this, new String(literal.source()), false);
		declarationFactory.setLiteralKind(tok,"string");
		tok.sourceStart = literal.sourceStart;
		tok.sourceEnd = literal.sourceEnd;
		pushOnAstStack(tok);
	}

	protected void consumePseudoTokenModifier() {
		//int modifier = modifiers;
		consumePseudoToken(Modifier.toString(modifiers), 0, true);
		modifiers = ClassFileConstants.AccDefault;
	}

	protected void consumePseudoTokenPrimitiveType() {
		TypeReference type = getTypeReference(0);

		ASTNode tok = declarationFactory.createPseudoToken(this, type.toString(), true);
		tok.sourceStart = type.sourceStart;
		tok.sourceEnd = type.sourceEnd;
		pushOnAstStack(tok);
	}

	protected void consumePseudoTokens() {
		optimizedConcatNodeLists();
	}
	@Override
  // This method is part of an automatic generation : do NOT edit-modify
	protected void consumeRule(int act) {
		switch (act) {
			case 47:
				if (DEBUG) { System.out.println("Type ::= PrimitiveType"); }  //$NON-NLS-1$
				consumePrimitiveType();
				break;

			case 61:
				if (DEBUG) { System.out.println("ReferenceType ::= ClassOrInterfaceType"); }  //$NON-NLS-1$
				consumeReferenceType();
				break;

			case 65:
				if (DEBUG) { System.out.println("ClassOrInterface ::= Name"); }  //$NON-NLS-1$
				consumeClassOrInterfaceName();
				break;

			case 66:
				if (DEBUG) { System.out.println("ClassOrInterface ::= GenericType DOT Name"); }  //$NON-NLS-1$
				consumeClassOrInterface();
				break;

			case 67:
				if (DEBUG) { System.out.println("GenericType ::= ClassOrInterface TypeArguments"); }  //$NON-NLS-1$
				consumeGenericType();
				break;

			case 68:
				if (DEBUG) { System.out.println("GenericType ::= ClassOrInterface LESS GREATER"); }  //$NON-NLS-1$
				consumeGenericTypeWithDiamond();
				break;

			case 69:
				if (DEBUG) { System.out.println("ArrayTypeWithTypeArgumentsName ::= GenericType DOT Name"); }  //$NON-NLS-1$
				consumeArrayTypeWithTypeArgumentsName();
				break;

			case 70:
				if (DEBUG) { System.out.println("ArrayType ::= PrimitiveType Dims"); }  //$NON-NLS-1$
				consumePrimitiveArrayType();
				break;

			case 71:
				if (DEBUG) { System.out.println("ArrayType ::= Name Dims"); }  //$NON-NLS-1$
				consumeNameArrayType();
				break;

			case 72:
				if (DEBUG) { System.out.println("ArrayType ::= ArrayTypeWithTypeArgumentsName Dims"); }  //$NON-NLS-1$
				consumeGenericTypeNameArrayType();
				break;

			case 73:
				if (DEBUG) { System.out.println("ArrayType ::= GenericType Dims"); }  //$NON-NLS-1$
				consumeGenericTypeArrayType();
				break;

			case 79:
				if (DEBUG) { System.out.println("AjName ::= AjSimpleName"); }  //$NON-NLS-1$
				consumeZeroTypeAnnotations();
				break;

			case 80:
				if (DEBUG) { System.out.println("AjName ::= AjQualifiedName"); }  //$NON-NLS-1$
				consumeZeroTypeAnnotations();
				break;

			case 89:
				if (DEBUG) { System.out.println("AjQualifiedName ::= AjName DOT SimpleNameOrAj"); }  //$NON-NLS-1$
				consumeQualifiedName();
				break;

			case 92:
				if (DEBUG) { System.out.println("Name ::= SimpleName"); }  //$NON-NLS-1$
				consumeZeroTypeAnnotations();
				break;

			case 97:
				if (DEBUG) { System.out.println("UnannotatableName ::= UnannotatableName DOT SimpleName"); }  //$NON-NLS-1$
				consumeUnannotatableQualifiedName();
				break;

			case 98:
				if (DEBUG) { System.out.println("QualifiedName ::= Name DOT JavaIdentifier"); }  //$NON-NLS-1$
				consumeQualifiedName(false);
				break;

			case 99:
				if (DEBUG) { System.out.println("QualifiedName ::= Name DOT TypeAnnotations JavaIdentifier"); }  //$NON-NLS-1$
				consumeQualifiedName(true);
				break;

			case 100:
				if (DEBUG) { System.out.println("TypeAnnotationsopt ::="); }  //$NON-NLS-1$
				consumeZeroTypeAnnotations();
				break;

			case 104:
				if (DEBUG) { System.out.println("TypeAnnotations0 ::= TypeAnnotations0 TypeAnnotation"); }  //$NON-NLS-1$
				consumeOneMoreTypeAnnotation();
				break;

			case 105:
				if (DEBUG) { System.out.println("TypeAnnotation ::= NormalTypeAnnotation"); }  //$NON-NLS-1$
				consumeTypeAnnotation();
				break;

			case 106:
				if (DEBUG) { System.out.println("TypeAnnotation ::= MarkerTypeAnnotation"); }  //$NON-NLS-1$
				consumeTypeAnnotation();
				break;

			case 107:
				if (DEBUG) { System.out.println("TypeAnnotation ::= SingleMemberTypeAnnotation"); }  //$NON-NLS-1$
				consumeTypeAnnotation();
				break;

			case 108:
				if (DEBUG) { System.out.println("TypeAnnotationName ::= AT308 UnannotatableName"); }  //$NON-NLS-1$
				consumeAnnotationName();
				break;

			case 109:
				if (DEBUG) { System.out.println("NormalTypeAnnotation ::= TypeAnnotationName LPAREN..."); }  //$NON-NLS-1$
				consumeNormalAnnotation(true);
				break;

			case 110:
				if (DEBUG) { System.out.println("MarkerTypeAnnotation ::= TypeAnnotationName"); }  //$NON-NLS-1$
				consumeMarkerAnnotation(true);
				break;

			case 111:
				if (DEBUG) { System.out.println("SingleMemberTypeAnnotation ::= TypeAnnotationName LPAREN"); }  //$NON-NLS-1$
				consumeSingleMemberAnnotation(true);
				break;

			case 112:
				if (DEBUG) { System.out.println("RejectTypeAnnotations ::="); }  //$NON-NLS-1$
				consumeNonTypeUseName();
				break;

			case 113:
				if (DEBUG) { System.out.println("PushZeroTypeAnnotations ::="); }  //$NON-NLS-1$
				consumeZeroTypeAnnotations();
				break;

			case 114:
				if (DEBUG) { System.out.println("VariableDeclaratorIdOrThis ::= this"); }  //$NON-NLS-1$
				consumeExplicitThisParameter(false);
				break;

			case 115:
				if (DEBUG) { System.out.println("VariableDeclaratorIdOrThis ::= UnannotatableName DOT..."); }  //$NON-NLS-1$
				consumeExplicitThisParameter(true);
				break;

			case 116:
				if (DEBUG) { System.out.println("VariableDeclaratorIdOrThis ::= VariableDeclaratorId"); }  //$NON-NLS-1$
				consumeVariableDeclaratorIdParameter();
				break;

			case 117:
				if (DEBUG) { System.out.println("CompilationUnit ::= EnterCompilationUnit..."); }  //$NON-NLS-1$
				consumeCompilationUnit();
				break;

			case 118:
				if (DEBUG) { System.out.println("InternalCompilationUnit ::= PackageDeclaration"); }  //$NON-NLS-1$
				consumeInternalCompilationUnit();
				break;

			case 119:
				if (DEBUG) { System.out.println("InternalCompilationUnit ::= PackageDeclaration..."); }  //$NON-NLS-1$
				consumeInternalCompilationUnit();
				break;

			case 120:
				if (DEBUG) { System.out.println("InternalCompilationUnit ::= PackageDeclaration..."); }  //$NON-NLS-1$
				consumeInternalCompilationUnitWithTypes();
				break;

			case 121:
				if (DEBUG) { System.out.println("InternalCompilationUnit ::= PackageDeclaration..."); }  //$NON-NLS-1$
				consumeInternalCompilationUnitWithTypes();
				break;

			case 122:
				if (DEBUG) { System.out.println("InternalCompilationUnit ::= ImportDeclarations..."); }  //$NON-NLS-1$
				consumeInternalCompilationUnit();
				break;

			case 123:
				if (DEBUG) { System.out.println("InternalCompilationUnit ::= TypeDeclarations"); }  //$NON-NLS-1$
				consumeInternalCompilationUnitWithTypes();
				break;

			case 124:
				if (DEBUG) { System.out.println("InternalCompilationUnit ::= ImportDeclarations..."); }  //$NON-NLS-1$
				consumeInternalCompilationUnitWithTypes();
				break;

			case 125:
				if (DEBUG) { System.out.println("InternalCompilationUnit ::="); }  //$NON-NLS-1$
				consumeEmptyInternalCompilationUnit();
				break;

			case 126:
				if (DEBUG) { System.out.println("InternalCompilationUnit ::= ImportDeclarations..."); }  //$NON-NLS-1$
				consumeInternalCompilationUnitWithModuleDeclaration();
				break;

			case 127:
				if (DEBUG) { System.out.println("InternalCompilationUnit ::= ModuleDeclaration"); }  //$NON-NLS-1$
				consumeInternalCompilationUnitWithModuleDeclaration();
				break;

			case 128:
				if (DEBUG) { System.out.println("ModuleDeclaration ::= ModuleHeader ModuleBody"); }  //$NON-NLS-1$
				consumeModuleDeclaration();
				break;

			case 129:
				if (DEBUG) { System.out.println("ModuleHeader ::= Modifiersopt ModuleModifieropt module"); }  //$NON-NLS-1$
				consumeModuleHeader();
				break;

			case 131:
				if (DEBUG) { System.out.println("ModuleModifieropt ::= ModuleModifier"); }  //$NON-NLS-1$
				consumeModuleModifiers();
				break;

			case 134:
				if (DEBUG) { System.out.println("ModuleStatementsOpt ::="); }  //$NON-NLS-1$
				consumeEmptyModuleStatementsOpt();
				break;

			case 137:
				if (DEBUG) { System.out.println("ModuleStatements ::= ModuleStatements ModuleStatement"); }  //$NON-NLS-1$
				consumeModuleStatements();
				break;

			case 143:
				if (DEBUG) { System.out.println("RequiresStatement ::= SingleRequiresModuleName SEMICOLON"); }  //$NON-NLS-1$
				consumeRequiresStatement();
				break;

			case 144:
				if (DEBUG) { System.out.println("SingleRequiresModuleName ::= requires..."); }  //$NON-NLS-1$
				consumeSingleRequiresModuleName();
				break;

			case 145:
				if (DEBUG) { System.out.println("RequiresModifiersopt ::= RequiresModifiers"); }  //$NON-NLS-1$
				consumeModifiers();
				break;

			case 146:
				if (DEBUG) { System.out.println("RequiresModifiersopt ::="); }  //$NON-NLS-1$
				consumeDefaultModifiers();
				break;

			case 148:
				if (DEBUG) { System.out.println("RequiresModifiers ::= RequiresModifiers RequiresModifier"); }  //$NON-NLS-1$
				consumeModifiers2();
				break;

			case 151:
				if (DEBUG) { System.out.println("ExportsStatement ::= ExportsHeader TargetModuleListopt"); }  //$NON-NLS-1$
				consumeExportsStatement();
				break;

			case 152:
				if (DEBUG) { System.out.println("ExportsHeader ::= exports SinglePkgName"); }  //$NON-NLS-1$
				consumeExportsHeader();
				break;

			case 154:
				if (DEBUG) { System.out.println("TargetModuleListopt ::= to TargetModuleNameList"); }  //$NON-NLS-1$
				consumeTargetModuleList();
				break;

			case 155:
				if (DEBUG) { System.out.println("TargetModuleName ::= UnannotatableName"); }  //$NON-NLS-1$
				consumeSingleTargetModuleName();
				break;

			case 157:
				if (DEBUG) { System.out.println("TargetModuleNameList ::= TargetModuleNameList COMMA..."); }  //$NON-NLS-1$
				consumeTargetModuleNameList();
				break;

			case 158:
				if (DEBUG) { System.out.println("SinglePkgName ::= UnannotatableName"); }  //$NON-NLS-1$
				consumeSinglePkgName();
				break;

			case 159:
				if (DEBUG) { System.out.println("OpensStatement ::= OpensHeader TargetModuleListopt..."); }  //$NON-NLS-1$
				consumeOpensStatement();
				break;

			case 160:
				if (DEBUG) { System.out.println("OpensHeader ::= opens SinglePkgName"); }  //$NON-NLS-1$
				consumeOpensHeader();
				break;

			case 161:
				if (DEBUG) { System.out.println("UsesStatement ::= UsesHeader SEMICOLON"); }  //$NON-NLS-1$
				consumeUsesStatement();
				break;

			case 162:
				if (DEBUG) { System.out.println("UsesHeader ::= uses Name"); }  //$NON-NLS-1$
				consumeUsesHeader();
				break;

			case 163:
				if (DEBUG) { System.out.println("ProvidesStatement ::= ProvidesInterface WithClause..."); }  //$NON-NLS-1$
				consumeProvidesStatement();
				break;

			case 164:
				if (DEBUG) { System.out.println("ProvidesInterface ::= provides Name"); }  //$NON-NLS-1$
				consumeProvidesInterface();
				break;

			case 165:
				if (DEBUG) { System.out.println("ServiceImplName ::= Name"); }  //$NON-NLS-1$
				consumeSingleServiceImplName();
				break;

			case 167:
				if (DEBUG) { System.out.println("ServiceImplNameList ::= ServiceImplNameList COMMA..."); }  //$NON-NLS-1$
				consumeServiceImplNameList();
				break;

			case 168:
				if (DEBUG) { System.out.println("WithClause ::= with ServiceImplNameList"); }  //$NON-NLS-1$
				consumeWithClause();
				break;

			case 169:
				if (DEBUG) { System.out.println("ReduceImports ::="); }  //$NON-NLS-1$
				consumeReduceImports();
				break;

			case 170:
				if (DEBUG) { System.out.println("EnterCompilationUnit ::="); }  //$NON-NLS-1$
				consumeEnterCompilationUnit();
				break;

			case 193:
				if (DEBUG) { System.out.println("CatchHeader ::= catch LPAREN CatchFormalParameter RPAREN"); }  //$NON-NLS-1$
				consumeCatchHeader();
				break;

			case 195:
				if (DEBUG) { System.out.println("ImportDeclarations ::= ImportDeclarations..."); }  //$NON-NLS-1$
				consumeImportDeclarations();
				break;

			case 197:
				if (DEBUG) { System.out.println("TypeDeclarations ::= TypeDeclarations TypeDeclaration"); }  //$NON-NLS-1$
				consumeTypeDeclarations();
				break;

			case 198:
				if (DEBUG) { System.out.println("PackageDeclaration ::= PackageDeclarationName SEMICOLON"); }  //$NON-NLS-1$
				consumePackageDeclaration();
				break;

			case 199:
				if (DEBUG) { System.out.println("PackageDeclarationName ::= Modifiers package..."); }  //$NON-NLS-1$
				consumePackageDeclarationNameWithModifiers();
				break;

			case 200:
				if (DEBUG) { System.out.println("PackageDeclarationName ::= PackageComment package Name"); }  //$NON-NLS-1$
				consumePackageDeclarationName();
				break;

			case 201:
				if (DEBUG) { System.out.println("PackageComment ::="); }  //$NON-NLS-1$
				consumePackageComment();
				break;

			case 206:
				if (DEBUG) { System.out.println("SingleTypeImportDeclaration ::=..."); }  //$NON-NLS-1$
				consumeImportDeclaration();
				break;

			case 207:
				if (DEBUG) { System.out.println("SingleTypeImportDeclarationName ::= import Name..."); }  //$NON-NLS-1$
				consumeSingleTypeImportDeclarationName();
				break;

			case 208:
				if (DEBUG) { System.out.println("TypeImportOnDemandDeclaration ::=..."); }  //$NON-NLS-1$
				consumeImportDeclaration();
				break;

			case 209:
				if (DEBUG) { System.out.println("TypeImportOnDemandDeclarationName ::= import Name DOT..."); }  //$NON-NLS-1$
				consumeTypeImportOnDemandDeclarationName();
				break;

			case 212:
				if (DEBUG) { System.out.println("TypeDeclaration ::= SEMICOLON"); }  //$NON-NLS-1$
				consumeEmptyTypeDeclaration();
				break;

			case 217:
				if (DEBUG) { System.out.println("Modifiers ::= Modifiers Modifier"); }  //$NON-NLS-1$
				consumeModifiers2();
				break;

			case 232:
				if (DEBUG) { System.out.println("Modifier ::= Annotation"); }  //$NON-NLS-1$
				consumeAnnotationAsModifier();
				break;

			case 245:
				if (DEBUG) { System.out.println("AspectDeclaration ::= AspectHeader AspectBody"); }  //$NON-NLS-1$
				consumeAspectDeclaration();
				break;

			case 246:
				if (DEBUG) { System.out.println("AspectHeader ::= AspectHeaderName ClassHeaderExtendsopt"); }  //$NON-NLS-1$
				consumeAspectHeader();
				break;

			case 249:
				if (DEBUG) { System.out.println("AspectHeaderName ::= AspectHeaderName1 TypeParameters"); }  //$NON-NLS-1$
				consumeAspectHeaderNameWithTypeParameters(false);
				break;

			case 250:
				if (DEBUG) { System.out.println("AspectHeaderName ::= AspectHeaderName2 TypeParameters"); }  //$NON-NLS-1$
				consumeAspectHeaderNameWithTypeParameters(true);
				break;

			case 251:
				if (DEBUG) { System.out.println("AspectHeaderName1 ::= Modifiersopt aspect Identifier"); }  //$NON-NLS-1$
				consumeAspectHeaderName(false);
				break;

			case 252:
				if (DEBUG) { System.out.println("AspectHeaderName2 ::= Modifiersopt privileged..."); }  //$NON-NLS-1$
				consumeAspectHeaderName(true);
				break;

			case 254:
				if (DEBUG) { System.out.println("AspectHeaderRest ::= AspectHeaderRestStart PseudoTokens"); }  //$NON-NLS-1$
				consumeAspectHeaderRest();
				break;

			case 255:
				if (DEBUG) { System.out.println("AspectHeaderRestStart ::= Identifier"); }  //$NON-NLS-1$
				consumePseudoTokenIdentifier();
				break;

			case 258:
				if (DEBUG) { System.out.println("AspectBodyDeclarations ::= AspectBodyDeclarations..."); }  //$NON-NLS-1$
				consumeClassBodyDeclarations();
				break;

			case 259:
				if (DEBUG) { System.out.println("AspectBodyDeclarationsopt ::="); }  //$NON-NLS-1$
				consumeEmptyClassBodyDeclarationsopt();
				break;

			case 260:
				if (DEBUG) { System.out.println("AspectBodyDeclarationsopt ::= NestedType..."); }  //$NON-NLS-1$
				consumeClassBodyDeclarationsopt();
				break;

			case 261:
				if (DEBUG) { System.out.println("AspectBodyDeclaration ::=..."); }  //$NON-NLS-1$
				consumeClassBodyDeclarationInAspect();
				break;

			case 265:
				if (DEBUG) { System.out.println("ClassBodyDeclarationNoAroundMethod ::= Diet NestedMethod"); }  //$NON-NLS-1$
				consumeClassBodyDeclaration();
				break;

			case 276:
				if (DEBUG) { System.out.println("ClassMemberDeclarationNoAroundMethod ::= SEMICOLON"); }  //$NON-NLS-1$
				consumeEmptyTypeDeclaration();
				break;

			case 278:
				if (DEBUG) { System.out.println("MethodDeclarationNoAround ::= MethodHeaderNoAround..."); }  //$NON-NLS-1$
				// set to true to consume a method with a body
				consumeMethodDeclaration(true, false);
				break;

			case 279:
				if (DEBUG) { System.out.println("AbstractMethodDeclarationNoAround ::=..."); }  //$NON-NLS-1$
				// set to false to consume a method without body
				consumeMethodDeclaration(false, false);
				break;

			case 280:
				if (DEBUG) { System.out.println("MethodHeaderNoAround ::= MethodHeaderNameNoAround..."); }  //$NON-NLS-1$
				consumeMethodHeader();
				break;

			case 281:
				if (DEBUG) { System.out.println("MethodHeaderNameNoAround ::= Modifiersopt TypeParameters"); }  //$NON-NLS-1$
				consumeMethodHeaderNameWithTypeParameters(false);
				break;

			case 282:
				if (DEBUG) { System.out.println("MethodHeaderNameNoAround ::= Modifiersopt Type..."); }  //$NON-NLS-1$
				consumeMethodHeaderName(false);
				break;

			case 283:
				if (DEBUG) { System.out.println("PointcutDeclaration ::= PointcutHeader..."); }  //$NON-NLS-1$
				consumeEmptyPointcutDeclaration();
				break;

			case 284:
				if (DEBUG) { System.out.println("PointcutDeclaration ::= PointcutHeader..."); }  //$NON-NLS-1$
				consumePointcutDeclaration();
				break;

			case 285:
				if (DEBUG) { System.out.println("PointcutHeader ::= Modifiersopt pointcut JavaIdentifier"); }  //$NON-NLS-1$
				consumePointcutHeader();
				break;

			case 288:
				if (DEBUG) { System.out.println("AroundDeclaration ::= AroundHeader MethodBody"); }  //$NON-NLS-1$
				consumeAroundDeclaration();
				break;

			case 289:
				if (DEBUG) { System.out.println("AroundHeader ::= AroundHeaderName FormalParameterListopt"); }  //$NON-NLS-1$
				consumeAroundHeader();
				break;

			case 290:
				if (DEBUG) { System.out.println("AroundHeaderName ::= Modifiersopt Type around LPAREN"); }  //$NON-NLS-1$
				consumeAroundHeaderName();
				break;

			case 291:
				if (DEBUG) { System.out.println("AroundHeaderName ::= Modifiersopt around LPAREN"); }  //$NON-NLS-1$
				consumeAroundHeaderNameMissingReturnType();
				break;

			case 292:
				if (DEBUG) { System.out.println("BasicAdviceDeclaration ::= BasicAdviceHeader MethodBody"); }  //$NON-NLS-1$
				consumeBasicAdviceDeclaration();
				break;

			case 295:
				if (DEBUG) { System.out.println("BeforeAdviceHeader ::= BeforeAdviceHeaderName..."); }  //$NON-NLS-1$
				consumeBasicAdviceHeader();
				break;

			case 296:
				if (DEBUG) { System.out.println("AfterAdviceHeader ::= AfterAdviceHeaderName..."); }  //$NON-NLS-1$
				consumeBasicAdviceHeader();
				break;

			case 297:
				if (DEBUG) { System.out.println("BeforeAdviceHeaderName ::= Modifiersopt before LPAREN"); }  //$NON-NLS-1$
				consumeBasicAdviceHeaderName(false);
				break;

			case 298:
				if (DEBUG) { System.out.println("AfterAdviceHeaderName ::= Modifiersopt after LPAREN"); }  //$NON-NLS-1$
				consumeBasicAdviceHeaderName(true);
				break;

			case 299:
				if (DEBUG) { System.out.println("ExtraParamopt ::= Identifier LPAREN FormalParameter..."); }  //$NON-NLS-1$
				consumeExtraParameterWithFormal();
				break;

			case 300:
				if (DEBUG) { System.out.println("ExtraParamopt ::= Identifier LPAREN RPAREN"); }  //$NON-NLS-1$
				consumeExtraParameterNoFormal();
				break;

			case 301:
				if (DEBUG) { System.out.println("ExtraParamopt ::= Identifier"); }  //$NON-NLS-1$
				consumeExtraParameterNoFormal();
				break;

			case 303:
				if (DEBUG) { System.out.println("OnType ::= JavaIdentifier"); }  //$NON-NLS-1$
				consumeZeroTypeAnnotations();
				break;

			case 304:
				if (DEBUG) { System.out.println("OnType ::= OnType DOT JavaIdentifier"); }  //$NON-NLS-1$
				consumeZeroTypeAnnotations();
				consumeQualifiedName();
				break;

			case 309:
				if (DEBUG) { System.out.println("InterTypeMethodDeclaration ::= InterTypeMethodHeader..."); }  //$NON-NLS-1$
				// set to true to consume a method with a body
				consumeInterTypeMethodDeclaration(true);
				break;

			case 310:
				if (DEBUG) { System.out.println("InterTypeMethodHeader ::= InterTypeMethodHeaderName..."); }  //$NON-NLS-1$
				consumeInterTypeMethodHeader();
				break;

			case 311:
				if (DEBUG) { System.out.println("InterTypeMethodHeaderName ::= Modifiersopt Type OnType"); }  //$NON-NLS-1$
				consumeInterTypeMethodHeaderName(false, false);
				break;

			case 312:
				if (DEBUG) { System.out.println("InterTypeMethodHeaderName ::= Modifiersopt Type OnType"); }  //$NON-NLS-1$
				consumeInterTypeMethodHeaderNameIllegallyUsingTypePattern("*");
				break;

			case 313:
				if (DEBUG) { System.out.println("InterTypeMethodHeaderName ::= Modifiersopt Type OnType"); }  //$NON-NLS-1$
				consumeInterTypeMethodHeaderNameIllegallyUsingTypePattern("+");
				break;

			case 314:
				if (DEBUG) { System.out.println("InterTypeMethodHeaderName ::= Modifiersopt Type OnType"); }  //$NON-NLS-1$
				consumeInterTypeMethodHeaderName(false, true);
				break;

			case 315:
				if (DEBUG) { System.out.println("InterTypeMethodHeaderName ::= Modifiersopt..."); }  //$NON-NLS-1$
				consumeInterTypeMethodHeaderName(true, false);
				break;

			case 316:
				if (DEBUG) { System.out.println("InterTypeMethodHeaderName ::= Modifiersopt..."); }  //$NON-NLS-1$
				consumeInterTypeMethodHeaderName(true, true);
				break;

			case 317:
				if (DEBUG) { System.out.println("AbstractInterTypeMethodDeclaration ::=..."); }  //$NON-NLS-1$
				// set to false to consume a method without body
				consumeInterTypeMethodDeclaration(false);
				break;

			case 318:
				if (DEBUG) { System.out.println("TypeParametersAsReference ::= TypeParameters"); }  //$NON-NLS-1$
				convertTypeParametersToSingleTypeReferences();
				break;

			case 319:
				if (DEBUG) { System.out.println("InterTypeConstructorDeclaration ::=..."); }  //$NON-NLS-1$
				// set to true to consume a method with a body
				consumeInterTypeConstructorDeclaration();
				break;

			case 320:
				if (DEBUG) { System.out.println("InterTypeConstructorHeader ::=..."); }  //$NON-NLS-1$
				consumeInterTypeConstructorHeader();
				break;

			case 321:
				if (DEBUG) { System.out.println("InterTypeConstructorHeaderName ::= Modifiersopt Name DOT"); }  //$NON-NLS-1$
				consumeInterTypeConstructorHeaderName(false, false);
				break;

			case 322:
				if (DEBUG) { System.out.println("InterTypeConstructorHeaderName ::= Modifiersopt Name DOT"); }  //$NON-NLS-1$
				consumeInterTypeConstructorHeaderNameIllegallyUsingTypePattern("*");
				break;

			case 323:
				if (DEBUG) { System.out.println("InterTypeConstructorHeaderName ::= Modifiersopt Name..."); }  //$NON-NLS-1$
				consumeInterTypeConstructorHeaderNameIllegallyUsingTypePattern("+");
				break;

			case 324:
				if (DEBUG) { System.out.println("InterTypeConstructorHeaderName ::= Modifiersopt..."); }  //$NON-NLS-1$
				consumeInterTypeConstructorHeaderName(true, false);
				break;

			case 325:
				if (DEBUG) { System.out.println("InterTypeConstructorHeaderName ::= Modifiersopt..."); }  //$NON-NLS-1$
				consumeInterTypeConstructorHeaderName(false, true);
				break;

			case 326:
				if (DEBUG) { System.out.println("InterTypeConstructorHeaderName ::= Modifiersopt..."); }  //$NON-NLS-1$
				consumeInterTypeConstructorHeaderName(true, true);
				break;

			case 327:
				if (DEBUG) { System.out.println("InterTypeFieldDeclaration ::= InterTypeFieldHeader..."); }  //$NON-NLS-1$
				consumeInterTypeFieldDeclaration();
				break;

			case 328:
				if (DEBUG) { System.out.println("InterTypeFieldHeader ::= Modifiersopt Type OnType DOT..."); }  //$NON-NLS-1$
				consumeInterTypeFieldHeader(false);
				break;

			case 329:
				if (DEBUG) { System.out.println("InterTypeFieldHeader ::= Modifiersopt Type OnType DOT..."); }  //$NON-NLS-1$
				consumeInterTypeFieldHeaderIllegallyAttemptingToUseATypePattern("*");
				break;

			case 330:
				if (DEBUG) { System.out.println("InterTypeFieldHeader ::= Modifiersopt Type OnType PLUS"); }  //$NON-NLS-1$
				consumeInterTypeFieldHeaderIllegallyAttemptingToUseATypePattern("+");
				break;

			case 331:
				if (DEBUG) { System.out.println("InterTypeFieldHeader ::= Modifiersopt Type OnType..."); }  //$NON-NLS-1$
				consumeInterTypeFieldHeader(true);
				break;

			case 332:
				if (DEBUG) { System.out.println("InterTypeFieldBody ::="); }  //$NON-NLS-1$
				consumeExitITDVariableWithoutInitializer();
				break;

			case 333:
				if (DEBUG) { System.out.println("InterTypeFieldBody ::= EQUAL ForceNoDiet..."); }  //$NON-NLS-1$
				consumeExitITDVariableWithInitializer();
				break;

			case 335:
				if (DEBUG) { System.out.println("DeclareDeclaration ::= DeclareHeader PseudoTokens..."); }  //$NON-NLS-1$
				consumeDeclareDeclaration();
				break;

			case 336:
				if (DEBUG) { System.out.println("DeclareHeader ::= declare Identifier COLON"); }  //$NON-NLS-1$
				consumeDeclareHeader();
				break;

			case 337:
				if (DEBUG) { System.out.println("DeclareDeclaration ::= DeclareAnnotationHeader..."); }  //$NON-NLS-1$
				consumeDeclareAnnotation(' ');
				break;

			case 338:
				if (DEBUG) { System.out.println("DeclareDeclaration ::= DeclareAnnotationHeader..."); }  //$NON-NLS-1$
				consumeDeclareAnnotation('+');
				break;

			case 339:
				if (DEBUG) { System.out.println("DeclareDeclaration ::= DeclareAnnotationHeader..."); }  //$NON-NLS-1$
				consumeDeclareAnnotation('-');
				break;

			case 342:
				if (DEBUG) { System.out.println("DeclareAnnotationHeader ::= declare AT Identifier COLON"); }  //$NON-NLS-1$
				consumeDeclareAnnotationHeader();
				break;

			case 345:
				if (DEBUG) { System.out.println("PseudoTokens ::= PseudoTokens ColonPseudoToken"); }  //$NON-NLS-1$
				consumePseudoTokens();
				break;

			case 346:
				if (DEBUG) { System.out.println("PseudoTokens ::= PseudoTokens PseudoToken"); }  //$NON-NLS-1$
				consumePseudoTokens();
				break;

			case 348:
				if (DEBUG) { System.out.println("PseudoTokensNoColon ::= PseudoTokensNoColon PseudoToken"); }  //$NON-NLS-1$
				consumePseudoTokens();
				break;

			case 349:
				if (DEBUG) { System.out.println("ColonPseudoToken ::= COLON"); }  //$NON-NLS-1$
				consumePseudoToken(":");
				break;

			case 350:
				if (DEBUG) { System.out.println("PseudoToken ::= JavaIdentifier"); }  //$NON-NLS-1$
				consumePseudoTokenIdentifier();
				break;

			case 351:
				if (DEBUG) { System.out.println("PseudoToken ::= LPAREN"); }  //$NON-NLS-1$
				consumePseudoToken("(");
				break;

			case 352:
				if (DEBUG) { System.out.println("PseudoToken ::= RPAREN"); }  //$NON-NLS-1$
				consumePseudoToken(")");
				break;

			case 353:
				if (DEBUG) { System.out.println("PseudoToken ::= DOT"); }  //$NON-NLS-1$
				consumePseudoToken(".");
				break;

			case 354:
				if (DEBUG) { System.out.println("PseudoToken ::= MULTIPLY"); }  //$NON-NLS-1$
				consumePseudoToken("*");
				break;

			case 355:
				if (DEBUG) { System.out.println("PseudoToken ::= PLUS"); }  //$NON-NLS-1$
				consumePseudoToken("+");
				break;

			case 356:
				if (DEBUG) { System.out.println("PseudoToken ::= EQUAL"); }  //$NON-NLS-1$
				consumePseudoToken("=");
				break;

			case 357:
				if (DEBUG) { System.out.println("PseudoToken ::= AND_AND"); }  //$NON-NLS-1$
				consumePseudoToken("&&");
				break;

			case 358:
				if (DEBUG) { System.out.println("PseudoToken ::= OR_OR"); }  //$NON-NLS-1$
				consumePseudoToken("||");
				break;

			case 359:
				if (DEBUG) { System.out.println("PseudoToken ::= NOT"); }  //$NON-NLS-1$
				consumePseudoToken("!");
				break;

			case 360:
				if (DEBUG) { System.out.println("PseudoToken ::= COMMA"); }  //$NON-NLS-1$
				consumePseudoToken(",");
				break;

			case 361:
				if (DEBUG) { System.out.println("PseudoToken ::= LBRACKET"); }  //$NON-NLS-1$
				consumePseudoToken("[");
				break;

			case 362:
				if (DEBUG) { System.out.println("PseudoToken ::= RBRACKET"); }  //$NON-NLS-1$
				consumePseudoToken("]");
				break;

			case 363:
				if (DEBUG) { System.out.println("PseudoToken ::= AT"); }  //$NON-NLS-1$
				consumePseudoToken("@");
				break;

			case 364:
				if (DEBUG) { System.out.println("PseudoToken ::= ELLIPSIS"); }  //$NON-NLS-1$
				consumePseudoToken("...");
				break;

			case 365:
				if (DEBUG) { System.out.println("PseudoToken ::= QUESTION"); }  //$NON-NLS-1$
				consumePseudoToken("?");
				break;

			case 366:
				if (DEBUG) { System.out.println("PseudoToken ::= LESS"); }  //$NON-NLS-1$
				consumePseudoToken("<");
				break;

			case 367:
				if (DEBUG) { System.out.println("PseudoToken ::= GREATER"); }  //$NON-NLS-1$
				consumePseudoToken(">");
				break;

			case 368:
				if (DEBUG) { System.out.println("PseudoToken ::= RIGHT_SHIFT"); }  //$NON-NLS-1$
				consumePseudoToken(">>");
				break;

			case 369:
				if (DEBUG) { System.out.println("PseudoToken ::= UNSIGNED_RIGHT_SHIFT"); }  //$NON-NLS-1$
				consumePseudoToken(">>>");
				break;

			case 370:
				if (DEBUG) { System.out.println("PseudoToken ::= AND"); }  //$NON-NLS-1$
				consumePseudoToken("&");
				break;

			case 371:
				if (DEBUG) { System.out.println("PseudoToken ::= NOT_EQUAL"); }  //$NON-NLS-1$
				consumePseudoToken("!=");
				break;

			case 372:
				if (DEBUG) { System.out.println("PseudoToken ::= PrimitiveType"); }  //$NON-NLS-1$
				consumePseudoTokenPrimitiveType();
				break;

			case 373:
				if (DEBUG) { System.out.println("PseudoToken ::= SimpleModifier"); }  //$NON-NLS-1$
				consumePseudoTokenModifier();
				break;

			case 374:
				if (DEBUG) { System.out.println("PseudoToken ::= Literal"); }  //$NON-NLS-1$
				consumePseudoTokenLiteral();
				break;

			case 375:
				if (DEBUG) { System.out.println("PseudoToken ::= this"); }  //$NON-NLS-1$
				consumePseudoToken("this", 1, true);
				break;

			case 376:
				if (DEBUG) { System.out.println("PseudoToken ::= class"); }  //$NON-NLS-1$
				consumePseudoToken("class", 1, true);
				break;

			case 377:
				if (DEBUG) { System.out.println("PseudoToken ::= super"); }  //$NON-NLS-1$
				consumePseudoToken("super", 1, true);
				break;

			case 378:
				if (DEBUG) { System.out.println("PseudoToken ::= if LPAREN Expression RPAREN"); }  //$NON-NLS-1$
				consumePseudoTokenIf();
				break;

			case 379:
				if (DEBUG) { System.out.println("PseudoToken ::= assert"); }  //$NON-NLS-1$
				consumePseudoToken("assert", 1, true);
				break;

			case 380:
				if (DEBUG) { System.out.println("PseudoToken ::= import"); }  //$NON-NLS-1$
				consumePseudoToken("import", 1, true);
				break;

			case 381:
				if (DEBUG) { System.out.println("PseudoToken ::= package"); }  //$NON-NLS-1$
				consumePseudoToken("package", 1, true);
				break;

			case 382:
				if (DEBUG) { System.out.println("PseudoToken ::= throw"); }  //$NON-NLS-1$
				consumePseudoToken("throw", 1, true);
				break;

			case 383:
				if (DEBUG) { System.out.println("PseudoToken ::= new"); }  //$NON-NLS-1$
				consumePseudoToken("new", 1, true);
				break;

			case 384:
				if (DEBUG) { System.out.println("PseudoToken ::= do"); }  //$NON-NLS-1$
				consumePseudoToken("do", 1, true);
				break;

			case 385:
				if (DEBUG) { System.out.println("PseudoToken ::= for"); }  //$NON-NLS-1$
				consumePseudoToken("for", 1, true);
				break;

			case 386:
				if (DEBUG) { System.out.println("PseudoToken ::= switch"); }  //$NON-NLS-1$
				consumePseudoToken("switch", 1, true);
				break;

			case 387:
				if (DEBUG) { System.out.println("PseudoToken ::= try"); }  //$NON-NLS-1$
				consumePseudoToken("try", 1, true);
				break;

			case 388:
				if (DEBUG) { System.out.println("PseudoToken ::= while"); }  //$NON-NLS-1$
				consumePseudoToken("while", 1, true);
				break;

			case 389:
				if (DEBUG) { System.out.println("PseudoToken ::= break"); }  //$NON-NLS-1$
				consumePseudoToken("break", 1, true);
				break;

			case 390:
				if (DEBUG) { System.out.println("PseudoToken ::= continue"); }  //$NON-NLS-1$
				consumePseudoToken("continue", 1, true);
				break;

			case 391:
				if (DEBUG) { System.out.println("PseudoToken ::= return"); }  //$NON-NLS-1$
				consumePseudoToken("return", 1, true);
				break;

			case 392:
				if (DEBUG) { System.out.println("PseudoToken ::= case"); }  //$NON-NLS-1$
				consumePseudoToken("case", 1, true);
				break;

			case 393:
				if (DEBUG) { System.out.println("PseudoToken ::= catch"); }  //$NON-NLS-1$
				consumePseudoToken("catch", 0, true);
				break;

			case 394:
				if (DEBUG) { System.out.println("PseudoToken ::= instanceof"); }  //$NON-NLS-1$
				consumePseudoToken("instanceof", 0, true);
				break;

			case 395:
				if (DEBUG) { System.out.println("PseudoToken ::= else"); }  //$NON-NLS-1$
				consumePseudoToken("else", 0, true);
				break;

			case 396:
				if (DEBUG) { System.out.println("PseudoToken ::= extends"); }  //$NON-NLS-1$
				consumePseudoToken("extends", 0, true);
				break;

			case 397:
				if (DEBUG) { System.out.println("PseudoToken ::= finally"); }  //$NON-NLS-1$
				consumePseudoToken("finally", 0, true);
				break;

			case 398:
				if (DEBUG) { System.out.println("PseudoToken ::= implements"); }  //$NON-NLS-1$
				consumePseudoToken("implements", 0, true);
				break;

			case 399:
				if (DEBUG) { System.out.println("PseudoToken ::= throws"); }  //$NON-NLS-1$
				consumePseudoToken("throws", 0, true);
				break;

			case 400:
				if (DEBUG) { System.out.println("ClassDeclaration ::= ClassHeader ClassBody"); }  //$NON-NLS-1$
				consumeClassDeclaration();
				break;

			case 401:
				if (DEBUG) { System.out.println("IntertypeClassDeclaration ::= IntertypeClassHeader..."); }  //$NON-NLS-1$
				consumeIntertypeClassDeclaration();
				break;

			case 402:
				if (DEBUG) { System.out.println("IntertypeClassHeader ::= IntertypeClassHeaderName..."); }  //$NON-NLS-1$
				consumeIntertypeClassHeader();
				break;

			case 403:
				if (DEBUG) { System.out.println("IntertypeClassHeaderName ::= IntertypeClassHeaderName1"); }  //$NON-NLS-1$
				consumeIntertypeTypeHeaderNameWithTypeParameters();
				break;

			case 405:
				if (DEBUG) { System.out.println("IntertypeClassHeaderName1 ::= Modifiersopt class OnType"); }  //$NON-NLS-1$
				consumeIntertypeClassHeaderName(false);
				break;

			case 406:
				if (DEBUG) { System.out.println("InterTypeClassHeaderName1 ::= Modifiersopt class OnType"); }  //$NON-NLS-1$
				consumeIntertypeClassHeaderName(true);
				break;

			case 407:
				if (DEBUG) { System.out.println("ClassHeader ::= ClassHeaderName ClassHeaderExtendsopt..."); }  //$NON-NLS-1$
				consumeClassHeader();
				break;

			case 408:
				if (DEBUG) { System.out.println("ClassHeaderName ::= ClassHeaderName1 TypeParameters"); }  //$NON-NLS-1$
				consumeTypeHeaderNameWithTypeParameters();
				break;

			case 410:
				if (DEBUG) { System.out.println("ClassHeaderName1 ::= Modifiersopt class JavaIdentifier"); }  //$NON-NLS-1$
				consumeClassHeaderName1();
				break;

			case 411:
				if (DEBUG) { System.out.println("ClassHeaderExtends ::= extends ClassType"); }  //$NON-NLS-1$
				consumeClassHeaderExtends();
				break;

			case 412:
				if (DEBUG) { System.out.println("ClassHeaderImplements ::= implements InterfaceTypeList"); }  //$NON-NLS-1$
				consumeClassHeaderImplements();
				break;

			case 414:
				if (DEBUG) { System.out.println("InterfaceTypeList ::= InterfaceTypeList COMMA..."); }  //$NON-NLS-1$
				consumeInterfaceTypeList();
				break;

			case 415:
				if (DEBUG) { System.out.println("InterfaceType ::= ClassOrInterfaceType"); }  //$NON-NLS-1$
				consumeInterfaceType();
				break;

			case 418:
				if (DEBUG) { System.out.println("ClassBodyDeclarations ::= ClassBodyDeclarations..."); }  //$NON-NLS-1$
				consumeClassBodyDeclarations();
				break;

			case 422:
				if (DEBUG) { System.out.println("ClassBodyDeclaration ::= Diet NestedMethod..."); }  //$NON-NLS-1$
				consumeClassBodyDeclaration();
				break;

			case 423:
				if (DEBUG) { System.out.println("Diet ::="); }  //$NON-NLS-1$
				consumeDiet();
				break;

			case 424:
				if (DEBUG) { System.out.println("Initializer ::= Diet NestedMethod CreateInitializer..."); }  //$NON-NLS-1$
				consumeClassBodyDeclaration();
				break;

			case 425:
				if (DEBUG) { System.out.println("CreateInitializer ::="); }  //$NON-NLS-1$
				consumeCreateInitializer();
				break;

			case 433:
				if (DEBUG) { System.out.println("ClassMemberDeclaration ::= SEMICOLON"); }  //$NON-NLS-1$
				consumeEmptyTypeDeclaration();
				break;

			case 436:
				if (DEBUG) { System.out.println("FieldDeclaration ::= Modifiersopt Type..."); }  //$NON-NLS-1$
				consumeFieldDeclaration();
				break;

			case 438:
				if (DEBUG) { System.out.println("VariableDeclarators ::= VariableDeclarators COMMA..."); }  //$NON-NLS-1$
				consumeVariableDeclarators();
				break;

			case 441:
				if (DEBUG) { System.out.println("EnterVariable ::="); }  //$NON-NLS-1$
				consumeEnterVariable();
				break;

			case 442:
				if (DEBUG) { System.out.println("ExitVariableWithInitialization ::="); }  //$NON-NLS-1$
				consumeExitVariableWithInitialization();
				break;

			case 443:
				if (DEBUG) { System.out.println("ExitVariableWithoutInitialization ::="); }  //$NON-NLS-1$
				consumeExitVariableWithoutInitialization();
				break;

			case 444:
				if (DEBUG) { System.out.println("ForceNoDiet ::="); }  //$NON-NLS-1$
				consumeForceNoDiet();
				break;

			case 445:
				if (DEBUG) { System.out.println("RestoreDiet ::="); }  //$NON-NLS-1$
				consumeRestoreDiet();
				break;

			case 450:
				if (DEBUG) { System.out.println("MethodDeclaration ::= MethodHeader MethodBody"); }  //$NON-NLS-1$
				// set to true to consume a method with a body
				consumeMethodDeclaration(true, false);
				break;

			case 451:
				if (DEBUG) { System.out.println("MethodDeclaration ::= DefaultMethodHeader MethodBody"); }  //$NON-NLS-1$
				// set to true to consume a method with a body
				consumeMethodDeclaration(true, true);
				break;

			case 452:
				if (DEBUG) { System.out.println("AbstractMethodDeclaration ::= MethodHeader SEMICOLON"); }  //$NON-NLS-1$
				// set to false to consume a method without body
				consumeMethodDeclaration(false, false);
				break;

			case 453:
				if (DEBUG) { System.out.println("MethodHeader ::= MethodHeaderName FormalParameterListopt"); }  //$NON-NLS-1$
				consumeMethodHeader();
				break;

			case 454:
				if (DEBUG) { System.out.println("DefaultMethodHeader ::= DefaultMethodHeaderName..."); }  //$NON-NLS-1$
				consumeMethodHeader();
				break;

			case 455:
				if (DEBUG) { System.out.println("MethodHeaderName ::= Modifiersopt TypeParameters Type..."); }  //$NON-NLS-1$
				consumeMethodHeaderNameWithTypeParameters(false);
				break;

			case 456:
				if (DEBUG) { System.out.println("MethodHeaderName ::= Modifiersopt Type JavaIdentifier..."); }  //$NON-NLS-1$
				consumeMethodHeaderName(false);
				break;

			case 457:
				if (DEBUG) { System.out.println("DefaultMethodHeaderName ::= ModifiersWithDefault..."); }  //$NON-NLS-1$
				consumeMethodHeaderNameWithTypeParameters(false);
				break;

			case 458:
				if (DEBUG) { System.out.println("DefaultMethodHeaderName ::= ModifiersWithDefault Type..."); }  //$NON-NLS-1$
				consumeMethodHeaderName(false);
				break;

			case 459:
				if (DEBUG) { System.out.println("ModifiersWithDefault ::= Modifiersopt default..."); }  //$NON-NLS-1$
				consumePushCombineModifiers();
				break;

			case 460:
				if (DEBUG) { System.out.println("MethodHeaderRightParen ::= RPAREN"); }  //$NON-NLS-1$
				consumeMethodHeaderRightParen();
				break;

			case 461:
				if (DEBUG) { System.out.println("MethodHeaderExtendedDims ::= Dimsopt"); }  //$NON-NLS-1$
				consumeMethodHeaderExtendedDims();
				break;

			case 462:
				if (DEBUG) { System.out.println("MethodHeaderThrowsClause ::= throws ClassTypeList"); }  //$NON-NLS-1$
				consumeMethodHeaderThrowsClause();
				break;

			case 463:
				if (DEBUG) { System.out.println("ConstructorHeader ::= ConstructorHeaderName..."); }  //$NON-NLS-1$
				consumeConstructorHeader();
				break;

			case 464:
				if (DEBUG) { System.out.println("ConstructorHeaderName ::= Modifiersopt TypeParameters..."); }  //$NON-NLS-1$
				consumeConstructorHeaderNameWithTypeParameters();
				break;

			case 465:
				if (DEBUG) { System.out.println("ConstructorHeaderName ::= Modifiersopt Identifier LPAREN"); }  //$NON-NLS-1$
				consumeConstructorHeaderName();
				break;

			case 466:
				if (DEBUG) { System.out.println("ConstructorHeaderName ::= Modifiersopt aspect LPAREN"); }  //$NON-NLS-1$
				consumeConstructorHeaderName();
				break;

			case 468:
				if (DEBUG) { System.out.println("FormalParameterList ::= FormalParameterList COMMA..."); }  //$NON-NLS-1$
				consumeFormalParameterList();
				break;

			case 469:
				if (DEBUG) { System.out.println("FormalParameter ::= Modifiersopt Type..."); }  //$NON-NLS-1$
				consumeFormalParameter(false);
				break;

			case 470:
				if (DEBUG) { System.out.println("FormalParameter ::= Modifiersopt Type..."); }  //$NON-NLS-1$
				consumeFormalParameter(true);
				break;

			case 471:
				if (DEBUG) { System.out.println("FormalParameter ::= Modifiersopt Type AT308DOTDOTDOT..."); }  //$NON-NLS-1$
				consumeFormalParameter(true);
				break;

			case 472:
				if (DEBUG) { System.out.println("CatchFormalParameter ::= Modifiersopt CatchType..."); }  //$NON-NLS-1$
				consumeCatchFormalParameter();
				break;

			case 473:
				if (DEBUG) { System.out.println("CatchType ::= UnionType"); }  //$NON-NLS-1$
				consumeCatchType();
				break;

			case 474:
				if (DEBUG) { System.out.println("UnionType ::= Type"); }  //$NON-NLS-1$
				consumeUnionTypeAsClassType();
				break;

			case 475:
				if (DEBUG) { System.out.println("UnionType ::= UnionType OR Type"); }  //$NON-NLS-1$
				consumeUnionType();
				break;

			case 477:
				if (DEBUG) { System.out.println("ClassTypeList ::= ClassTypeList COMMA ClassTypeElt"); }  //$NON-NLS-1$
				consumeClassTypeList();
				break;

			case 478:
				if (DEBUG) { System.out.println("ClassTypeElt ::= ClassType"); }  //$NON-NLS-1$
				consumeClassTypeElt();
				break;

			case 479:
				if (DEBUG) { System.out.println("MethodBody ::= NestedMethod LBRACE BlockStatementsopt..."); }  //$NON-NLS-1$
				consumeMethodBody();
				break;

			case 480:
				if (DEBUG) { System.out.println("NestedMethod ::="); }  //$NON-NLS-1$
				consumeNestedMethod();
				break;

			case 481:
				if (DEBUG) { System.out.println("StaticInitializer ::= StaticOnly Block"); }  //$NON-NLS-1$
				consumeStaticInitializer();
				break;

			case 482:
				if (DEBUG) { System.out.println("StaticOnly ::= static"); }  //$NON-NLS-1$
				consumeStaticOnly();
				break;

			case 483:
				if (DEBUG) { System.out.println("ConstructorDeclaration ::= ConstructorHeader MethodBody"); }  //$NON-NLS-1$
				consumeConstructorDeclaration();
				break;

			case 484:
				if (DEBUG) { System.out.println("ConstructorDeclaration ::= ConstructorHeader SEMICOLON"); }  //$NON-NLS-1$
				consumeInvalidConstructorDeclaration();
				break;

			case 485:
				if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= this LPAREN..."); }  //$NON-NLS-1$
				consumeExplicitConstructorInvocation(0, THIS_CALL);
				break;

			case 486:
				if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= OnlyTypeArguments this"); }  //$NON-NLS-1$
				consumeExplicitConstructorInvocationWithTypeArguments(0, THIS_CALL);
				break;

			case 487:
				if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= super LPAREN..."); }  //$NON-NLS-1$
				consumeExplicitConstructorInvocation(0, SUPER_CALL);
				break;

			case 488:
				if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= OnlyTypeArguments..."); }  //$NON-NLS-1$
				consumeExplicitConstructorInvocationWithTypeArguments(0, SUPER_CALL);
				break;

			case 489:
				if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Primary DOT super..."); }  //$NON-NLS-1$
				consumeExplicitConstructorInvocation(1, SUPER_CALL);
				break;

			case 490:
				if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Primary DOT..."); }  //$NON-NLS-1$
				consumeExplicitConstructorInvocationWithTypeArguments(1, SUPER_CALL);
				break;

			case 491:
				if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Name DOT super LPAREN"); }  //$NON-NLS-1$
				consumeExplicitConstructorInvocation(2, SUPER_CALL);
				break;

			case 492:
				if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Name DOT..."); }  //$NON-NLS-1$
				consumeExplicitConstructorInvocationWithTypeArguments(2, SUPER_CALL);
				break;

			case 493:
				if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Primary DOT this..."); }  //$NON-NLS-1$
				consumeExplicitConstructorInvocation(1, THIS_CALL);
				break;

			case 494:
				if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Primary DOT..."); }  //$NON-NLS-1$
				consumeExplicitConstructorInvocationWithTypeArguments(1, THIS_CALL);
				break;

			case 495:
				if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Name DOT this LPAREN"); }  //$NON-NLS-1$
				consumeExplicitConstructorInvocation(2, THIS_CALL);
				break;

			case 496:
				if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Name DOT..."); }  //$NON-NLS-1$
				consumeExplicitConstructorInvocationWithTypeArguments(2, THIS_CALL);
				break;

			case 497:
				if (DEBUG) { System.out.println("InterfaceDeclaration ::= InterfaceHeader InterfaceBody"); }  //$NON-NLS-1$
				consumeInterfaceDeclaration();
				break;

			case 498:
				if (DEBUG) { System.out.println("InterfaceHeader ::= InterfaceHeaderName..."); }  //$NON-NLS-1$
				consumeInterfaceHeader();
				break;

			case 499:
				if (DEBUG) { System.out.println("InterfaceHeaderName ::= InterfaceHeaderName1..."); }  //$NON-NLS-1$
				consumeTypeHeaderNameWithTypeParameters();
				break;

			case 501:
				if (DEBUG) { System.out.println("InterfaceHeaderName1 ::= Modifiersopt interface..."); }  //$NON-NLS-1$
				consumeInterfaceHeaderName1();
				break;

			case 502:
				if (DEBUG) { System.out.println("InterfaceHeaderExtends ::= extends InterfaceTypeList"); }  //$NON-NLS-1$
				consumeInterfaceHeaderExtends();
				break;

			case 505:
				if (DEBUG) { System.out.println("InterfaceMemberDeclarations ::=..."); }  //$NON-NLS-1$
				consumeInterfaceMemberDeclarations();
				break;

			case 506:
				if (DEBUG) { System.out.println("InterfaceMemberDeclaration ::= SEMICOLON"); }  //$NON-NLS-1$
				consumeEmptyTypeDeclaration();
				break;

			case 508:
				if (DEBUG) { System.out.println("InterfaceMemberDeclaration ::= DefaultMethodHeader..."); }  //$NON-NLS-1$
				consumeInterfaceMethodDeclaration(false);
				break;

			case 509:
				if (DEBUG) { System.out.println("InterfaceMemberDeclaration ::= MethodHeader MethodBody"); }  //$NON-NLS-1$
				consumeInterfaceMethodDeclaration(false);
				break;

			case 510:
				if (DEBUG) { System.out.println("InterfaceMemberDeclaration ::= DefaultMethodHeader..."); }  //$NON-NLS-1$
				consumeInterfaceMethodDeclaration(true);
				break;

			case 511:
				if (DEBUG) { System.out.println("InvalidConstructorDeclaration ::= ConstructorHeader..."); }  //$NON-NLS-1$
				consumeInvalidConstructorDeclaration(true);
				break;

			case 512:
				if (DEBUG) { System.out.println("InvalidConstructorDeclaration ::= ConstructorHeader..."); }  //$NON-NLS-1$
				consumeInvalidConstructorDeclaration(false);
				break;

			case 523:
				if (DEBUG) { System.out.println("RecordDeclaration ::= RecordHeaderPart RecordBody"); }  //$NON-NLS-1$
				consumeRecordDeclaration();
				break;

			case 524:
				if (DEBUG) { System.out.println("RecordHeaderPart ::= RecordHeaderName RecordHeader..."); }  //$NON-NLS-1$
				consumeRecordHeaderPart();
				break;

			case 525:
				if (DEBUG) { System.out.println("RecordHeaderName ::= RecordHeaderName1 TypeParameters"); }  //$NON-NLS-1$
				consumeRecordHeaderNameWithTypeParameters();
				break;

			case 527:
				if (DEBUG) { System.out.println("RecordHeaderName1 ::= Modifiersopt..."); }  //$NON-NLS-1$
				consumeRecordHeaderName1();
				break;

			case 528:
				if (DEBUG) { System.out.println("RecordComponentHeaderRightParen ::= RPAREN"); }  //$NON-NLS-1$
				consumeRecordComponentHeaderRightParen();
				break;

			case 529:
				if (DEBUG) { System.out.println("RecordHeader ::= LPAREN RecordComponentsopt..."); }  //$NON-NLS-1$
				consumeRecordHeader();
				break;

			case 530:
				if (DEBUG) { System.out.println("RecordComponentsopt ::="); }  //$NON-NLS-1$
				consumeRecordComponentsopt();
				break;

			case 533:
				if (DEBUG) { System.out.println("RecordComponents ::= RecordComponents COMMA..."); }  //$NON-NLS-1$
				consumeRecordComponents();
				break;

			case 535:
				if (DEBUG) { System.out.println("RecordComponent ::= Modifiersopt Type..."); }  //$NON-NLS-1$
				consumeRecordComponent(false);
				break;

			case 536:
				if (DEBUG) { System.out.println("VariableArityRecordComponent ::= Modifiersopt Type..."); }  //$NON-NLS-1$
				consumeRecordComponent(true);
				break;

			case 537:
				if (DEBUG) { System.out.println("VariableArityRecordComponent ::= Modifiersopt Type..."); }  //$NON-NLS-1$
				consumeRecordComponent(true);
				break;

			case 538:
				if (DEBUG) { System.out.println("RecordBody ::= LBRACE RecordBodyDeclarationopt RBRACE"); }  //$NON-NLS-1$
				consumeRecordBody();
				break;

			case 539:
				if (DEBUG) { System.out.println("RecordBodyDeclarationopt ::="); }  //$NON-NLS-1$
				consumeEmptyRecordBodyDeclaration();
				break;

			case 542:
				if (DEBUG) { System.out.println("RecordBodyDeclarations ::= RecordBodyDeclarations..."); }  //$NON-NLS-1$
				consumeRecordBodyDeclarations();
				break;

			case 543:
				if (DEBUG) { System.out.println("RecordBodyDeclaration ::= ClassBodyDeclaration"); }  //$NON-NLS-1$
				consumeRecordBodyDeclaration();
				break;

			case 544:
				if (DEBUG) { System.out.println("RecordBodyDeclaration ::= CompactConstructorDeclaration"); }  //$NON-NLS-1$
				consumeRecordBodyDeclaration();
				break;

			case 545:
				if (DEBUG) { System.out.println("CompactConstructorDeclaration ::=..."); }  //$NON-NLS-1$
				consumeCompactConstructorDeclaration();
				break;

			case 546:
				if (DEBUG) { System.out.println("CompactConstructorHeader ::=..."); }  //$NON-NLS-1$
				consumeCompactConstructorHeader();
				break;

			case 547:
				if (DEBUG) { System.out.println("CompactConstructorHeaderName ::= Modifiersopt..."); }  //$NON-NLS-1$
				consumeCompactConstructorHeaderName();
				break;

			case 548:
				if (DEBUG) { System.out.println("CompactConstructorHeaderName ::= Modifiersopt..."); }  //$NON-NLS-1$
				consumeCompactConstructorHeaderNameWithTypeParameters();
				break;

			case 550:
				if (DEBUG) { System.out.println("InstanceofExpression ::= InstanceofExpression..."); }  //$NON-NLS-1$
				consumeInstanceOfExpression();
				break;

			case 552:
				if (DEBUG) { System.out.println("InstanceofRHS -> InstanceofPattern"); }  //$NON-NLS-1$
				consumeInstanceOfRHS();
				break;

			case 553:
				if (DEBUG) { System.out.println("InstanceofClassic ::= instanceof Modifiersopt Type"); }  //$NON-NLS-1$
				consumeInstanceOfClassic();
				break;

			case 554:
				if (DEBUG) { System.out.println("InstanceofPattern ::= instanceof Pattern"); }  //$NON-NLS-1$
				consumeInstanceofPattern();
				break;

			case 557:
				if (DEBUG) { System.out.println("Pattern -> RecordPattern"); }  //$NON-NLS-1$
				consumePattern();
				break;

			case 558:
				if (DEBUG) { System.out.println("ParenthesizedPattern ::= PushLPAREN Pattern PushRPAREN"); }  //$NON-NLS-1$
				consumeParenthesizedPattern();
				break;

			case 559:
				if (DEBUG) { System.out.println("TypePattern ::= Modifiersopt Type JavaIdentifier"); }  //$NON-NLS-1$
				consumeTypePattern();
				break;

			case 560:
				if (DEBUG) { System.out.println("RecordPattern ::= Modifiersopt Type..."); }  //$NON-NLS-1$
				consumeRecordPattern();
				break;

			case 561:
				if (DEBUG) { System.out.println("RecordPattern ::= Modifiersopt Type..."); }  //$NON-NLS-1$
				consumeRecordPatternWithId();
				break;

			case 563:
				if (DEBUG) { System.out.println("RecordStructurePattern ::= PushLPAREN..."); }  //$NON-NLS-1$
				consumeRecordStructure();
				break;

			case 564:
				if (DEBUG) { System.out.println("RecordComponentPatternsopt ::="); }  //$NON-NLS-1$
				consumeRecordComponentPatternsopt();
				break;

			case 566:
				if (DEBUG) { System.out.println("RecordComponentPatternList ::=..."); }  //$NON-NLS-1$
				consumeRecordComponentPatternList();
				break;

			case 568:
				if (DEBUG) { System.out.println("PushLeftBrace ::="); }  //$NON-NLS-1$
				consumePushLeftBrace();
				break;

			case 569:
				if (DEBUG) { System.out.println("ArrayInitializer ::= LBRACE PushLeftBrace ,opt RBRACE"); }  //$NON-NLS-1$
				consumeEmptyArrayInitializer();
				break;

			case 570:
				if (DEBUG) { System.out.println("ArrayInitializer ::= LBRACE PushLeftBrace..."); }  //$NON-NLS-1$
				consumeArrayInitializer();
				break;

			case 571:
				if (DEBUG) { System.out.println("ArrayInitializer ::= LBRACE PushLeftBrace..."); }  //$NON-NLS-1$
				consumeArrayInitializer();
				break;

			case 573:
				if (DEBUG) { System.out.println("VariableInitializers ::= VariableInitializers COMMA..."); }  //$NON-NLS-1$
				consumeVariableInitializers();
				break;

			case 574:
				if (DEBUG) { System.out.println("Block ::= OpenBlock LBRACE BlockStatementsopt RBRACE"); }  //$NON-NLS-1$
				consumeBlock();
				break;

			case 575:
				if (DEBUG) { System.out.println("OpenBlock ::="); }  //$NON-NLS-1$
				consumeOpenBlock();
				break;

			case 576:
				if (DEBUG) { System.out.println("BlockStatements ::= BlockStatement"); }  //$NON-NLS-1$
				consumeBlockStatement();
				break;

			case 577:
				if (DEBUG) { System.out.println("BlockStatements ::= BlockStatements BlockStatement"); }  //$NON-NLS-1$
				consumeBlockStatements();
				break;

			case 585:
				if (DEBUG) { System.out.println("BlockStatement ::= InterfaceDeclaration"); }  //$NON-NLS-1$
				consumeInvalidInterfaceDeclaration();
				break;

			case 586:
				if (DEBUG) { System.out.println("BlockStatement ::= AnnotationTypeDeclaration"); }  //$NON-NLS-1$
				consumeInvalidAnnotationTypeDeclaration();
				break;

			case 587:
				if (DEBUG) { System.out.println("BlockStatement ::= EnumDeclaration"); }  //$NON-NLS-1$
				consumeInvalidEnumDeclaration();
				break;

			case 588:
				if (DEBUG) { System.out.println("LocalVariableDeclarationStatement ::=..."); }  //$NON-NLS-1$
				consumeLocalVariableDeclarationStatement();
				break;

			case 589:
				if (DEBUG) { System.out.println("LocalVariableDeclaration ::= Type PushModifiers..."); }  //$NON-NLS-1$
				consumeLocalVariableDeclaration();
				break;

			case 590:
				if (DEBUG) { System.out.println("LocalVariableDeclaration ::= Modifiers Type..."); }  //$NON-NLS-1$
				consumeLocalVariableDeclaration();
				break;

			case 591:
				if (DEBUG) { System.out.println("PushModifiers ::="); }  //$NON-NLS-1$
				consumePushModifiers();
				break;

			case 592:
				if (DEBUG) { System.out.println("PushModifiersForHeader ::="); }  //$NON-NLS-1$
				consumePushModifiersForHeader();
				break;

			case 593:
				if (DEBUG) { System.out.println("PushRealModifiers ::="); }  //$NON-NLS-1$
				consumePushRealModifiers();
				break;

			case 621:
				if (DEBUG) { System.out.println("EmptyStatement ::= SEMICOLON"); }  //$NON-NLS-1$
				consumeEmptyStatement();
				break;

			case 622:
				if (DEBUG) { System.out.println("LabeledStatement ::= Label COLON Statement"); }  //$NON-NLS-1$
				consumeStatementLabel();
				break;

			case 623:
				if (DEBUG) { System.out.println("LabeledStatementNoShortIf ::= Label COLON..."); }  //$NON-NLS-1$
				consumeStatementLabel();
				break;

			case 624:
				if (DEBUG) { System.out.println("Label ::= JavaIdentifier"); }  //$NON-NLS-1$
				consumeLabel();
				break;

			case 625:
				if (DEBUG) { System.out.println("ExpressionStatement ::= StatementExpression SEMICOLON"); }  //$NON-NLS-1$
				consumeExpressionStatement();
				break;

			case 634:
				if (DEBUG) { System.out.println("IfThenStatement ::= if LPAREN Expression RPAREN..."); }  //$NON-NLS-1$
				consumeStatementIfNoElse();
				break;

			case 635:
				if (DEBUG) { System.out.println("IfThenElseStatement ::= if LPAREN Expression RPAREN..."); }  //$NON-NLS-1$
				consumeStatementIfWithElse();
				break;

			case 636:
				if (DEBUG) { System.out.println("IfThenElseStatementNoShortIf ::= if LPAREN Expression..."); }  //$NON-NLS-1$
				consumeStatementIfWithElse();
				break;

			case 637:
				if (DEBUG) { System.out.println("SwitchStatement ::= switch LPAREN Expression RPAREN..."); }  //$NON-NLS-1$
				consumeStatementSwitch();
				break;

			case 638:
				if (DEBUG) { System.out.println("SwitchBlock ::= LBRACE RBRACE"); }  //$NON-NLS-1$
				consumeEmptySwitchBlock();
				break;

			case 641:
				if (DEBUG) { System.out.println("SwitchBlock ::= LBRACE SwitchBlockStatements..."); }  //$NON-NLS-1$
				consumeSwitchBlock();
				break;

			case 643:
				if (DEBUG) { System.out.println("SwitchBlockStatements ::= SwitchBlockStatements..."); }  //$NON-NLS-1$
				consumeSwitchBlockStatements();
				break;

			case 645:
				if (DEBUG) { System.out.println("SwitchBlockStatement ::= SwitchLabels BlockStatements"); }  //$NON-NLS-1$
				consumeSwitchBlockStatement();
				break;

			case 647:
				if (DEBUG) { System.out.println("SwitchLabels ::= SwitchLabels SwitchLabel"); }  //$NON-NLS-1$
				consumeSwitchLabels();
				break;

			case 648:
				if (DEBUG) { System.out.println("SwitchLabel ::= SwitchLabelCaseLhs COLON"); }  //$NON-NLS-1$
				consumeCaseLabel();
				break;

			case 649:
				if (DEBUG) { System.out.println("SwitchLabel ::= default COLON"); }  //$NON-NLS-1$
				consumeDefaultLabel();
				break;

			case 652:
				if (DEBUG) { System.out.println("SwitchExpression ::= switch LPAREN Expression RPAREN..."); }  //$NON-NLS-1$
				consumeSwitchExpression();
				break;

			case 655:
				if (DEBUG) { System.out.println("SwitchLabeledRule ::= SwitchLabeledThrowStatement"); }  //$NON-NLS-1$
				consumeSwitchLabeledRule();
				break;

			case 656:
				if (DEBUG) { System.out.println("SwitchLabeledExpression ::= SwitchLabelExpr Expression"); }  //$NON-NLS-1$
				consumeSwitchLabeledExpression();
				break;

			case 657:
				if (DEBUG) { System.out.println("SwitchLabeledBlock ::= SwitchLabelExpr Block"); }  //$NON-NLS-1$
				consumeSwitchLabeledBlock();
				break;

			case 658:
				if (DEBUG) { System.out.println("SwitchLabeledThrowStatement ::= SwitchLabelExpr..."); }  //$NON-NLS-1$
				consumeSwitchLabeledThrowStatement();
				break;

			case 659:
				if (DEBUG) { System.out.println("SwitchLabelExpr ::= default ARROW"); }  //$NON-NLS-1$
				consumeDefaultLabelExpr();
				break;

			case 660:
				if (DEBUG) { System.out.println("SwitchLabelExpr ::= SwitchLabelCaseLhs BeginCaseExpr..."); }  //$NON-NLS-1$
				consumeCaseLabelExpr();
				break;

			case 661:
				if (DEBUG) { System.out.println("SwitchLabelCaseLhs ::= case CaseLabelElements"); }  //$NON-NLS-1$
				consumeSwitchLabelCaseLhs();
				break;

			case 663:
				if (DEBUG) { System.out.println("CaseLabelElements ::= CaseLabelElements COMMA..."); }  //$NON-NLS-1$
				consumeCaseLabelElements();
				break;

			case 664:
				if (DEBUG) { System.out.println("CaseLabelElement ::= ConstantExpression"); }  //$NON-NLS-1$
				consumeCaseLabelElement(CaseLabelKind.CASE_EXPRESSION);
				break;

			case 665:
				if (DEBUG) { System.out.println("CaseLabelElement ::= default"); }  //$NON-NLS-1$
				consumeCaseLabelElement(CaseLabelKind.CASE_DEFAULT);
				break;

			case 666:
				if (DEBUG) { System.out.println("CaseLabelElement ::= CaseLabelElementPattern"); }  //$NON-NLS-1$
				consumeCaseLabelElement(CaseLabelKind.CASE_PATTERN);
				break;

			case 667:
				if (DEBUG) { System.out.println("CaseLabelElement ::= CaseLabelElementPattern Guard"); }  //$NON-NLS-1$
				consumeCaseLabelElement(CaseLabelKind.CASE_PATTERN);
				break;

			case 668:
				if (DEBUG) { System.out.println("CaseLabelElementPattern ::= BeginCaseElement Pattern"); }  //$NON-NLS-1$
				consumeCaseLabelElementPattern();
				break;

			case 669:
				if (DEBUG) { System.out.println("Guard ::= RestrictedIdentifierWhen Expression"); }  //$NON-NLS-1$
				consumeGuard();
				break;

			case 670:
				if (DEBUG) { System.out.println("YieldStatement ::= RestrictedIdentifierYield Expression"); }  //$NON-NLS-1$
				consumeStatementYield();
				break;

			case 671:
				if (DEBUG) { System.out.println("WhileStatement ::= while LPAREN Expression RPAREN..."); }  //$NON-NLS-1$
				consumeStatementWhile();
				break;

			case 672:
				if (DEBUG) { System.out.println("WhileStatementNoShortIf ::= while LPAREN Expression..."); }  //$NON-NLS-1$
				consumeStatementWhile();
				break;

			case 673:
				if (DEBUG) { System.out.println("DoStatement ::= do Statement while LPAREN Expression..."); }  //$NON-NLS-1$
				consumeStatementDo();
				break;

			case 674:
				if (DEBUG) { System.out.println("ForStatement ::= for LPAREN ForInitopt SEMICOLON..."); }  //$NON-NLS-1$
				consumeStatementFor();
				break;

			case 675:
				if (DEBUG) { System.out.println("ForStatementNoShortIf ::= for LPAREN ForInitopt..."); }  //$NON-NLS-1$
				consumeStatementFor();
				break;

			case 676:
				if (DEBUG) { System.out.println("ForInit ::= StatementExpressionList"); }  //$NON-NLS-1$
				consumeForInit();
				break;

			case 680:
				if (DEBUG) { System.out.println("StatementExpressionList ::= StatementExpressionList..."); }  //$NON-NLS-1$
				consumeStatementExpressionList();
				break;

			case 681:
				if (DEBUG) { System.out.println("AssertStatement ::= assert Expression SEMICOLON"); }  //$NON-NLS-1$
				consumeSimpleAssertStatement();
				break;

			case 682:
				if (DEBUG) { System.out.println("AssertStatement ::= assert Expression COLON Expression"); }  //$NON-NLS-1$
				consumeAssertStatement();
				break;

			case 683:
				if (DEBUG) { System.out.println("BreakStatement ::= break SEMICOLON"); }  //$NON-NLS-1$
				consumeStatementBreak();
				break;

			case 684:
				if (DEBUG) { System.out.println("BreakStatement ::= break Identifier SEMICOLON"); }  //$NON-NLS-1$
				consumeStatementBreakWithLabel();
				break;

			case 685:
				if (DEBUG) { System.out.println("ContinueStatement ::= continue SEMICOLON"); }  //$NON-NLS-1$
				consumeStatementContinue();
				break;

			case 686:
				if (DEBUG) { System.out.println("ContinueStatement ::= continue Identifier SEMICOLON"); }  //$NON-NLS-1$
				consumeStatementContinueWithLabel();
				break;

			case 687:
				if (DEBUG) { System.out.println("ReturnStatement ::= return Expressionopt SEMICOLON"); }  //$NON-NLS-1$
				consumeStatementReturn();
				break;

			case 688:
				if (DEBUG) { System.out.println("ThrowStatement ::= throw Expression SEMICOLON"); }  //$NON-NLS-1$
				consumeStatementThrow();
				break;

			case 689:
				if (DEBUG) { System.out.println("ThrowExpression ::= throw Expression"); }  //$NON-NLS-1$
				consumeThrowExpression();
				break;

			case 690:
				if (DEBUG) { System.out.println("SynchronizedStatement ::= OnlySynchronized LPAREN..."); }  //$NON-NLS-1$
				consumeStatementSynchronized();
				break;

			case 691:
				if (DEBUG) { System.out.println("OnlySynchronized ::= synchronized"); }  //$NON-NLS-1$
				consumeOnlySynchronized();
				break;

			case 692:
				if (DEBUG) { System.out.println("TryStatement ::= try TryBlock Catches"); }  //$NON-NLS-1$
				consumeStatementTry(false, false);
				break;

			case 693:
				if (DEBUG) { System.out.println("TryStatement ::= try TryBlock Catchesopt Finally"); }  //$NON-NLS-1$
				consumeStatementTry(true, false);
				break;

			case 694:
				if (DEBUG) { System.out.println("TryStatementWithResources ::= try ResourceSpecification"); }  //$NON-NLS-1$
				consumeStatementTry(false, true);
				break;

			case 695:
				if (DEBUG) { System.out.println("TryStatementWithResources ::= try ResourceSpecification"); }  //$NON-NLS-1$
				consumeStatementTry(true, true);
				break;

			case 696:
				if (DEBUG) { System.out.println("ResourceSpecification ::= LPAREN Resources ;opt RPAREN"); }  //$NON-NLS-1$
				consumeResourceSpecification();
				break;

			case 697:
				if (DEBUG) { System.out.println(";opt ::="); }  //$NON-NLS-1$
				consumeResourceOptionalTrailingSemiColon(false);
				break;

			case 698:
				if (DEBUG) { System.out.println(";opt ::= SEMICOLON"); }  //$NON-NLS-1$
				consumeResourceOptionalTrailingSemiColon(true);
				break;

			case 699:
				if (DEBUG) { System.out.println("Resources ::= Resource"); }  //$NON-NLS-1$
				consumeSingleResource();
				break;

			case 700:
				if (DEBUG) { System.out.println("Resources ::= Resources TrailingSemiColon Resource"); }  //$NON-NLS-1$
				consumeMultipleResources();
				break;

			case 701:
				if (DEBUG) { System.out.println("TrailingSemiColon ::= SEMICOLON"); }  //$NON-NLS-1$
				consumeResourceOptionalTrailingSemiColon(true);
				break;

			case 702:
				if (DEBUG) { System.out.println("Resource ::= Type PushModifiers VariableDeclaratorId..."); }  //$NON-NLS-1$
				consumeResourceAsLocalVariableDeclaration();
				break;

			case 703:
				if (DEBUG) { System.out.println("Resource ::= Modifiers Type PushRealModifiers..."); }  //$NON-NLS-1$
				consumeResourceAsLocalVariableDeclaration();
				break;

			case 704:
				if (DEBUG) { System.out.println("Resource ::= Name"); }  //$NON-NLS-1$
				consumeResourceAsLocalVariable();
				break;

			case 705:
				if (DEBUG) { System.out.println("Resource ::= this"); }  //$NON-NLS-1$
				consumeResourceAsThis();
				break;

			case 706:
				if (DEBUG) { System.out.println("Resource ::= FieldAccess"); }  //$NON-NLS-1$
				consumeResourceAsFieldAccess();
				break;

			case 708:
				if (DEBUG) { System.out.println("ExitTryBlock ::="); }  //$NON-NLS-1$
				consumeExitTryBlock();
				break;

			case 710:
				if (DEBUG) { System.out.println("Catches ::= Catches CatchClause"); }  //$NON-NLS-1$
				consumeCatches();
				break;

			case 711:
				if (DEBUG) { System.out.println("CatchClause ::= catch LPAREN CatchFormalParameter RPAREN"); }  //$NON-NLS-1$
				consumeStatementCatch();
				break;

			case 713:
				if (DEBUG) { System.out.println("PushLPAREN ::= LPAREN"); }  //$NON-NLS-1$
				consumeLeftParen();
				break;

			case 714:
				if (DEBUG) { System.out.println("PushRPAREN ::= RPAREN"); }  //$NON-NLS-1$
				consumeRightParen();
				break;

			case 719:
				if (DEBUG) { System.out.println("PrimaryNoNewArray ::= this"); }  //$NON-NLS-1$
				consumePrimaryNoNewArrayThis();
				break;

			case 720:
				if (DEBUG) { System.out.println("PrimaryNoNewArray ::= PushLPAREN Expression_NotName..."); }  //$NON-NLS-1$
				consumePrimaryNoNewArray();
				break;

			case 721:
				if (DEBUG) { System.out.println("PrimaryNoNewArray ::= PushLPAREN Name PushRPAREN"); }  //$NON-NLS-1$
				consumePrimaryNoNewArrayWithName();
				break;

			case 724:
				if (DEBUG) { System.out.println("PrimaryNoNewArray ::= Name DOT this"); }  //$NON-NLS-1$
				consumePrimaryNoNewArrayNameThis();
				break;

			case 725:
				if (DEBUG) { System.out.println("QualifiedSuperReceiver ::= Name DOT super"); }  //$NON-NLS-1$
				consumeQualifiedSuperReceiver();
				break;

			case 726:
				if (DEBUG) { System.out.println("PrimaryNoNewArray ::= Name DOT class"); }  //$NON-NLS-1$
				consumePrimaryNoNewArrayName();
				break;

			case 727:
				if (DEBUG) { System.out.println("PrimaryNoNewArray ::= Name Dims DOT class"); }  //$NON-NLS-1$
				consumePrimaryNoNewArrayArrayType();
				break;

			case 728:
				if (DEBUG) { System.out.println("PrimaryNoNewArray ::= PrimitiveType Dims DOT class"); }  //$NON-NLS-1$
				consumePrimaryNoNewArrayPrimitiveArrayType();
				break;

			case 729:
				if (DEBUG) { System.out.println("PrimaryNoNewArray ::= PrimitiveType DOT class"); }  //$NON-NLS-1$
				consumePrimaryNoNewArrayPrimitiveType();
				break;

			case 735:
				if (DEBUG) { System.out.println("ReferenceExpressionTypeArgumentsAndTrunk0 ::=..."); }  //$NON-NLS-1$
				consumeReferenceExpressionTypeArgumentsAndTrunk(false);
				break;

			case 736:
				if (DEBUG) { System.out.println("ReferenceExpressionTypeArgumentsAndTrunk0 ::=..."); }  //$NON-NLS-1$
				consumeReferenceExpressionTypeArgumentsAndTrunk(true);
				break;

			case 737:
				if (DEBUG) { System.out.println("ReferenceExpression ::= PrimitiveType Dims COLON_COLON"); }  //$NON-NLS-1$
				consumeReferenceExpressionTypeForm(true);
				break;

			case 738:
				if (DEBUG) { System.out.println("ReferenceExpression ::= Name Dimsopt COLON_COLON..."); }  //$NON-NLS-1$
				consumeReferenceExpressionTypeForm(false);
				break;

			case 739:
				if (DEBUG) { System.out.println("ReferenceExpression ::= Name BeginTypeArguments..."); }  //$NON-NLS-1$
				consumeReferenceExpressionGenericTypeForm();
				break;

			case 740:
				if (DEBUG) { System.out.println("ReferenceExpression ::= Primary COLON_COLON..."); }  //$NON-NLS-1$
				consumeReferenceExpressionPrimaryForm();
				break;

			case 741:
				if (DEBUG) { System.out.println("ReferenceExpression ::= QualifiedSuperReceiver..."); }  //$NON-NLS-1$
				consumeReferenceExpressionPrimaryForm();
				break;

			case 742:
				if (DEBUG) { System.out.println("ReferenceExpression ::= super COLON_COLON..."); }  //$NON-NLS-1$
				consumeReferenceExpressionSuperForm();
				break;

			case 743:
				if (DEBUG) { System.out.println("NonWildTypeArgumentsopt ::="); }  //$NON-NLS-1$
				consumeEmptyTypeArguments();
				break;

			case 745:
				if (DEBUG) { System.out.println("IdentifierOrNew ::= Identifier"); }  //$NON-NLS-1$
				consumeIdentifierOrNew(false);
				break;

			case 746:
				if (DEBUG) { System.out.println("IdentifierOrNew ::= new"); }  //$NON-NLS-1$
				consumeIdentifierOrNew(true);
				break;

			case 747:
				if (DEBUG) { System.out.println("LambdaExpression ::= LambdaParameters ARROW LambdaBody"); }  //$NON-NLS-1$
				consumeLambdaExpression();
				break;

			case 748:
				if (DEBUG) { System.out.println("NestedLambda ::="); }  //$NON-NLS-1$
				consumeNestedLambda();
				break;

			case 749:
				if (DEBUG) { System.out.println("LambdaParameters ::= Identifier NestedLambda"); }  //$NON-NLS-1$
				consumeTypeElidedLambdaParameter(false);
				break;

			case 755:
				if (DEBUG) { System.out.println("TypeElidedFormalParameterList ::=..."); }  //$NON-NLS-1$
				consumeFormalParameterList();
				break;

			case 756:
				if (DEBUG) { System.out.println("TypeElidedFormalParameter ::= Modifiersopt Identifier"); }  //$NON-NLS-1$
				consumeTypeElidedLambdaParameter(true);
				break;

			case 759:
				if (DEBUG) { System.out.println("ElidedLeftBraceAndReturn ::="); }  //$NON-NLS-1$
				consumeElidedLeftBraceAndReturn();
				break;

			case 760:
				if (DEBUG) { System.out.println("AllocationHeader ::= new ClassType LPAREN..."); }  //$NON-NLS-1$
				consumeAllocationHeader();
				break;

			case 761:
				if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::= new..."); }  //$NON-NLS-1$
				consumeClassInstanceCreationExpressionWithTypeArguments();
				break;

			case 762:
				if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::= new ClassType..."); }  //$NON-NLS-1$
				consumeClassInstanceCreationExpression();
				break;

			case 763:
				if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::= Primary DOT new..."); }  //$NON-NLS-1$
				consumeClassInstanceCreationExpressionQualifiedWithTypeArguments();
				break;

			case 764:
				if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::= Primary DOT new..."); }  //$NON-NLS-1$
				consumeClassInstanceCreationExpressionQualified();
				break;

			case 765:
				if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::=..."); }  //$NON-NLS-1$
				consumeClassInstanceCreationExpressionQualified();
				break;

			case 766:
				if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::=..."); }  //$NON-NLS-1$
				consumeClassInstanceCreationExpressionQualifiedWithTypeArguments();
				break;

			case 767:
				if (DEBUG) { System.out.println("EnterInstanceCreationArgumentList ::="); }  //$NON-NLS-1$
				consumeEnterInstanceCreationArgumentList();
				break;

			case 768:
				if (DEBUG) { System.out.println("ClassInstanceCreationExpressionName ::= Name DOT new"); }  //$NON-NLS-1$
				consumeClassInstanceCreationExpressionName();
				break;

			case 769:
				if (DEBUG) { System.out.println("UnqualifiedClassBodyopt ::="); }  //$NON-NLS-1$
				consumeClassBodyopt();
				break;

			case 771:
				if (DEBUG) { System.out.println("UnqualifiedEnterAnonymousClassBody ::="); }  //$NON-NLS-1$
				consumeEnterAnonymousClassBody(false);
				break;

			case 772:
				if (DEBUG) { System.out.println("QualifiedClassBodyopt ::="); }  //$NON-NLS-1$
				consumeClassBodyopt();
				break;

			case 774:
				if (DEBUG) { System.out.println("QualifiedEnterAnonymousClassBody ::="); }  //$NON-NLS-1$
				consumeEnterAnonymousClassBody(true);
				break;

			case 776:
				if (DEBUG) { System.out.println("ArgumentList ::= ArgumentList COMMA Expression"); }  //$NON-NLS-1$
				consumeArgumentList();
				break;

			case 777:
				if (DEBUG) { System.out.println("ArrayCreationHeader ::= new PrimitiveType..."); }  //$NON-NLS-1$
				consumeArrayCreationHeader();
				break;

			case 778:
				if (DEBUG) { System.out.println("ArrayCreationHeader ::= new ClassOrInterfaceType..."); }  //$NON-NLS-1$
				consumeArrayCreationHeader();
				break;

			case 779:
				if (DEBUG) { System.out.println("ArrayCreationWithoutArrayInitializer ::= new..."); }  //$NON-NLS-1$
				consumeArrayCreationExpressionWithoutInitializer();
				break;

			case 780:
				if (DEBUG) { System.out.println("ArrayCreationWithArrayInitializer ::= new PrimitiveType"); }  //$NON-NLS-1$
				consumeArrayCreationExpressionWithInitializer();
				break;

			case 781:
				if (DEBUG) { System.out.println("ArrayCreationWithoutArrayInitializer ::= new..."); }  //$NON-NLS-1$
				consumeArrayCreationExpressionWithoutInitializer();
				break;

			case 782:
				if (DEBUG) { System.out.println("ArrayCreationWithArrayInitializer ::= new..."); }  //$NON-NLS-1$
				consumeArrayCreationExpressionWithInitializer();
				break;

			case 784:
				if (DEBUG) { System.out.println("DimWithOrWithOutExprs ::= DimWithOrWithOutExprs..."); }  //$NON-NLS-1$
				consumeDimWithOrWithOutExprs();
				break;

			case 786:
				if (DEBUG) { System.out.println("DimWithOrWithOutExpr ::= TypeAnnotationsopt LBRACKET..."); }  //$NON-NLS-1$
				consumeDimWithOrWithOutExpr();
				break;

			case 787:
				if (DEBUG) { System.out.println("Dims ::= DimsLoop"); }  //$NON-NLS-1$
				consumeDims();
				break;

			case 790:
				if (DEBUG) { System.out.println("OneDimLoop ::= LBRACKET RBRACKET"); }  //$NON-NLS-1$
				consumeOneDimLoop(false);
				break;

			case 791:
				if (DEBUG) { System.out.println("OneDimLoop ::= TypeAnnotations LBRACKET RBRACKET"); }  //$NON-NLS-1$
				consumeOneDimLoop(true);
				break;

			case 792:
				if (DEBUG) { System.out.println("FieldAccess ::= Primary DOT JavaIdentifier"); }  //$NON-NLS-1$
				consumeFieldAccess(false);
				break;

			case 793:
				if (DEBUG) { System.out.println("FieldAccess ::= super DOT JavaIdentifier"); }  //$NON-NLS-1$
				consumeFieldAccess(true);
				break;

			case 794:
				if (DEBUG) { System.out.println("FieldAccess ::= QualifiedSuperReceiver DOT..."); }  //$NON-NLS-1$
				consumeFieldAccess(false);
				break;

			case 795:
				if (DEBUG) { System.out.println("MethodInvocation ::= NameOrAj LPAREN ArgumentListopt..."); }  //$NON-NLS-1$
				consumeMethodInvocationName();
				break;

			case 796:
				if (DEBUG) { System.out.println("MethodInvocation ::= Name DOT OnlyTypeArguments..."); }  //$NON-NLS-1$
				consumeMethodInvocationNameWithTypeArguments();
				break;

			case 797:
				if (DEBUG) { System.out.println("MethodInvocation ::= Primary DOT OnlyTypeArguments..."); }  //$NON-NLS-1$
				consumeMethodInvocationPrimaryWithTypeArguments();
				break;

			case 798:
				if (DEBUG) { System.out.println("MethodInvocation ::= Primary DOT JavaIdentifier LPAREN"); }  //$NON-NLS-1$
				consumeMethodInvocationPrimary();
				break;

			case 799:
				if (DEBUG) { System.out.println("MethodInvocation ::= QualifiedSuperReceiver DOT..."); }  //$NON-NLS-1$
				consumeMethodInvocationPrimary();
				break;

			case 800:
				if (DEBUG) { System.out.println("MethodInvocation ::= QualifiedSuperReceiver DOT..."); }  //$NON-NLS-1$
				consumeMethodInvocationPrimaryWithTypeArguments();
				break;

			case 801:
				if (DEBUG) { System.out.println("MethodInvocation ::= super DOT OnlyTypeArguments..."); }  //$NON-NLS-1$
				consumeMethodInvocationSuperWithTypeArguments();
				break;

			case 802:
				if (DEBUG) { System.out.println("MethodInvocation ::= super DOT JavaIdentifier LPAREN..."); }  //$NON-NLS-1$
				consumeMethodInvocationSuper();
				break;

			case 803:
				if (DEBUG) { System.out.println("ArrayAccess ::= Name LBRACKET Expression RBRACKET"); }  //$NON-NLS-1$
				consumeArrayAccess(true);
				break;

			case 804:
				if (DEBUG) { System.out.println("ArrayAccess ::= AjName LBRACKET Expression RBRACKET"); }  //$NON-NLS-1$
				consumeArrayAccess(true);
				break;

			case 805:
				if (DEBUG) { System.out.println("ArrayAccess ::= PrimaryNoNewArray LBRACKET Expression..."); }  //$NON-NLS-1$
				consumeArrayAccess(false);
				break;

			case 806:
				if (DEBUG) { System.out.println("ArrayAccess ::= ArrayCreationWithArrayInitializer..."); }  //$NON-NLS-1$
				consumeArrayAccess(false);
				break;

			case 808:
				if (DEBUG) { System.out.println("PostfixExpression ::= NameOrAj"); }  //$NON-NLS-1$
				consumePostfixExpression();
				break;

			case 811:
				if (DEBUG) { System.out.println("PostIncrementExpression ::= PostfixExpression PLUS_PLUS"); }  //$NON-NLS-1$
				consumeUnaryExpression(OperatorIds.PLUS, true);
				break;

			case 812:
				if (DEBUG) { System.out.println("PostDecrementExpression ::= PostfixExpression..."); }  //$NON-NLS-1$
				consumeUnaryExpression(OperatorIds.MINUS, true);
				break;

			case 813:
				if (DEBUG) { System.out.println("PushPosition ::="); }  //$NON-NLS-1$
				consumePushPosition();
				break;

			case 816:
				if (DEBUG) { System.out.println("UnaryExpression ::= PLUS PushPosition UnaryExpression"); }  //$NON-NLS-1$
				consumeUnaryExpression(OperatorIds.PLUS);
				break;

			case 817:
				if (DEBUG) { System.out.println("UnaryExpression ::= MINUS PushPosition UnaryExpression"); }  //$NON-NLS-1$
				consumeUnaryExpression(OperatorIds.MINUS);
				break;

			case 819:
				if (DEBUG) { System.out.println("PreIncrementExpression ::= PLUS_PLUS PushPosition..."); }  //$NON-NLS-1$
				consumeUnaryExpression(OperatorIds.PLUS, false);
				break;

			case 820:
				if (DEBUG) { System.out.println("PreDecrementExpression ::= MINUS_MINUS PushPosition..."); }  //$NON-NLS-1$
				consumeUnaryExpression(OperatorIds.MINUS, false);
				break;

			case 822:
				if (DEBUG) { System.out.println("UnaryExpressionNotPlusMinus ::= TWIDDLE PushPosition..."); }  //$NON-NLS-1$
				consumeUnaryExpression(OperatorIds.TWIDDLE);
				break;

			case 823:
				if (DEBUG) { System.out.println("UnaryExpressionNotPlusMinus ::= NOT PushPosition..."); }  //$NON-NLS-1$
				consumeUnaryExpression(OperatorIds.NOT);
				break;

			case 825:
				if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN PrimitiveType Dimsopt..."); }  //$NON-NLS-1$
				consumeCastExpressionWithPrimitiveType();
				break;

			case 826:
				if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN Name..."); }  //$NON-NLS-1$
				consumeCastExpressionWithGenericsArray();
				break;

			case 827:
				if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN Name..."); }  //$NON-NLS-1$
				consumeCastExpressionWithQualifiedGenericsArray();
				break;

			case 828:
				if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN Name PushRPAREN..."); }  //$NON-NLS-1$
				consumeCastExpressionLL1();
				break;

			case 829:
				if (DEBUG) { System.out.println("CastExpression ::= BeginIntersectionCast PushLPAREN..."); }  //$NON-NLS-1$
				consumeCastExpressionLL1WithBounds();
				break;

			case 830:
				if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN Name Dims..."); }  //$NON-NLS-1$
				consumeCastExpressionWithNameArray();
				break;

			case 831:
				if (DEBUG) { System.out.println("AdditionalBoundsListOpt ::="); }  //$NON-NLS-1$
				consumeZeroAdditionalBounds();
				break;

			case 835:
				if (DEBUG) { System.out.println("OnlyTypeArgumentsForCastExpression ::= OnlyTypeArguments"); }  //$NON-NLS-1$
				consumeOnlyTypeArgumentsForCastExpression();
				break;

			case 836:
				if (DEBUG) { System.out.println("InsideCastExpression ::="); }  //$NON-NLS-1$
				consumeInsideCastExpression();
				break;

			case 837:
				if (DEBUG) { System.out.println("InsideCastExpressionLL1 ::="); }  //$NON-NLS-1$
				consumeInsideCastExpressionLL1();
				break;

			case 838:
				if (DEBUG) { System.out.println("InsideCastExpressionLL1WithBounds ::="); }  //$NON-NLS-1$
				consumeInsideCastExpressionLL1WithBounds();
				break;

			case 839:
				if (DEBUG) { System.out.println("InsideCastExpressionWithQualifiedGenerics ::="); }  //$NON-NLS-1$
				consumeInsideCastExpressionWithQualifiedGenerics();
				break;

			case 841:
				if (DEBUG) { System.out.println("MultiplicativeExpression ::= MultiplicativeExpression..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.MULTIPLY);
				break;

			case 842:
				if (DEBUG) { System.out.println("MultiplicativeExpression ::= MultiplicativeExpression..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.DIVIDE);
				break;

			case 843:
				if (DEBUG) { System.out.println("MultiplicativeExpression ::= MultiplicativeExpression..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.REMAINDER);
				break;

			case 845:
				if (DEBUG) { System.out.println("AdditiveExpression ::= AdditiveExpression PLUS..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.PLUS);
				break;

			case 846:
				if (DEBUG) { System.out.println("AdditiveExpression ::= AdditiveExpression MINUS..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.MINUS);
				break;

			case 848:
				if (DEBUG) { System.out.println("ShiftExpression ::= ShiftExpression LEFT_SHIFT..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.LEFT_SHIFT);
				break;

			case 849:
				if (DEBUG) { System.out.println("ShiftExpression ::= ShiftExpression RIGHT_SHIFT..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.RIGHT_SHIFT);
				break;

			case 850:
				if (DEBUG) { System.out.println("ShiftExpression ::= ShiftExpression UNSIGNED_RIGHT_SHIFT"); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.UNSIGNED_RIGHT_SHIFT);
				break;

			case 852:
				if (DEBUG) { System.out.println("RelationalExpression ::= RelationalExpression LESS..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.LESS);
				break;

			case 853:
				if (DEBUG) { System.out.println("RelationalExpression ::= RelationalExpression GREATER..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.GREATER);
				break;

			case 854:
				if (DEBUG) { System.out.println("RelationalExpression ::= RelationalExpression LESS_EQUAL"); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.LESS_EQUAL);
				break;

			case 855:
				if (DEBUG) { System.out.println("RelationalExpression ::= RelationalExpression..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.GREATER_EQUAL);
				break;

			case 857:
				if (DEBUG) { System.out.println("EqualityExpression ::= EqualityExpression EQUAL_EQUAL..."); }  //$NON-NLS-1$
				consumeEqualityExpression(OperatorIds.EQUAL_EQUAL);
				break;

			case 858:
				if (DEBUG) { System.out.println("EqualityExpression ::= EqualityExpression NOT_EQUAL..."); }  //$NON-NLS-1$
				consumeEqualityExpression(OperatorIds.NOT_EQUAL);
				break;

			case 860:
				if (DEBUG) { System.out.println("AndExpression ::= AndExpression AND EqualityExpression"); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.AND);
				break;

			case 862:
				if (DEBUG) { System.out.println("ExclusiveOrExpression ::= ExclusiveOrExpression XOR..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.XOR);
				break;

			case 864:
				if (DEBUG) { System.out.println("InclusiveOrExpression ::= InclusiveOrExpression OR..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.OR);
				break;

			case 866:
				if (DEBUG) { System.out.println("ConditionalAndExpression ::= ConditionalAndExpression..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.AND_AND);
				break;

			case 868:
				if (DEBUG) { System.out.println("ConditionalOrExpression ::= ConditionalOrExpression..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.OR_OR);
				break;

			case 870:
				if (DEBUG) { System.out.println("ConditionalExpression ::= ConditionalOrExpression..."); }  //$NON-NLS-1$
				consumeConditionalExpression(OperatorIds.QUESTIONCOLON);
				break;

			case 873:
				if (DEBUG) { System.out.println("Assignment ::= PostfixExpression AssignmentOperator..."); }  //$NON-NLS-1$
				consumeAssignment();
				break;

			case 875:
				if (DEBUG) { System.out.println("Assignment ::= InvalidArrayInitializerAssignement"); }  //$NON-NLS-1$
				ignoreExpressionAssignment();
				break;

			case 876:
				if (DEBUG) { System.out.println("AssignmentOperator ::= EQUAL"); }  //$NON-NLS-1$
				consumeAssignmentOperator(EQUAL);
				break;

			case 877:
				if (DEBUG) { System.out.println("AssignmentOperator ::= MULTIPLY_EQUAL"); }  //$NON-NLS-1$
				consumeAssignmentOperator(MULTIPLY);
				break;

			case 878:
				if (DEBUG) { System.out.println("AssignmentOperator ::= DIVIDE_EQUAL"); }  //$NON-NLS-1$
				consumeAssignmentOperator(DIVIDE);
				break;

			case 879:
				if (DEBUG) { System.out.println("AssignmentOperator ::= REMAINDER_EQUAL"); }  //$NON-NLS-1$
				consumeAssignmentOperator(REMAINDER);
				break;

			case 880:
				if (DEBUG) { System.out.println("AssignmentOperator ::= PLUS_EQUAL"); }  //$NON-NLS-1$
				consumeAssignmentOperator(PLUS);
				break;

			case 881:
				if (DEBUG) { System.out.println("AssignmentOperator ::= MINUS_EQUAL"); }  //$NON-NLS-1$
				consumeAssignmentOperator(MINUS);
				break;

			case 882:
				if (DEBUG) { System.out.println("AssignmentOperator ::= LEFT_SHIFT_EQUAL"); }  //$NON-NLS-1$
				consumeAssignmentOperator(LEFT_SHIFT);
				break;

			case 883:
				if (DEBUG) { System.out.println("AssignmentOperator ::= RIGHT_SHIFT_EQUAL"); }  //$NON-NLS-1$
				consumeAssignmentOperator(RIGHT_SHIFT);
				break;

			case 884:
				if (DEBUG) { System.out.println("AssignmentOperator ::= UNSIGNED_RIGHT_SHIFT_EQUAL"); }  //$NON-NLS-1$
				consumeAssignmentOperator(UNSIGNED_RIGHT_SHIFT);
				break;

			case 885:
				if (DEBUG) { System.out.println("AssignmentOperator ::= AND_EQUAL"); }  //$NON-NLS-1$
				consumeAssignmentOperator(AND);
				break;

			case 886:
				if (DEBUG) { System.out.println("AssignmentOperator ::= XOR_EQUAL"); }  //$NON-NLS-1$
				consumeAssignmentOperator(XOR);
				break;

			case 887:
				if (DEBUG) { System.out.println("AssignmentOperator ::= OR_EQUAL"); }  //$NON-NLS-1$
				consumeAssignmentOperator(OR);
				break;

			case 888:
				if (DEBUG) { System.out.println("Expression ::= AssignmentExpression"); }  //$NON-NLS-1$
				consumeExpression();
				break;

			case 891:
				if (DEBUG) { System.out.println("Expressionopt ::="); }  //$NON-NLS-1$
				consumeEmptyExpression();
				break;

			case 896:
				if (DEBUG) { System.out.println("ClassBodyDeclarationsopt ::="); }  //$NON-NLS-1$
				consumeEmptyClassBodyDeclarationsopt();
				break;

			case 897:
				if (DEBUG) { System.out.println("ClassBodyDeclarationsopt ::= NestedType..."); }  //$NON-NLS-1$
				consumeClassBodyDeclarationsopt();
				break;

			case 898:
				if (DEBUG) { System.out.println("Modifiersopt ::="); }  //$NON-NLS-1$
				consumeDefaultModifiers();
				break;

			case 899:
				if (DEBUG) { System.out.println("Modifiersopt ::= Modifiers"); }  //$NON-NLS-1$
				consumeModifiers();
				break;

			case 900:
				if (DEBUG) { System.out.println("BlockStatementsopt ::="); }  //$NON-NLS-1$
				consumeEmptyBlockStatementsopt();
				break;

			case 902:
				if (DEBUG) { System.out.println("Dimsopt ::="); }  //$NON-NLS-1$
				consumeEmptyDimsopt();
				break;

			case 904:
				if (DEBUG) { System.out.println("ArgumentListopt ::="); }  //$NON-NLS-1$
				consumeEmptyArgumentListopt();
				break;

			case 908:
				if (DEBUG) { System.out.println("FormalParameterListopt ::="); }  //$NON-NLS-1$
				consumeFormalParameterListopt();
				break;

			case 915:
				if (DEBUG) { System.out.println("ClassHeaderPermittedSubclasses ::=..."); }  //$NON-NLS-1$
				consumeClassHeaderPermittedSubclasses();
				break;

			case 918:
				if (DEBUG) { System.out.println("InterfaceHeaderPermittedSubClassesAndSubInterfaces ::="); }  //$NON-NLS-1$
				consumeInterfaceHeaderPermittedSubClassesAndSubInterfaces();
				break;

			case 919:
				if (DEBUG) { System.out.println("InterfaceMemberDeclarationsopt ::="); }  //$NON-NLS-1$
				consumeEmptyInterfaceMemberDeclarationsopt();
				break;

			case 920:
				if (DEBUG) { System.out.println("InterfaceMemberDeclarationsopt ::= NestedType..."); }  //$NON-NLS-1$
				consumeInterfaceMemberDeclarationsopt();
				break;

			case 921:
				if (DEBUG) { System.out.println("NestedType ::="); }  //$NON-NLS-1$
				consumeNestedType();
				break;

			case 922:
				if (DEBUG) { System.out.println("ForInitopt ::="); }  //$NON-NLS-1$
				consumeEmptyForInitopt();
				break;

			case 924:
				if (DEBUG) { System.out.println("ForUpdateopt ::="); }  //$NON-NLS-1$
				consumeEmptyForUpdateopt();
				break;

			case 928:
				if (DEBUG) { System.out.println("Catchesopt ::="); }  //$NON-NLS-1$
				consumeEmptyCatchesopt();
				break;

			case 930:
				if (DEBUG) { System.out.println("EnumDeclaration ::= EnumHeader EnumBody"); }  //$NON-NLS-1$
				consumeEnumDeclaration();
				break;

			case 931:
				if (DEBUG) { System.out.println("EnumHeader ::= EnumHeaderName ClassHeaderImplementsopt"); }  //$NON-NLS-1$
				consumeEnumHeader();
				break;

			case 932:
				if (DEBUG) { System.out.println("EnumHeaderName ::= Modifiersopt enum JavaIdentifier"); }  //$NON-NLS-1$
				consumeEnumHeaderName();
				break;

			case 933:
				if (DEBUG) { System.out.println("EnumHeaderName ::= Modifiersopt enum JavaIdentifier..."); }  //$NON-NLS-1$
				consumeEnumHeaderNameWithTypeParameters();
				break;

			case 934:
				if (DEBUG) { System.out.println("EnumBody ::= LBRACE EnumBodyDeclarationsopt RBRACE"); }  //$NON-NLS-1$
				consumeEnumBodyNoConstants();
				break;

			case 935:
				if (DEBUG) { System.out.println("EnumBody ::= LBRACE COMMA EnumBodyDeclarationsopt..."); }  //$NON-NLS-1$
				consumeEnumBodyNoConstants();
				break;

			case 936:
				if (DEBUG) { System.out.println("EnumBody ::= LBRACE EnumConstants COMMA..."); }  //$NON-NLS-1$
				consumeEnumBodyWithConstants();
				break;

			case 937:
				if (DEBUG) { System.out.println("EnumBody ::= LBRACE EnumConstants..."); }  //$NON-NLS-1$
				consumeEnumBodyWithConstants();
				break;

			case 939:
				if (DEBUG) { System.out.println("EnumConstants ::= EnumConstants COMMA EnumConstant"); }  //$NON-NLS-1$
				consumeEnumConstants();
				break;

			case 940:
				if (DEBUG) { System.out.println("EnumConstantHeaderName ::= Modifiersopt Identifier"); }  //$NON-NLS-1$
				consumeEnumConstantHeaderName();
				break;

			case 941:
				if (DEBUG) { System.out.println("EnumConstantHeader ::= EnumConstantHeaderName..."); }  //$NON-NLS-1$
				consumeEnumConstantHeader();
				break;

			case 942:
				if (DEBUG) { System.out.println("EnumConstant ::= EnumConstantHeader ForceNoDiet..."); }  //$NON-NLS-1$
				consumeEnumConstantWithClassBody();
				break;

			case 943:
				if (DEBUG) { System.out.println("EnumConstant ::= EnumConstantHeader"); }  //$NON-NLS-1$
				consumeEnumConstantNoClassBody();
				break;

			case 944:
				if (DEBUG) { System.out.println("Arguments ::= LPAREN ArgumentListopt RPAREN"); }  //$NON-NLS-1$
				consumeArguments();
				break;

			case 945:
				if (DEBUG) { System.out.println("Argumentsopt ::="); }  //$NON-NLS-1$
				consumeEmptyArguments();
				break;

			case 947:
				if (DEBUG) { System.out.println("EnumDeclarations ::= SEMICOLON ClassBodyDeclarationsopt"); }  //$NON-NLS-1$
				consumeEnumDeclarations();
				break;

			case 948:
				if (DEBUG) { System.out.println("EnumBodyDeclarationsopt ::="); }  //$NON-NLS-1$
				consumeEmptyEnumDeclarations();
				break;

			case 950:
				if (DEBUG) { System.out.println("EnhancedForStatement ::= EnhancedForStatementHeader..."); }  //$NON-NLS-1$
				consumeEnhancedForStatement();
				break;

			case 951:
				if (DEBUG) { System.out.println("EnhancedForStatementNoShortIf ::=..."); }  //$NON-NLS-1$
				consumeEnhancedForStatement();
				break;

			case 952:
				if (DEBUG) { System.out.println("EnhancedForStatementHeaderInit ::= for LPAREN Type..."); }  //$NON-NLS-1$
				consumeEnhancedForStatementHeaderInit(false);
				break;

			case 953:
				if (DEBUG) { System.out.println("EnhancedForStatementHeaderInit ::= for LPAREN Modifiers"); }  //$NON-NLS-1$
				consumeEnhancedForStatementHeaderInit(true);
				break;

			case 954:
				if (DEBUG) { System.out.println("EnhancedForStatementHeader ::=..."); }  //$NON-NLS-1$
				consumeEnhancedForStatementHeader();
				break;

			case 955:
				if (DEBUG) { System.out.println("SingleStaticImportDeclaration ::=..."); }  //$NON-NLS-1$
				consumeImportDeclaration();
				break;

			case 956:
				if (DEBUG) { System.out.println("SingleStaticImportDeclarationName ::= import static Name"); }  //$NON-NLS-1$
				consumeSingleStaticImportDeclarationName();
				break;

			case 957:
				if (DEBUG) { System.out.println("StaticImportOnDemandDeclaration ::=..."); }  //$NON-NLS-1$
				consumeImportDeclaration();
				break;

			case 958:
				if (DEBUG) { System.out.println("StaticImportOnDemandDeclarationName ::= import static..."); }  //$NON-NLS-1$
				consumeStaticImportOnDemandDeclarationName();
				break;

			case 959:
				if (DEBUG) { System.out.println("TypeArguments ::= LESS TypeArgumentList1"); }  //$NON-NLS-1$
				consumeTypeArguments();
				break;

			case 960:
				if (DEBUG) { System.out.println("OnlyTypeArguments ::= LESS TypeArgumentList1"); }  //$NON-NLS-1$
				consumeOnlyTypeArguments();
				break;

			case 962:
				if (DEBUG) { System.out.println("TypeArgumentList1 ::= TypeArgumentList COMMA..."); }  //$NON-NLS-1$
				consumeTypeArgumentList1();
				break;

			case 964:
				if (DEBUG) { System.out.println("TypeArgumentList ::= TypeArgumentList COMMA TypeArgument"); }  //$NON-NLS-1$
				consumeTypeArgumentList();
				break;

			case 965:
				if (DEBUG) { System.out.println("TypeArgument ::= ReferenceType"); }  //$NON-NLS-1$
				consumeTypeArgument();
				break;

			case 969:
				if (DEBUG) { System.out.println("ReferenceType1 ::= ReferenceType GREATER"); }  //$NON-NLS-1$
				consumeReferenceType1();
				break;

			case 970:
				if (DEBUG) { System.out.println("ReferenceType1 ::= ClassOrInterface LESS..."); }  //$NON-NLS-1$
				consumeTypeArgumentReferenceType1();
				break;

			case 972:
				if (DEBUG) { System.out.println("TypeArgumentList2 ::= TypeArgumentList COMMA..."); }  //$NON-NLS-1$
				consumeTypeArgumentList2();
				break;

			case 975:
				if (DEBUG) { System.out.println("ReferenceType2 ::= ReferenceType RIGHT_SHIFT"); }  //$NON-NLS-1$
				consumeReferenceType2();
				break;

			case 976:
				if (DEBUG) { System.out.println("ReferenceType2 ::= ClassOrInterface LESS..."); }  //$NON-NLS-1$
				consumeTypeArgumentReferenceType2();
				break;

			case 978:
				if (DEBUG) { System.out.println("TypeArgumentList3 ::= TypeArgumentList COMMA..."); }  //$NON-NLS-1$
				consumeTypeArgumentList3();
				break;

			case 981:
				if (DEBUG) { System.out.println("ReferenceType3 ::= ReferenceType UNSIGNED_RIGHT_SHIFT"); }  //$NON-NLS-1$
				consumeReferenceType3();
				break;

			case 982:
				if (DEBUG) { System.out.println("Wildcard ::= TypeAnnotationsopt QUESTION"); }  //$NON-NLS-1$
				consumeWildcard();
				break;

			case 983:
				if (DEBUG) { System.out.println("Wildcard ::= TypeAnnotationsopt QUESTION WildcardBounds"); }  //$NON-NLS-1$
				consumeWildcardWithBounds();
				break;

			case 984:
				if (DEBUG) { System.out.println("WildcardBounds ::= extends ReferenceType"); }  //$NON-NLS-1$
				consumeWildcardBoundsExtends();
				break;

			case 985:
				if (DEBUG) { System.out.println("WildcardBounds ::= super ReferenceType"); }  //$NON-NLS-1$
				consumeWildcardBoundsSuper();
				break;

			case 986:
				if (DEBUG) { System.out.println("Wildcard1 ::= TypeAnnotationsopt QUESTION GREATER"); }  //$NON-NLS-1$
				consumeWildcard1();
				break;

			case 987:
				if (DEBUG) { System.out.println("Wildcard1 ::= TypeAnnotationsopt QUESTION..."); }  //$NON-NLS-1$
				consumeWildcard1WithBounds();
				break;

			case 988:
				if (DEBUG) { System.out.println("WildcardBounds1 ::= extends ReferenceType1"); }  //$NON-NLS-1$
				consumeWildcardBounds1Extends();
				break;

			case 989:
				if (DEBUG) { System.out.println("WildcardBounds1 ::= super ReferenceType1"); }  //$NON-NLS-1$
				consumeWildcardBounds1Super();
				break;

			case 990:
				if (DEBUG) { System.out.println("Wildcard2 ::= TypeAnnotationsopt QUESTION RIGHT_SHIFT"); }  //$NON-NLS-1$
				consumeWildcard2();
				break;

			case 991:
				if (DEBUG) { System.out.println("Wildcard2 ::= TypeAnnotationsopt QUESTION..."); }  //$NON-NLS-1$
				consumeWildcard2WithBounds();
				break;

			case 992:
				if (DEBUG) { System.out.println("WildcardBounds2 ::= extends ReferenceType2"); }  //$NON-NLS-1$
				consumeWildcardBounds2Extends();
				break;

			case 993:
				if (DEBUG) { System.out.println("WildcardBounds2 ::= super ReferenceType2"); }  //$NON-NLS-1$
				consumeWildcardBounds2Super();
				break;

			case 994:
				if (DEBUG) { System.out.println("Wildcard3 ::= TypeAnnotationsopt QUESTION..."); }  //$NON-NLS-1$
				consumeWildcard3();
				break;

			case 995:
				if (DEBUG) { System.out.println("Wildcard3 ::= TypeAnnotationsopt QUESTION..."); }  //$NON-NLS-1$
				consumeWildcard3WithBounds();
				break;

			case 996:
				if (DEBUG) { System.out.println("WildcardBounds3 ::= extends ReferenceType3"); }  //$NON-NLS-1$
				consumeWildcardBounds3Extends();
				break;

			case 997:
				if (DEBUG) { System.out.println("WildcardBounds3 ::= super ReferenceType3"); }  //$NON-NLS-1$
				consumeWildcardBounds3Super();
				break;

			case 998:
				if (DEBUG) { System.out.println("TypeParameterHeader ::= TypeAnnotationsopt..."); }  //$NON-NLS-1$
				consumeTypeParameterHeader();
				break;

			case 999:
				if (DEBUG) { System.out.println("TypeParameters ::= LESS TypeParameterList1"); }  //$NON-NLS-1$
				consumeTypeParameters();
				break;

			case 1001:
				if (DEBUG) { System.out.println("TypeParameterList ::= TypeParameterList COMMA..."); }  //$NON-NLS-1$
				consumeTypeParameterList();
				break;

			case 1003:
				if (DEBUG) { System.out.println("TypeParameter ::= TypeParameterHeader extends..."); }  //$NON-NLS-1$
				consumeTypeParameterWithExtends();
				break;

			case 1004:
				if (DEBUG) { System.out.println("TypeParameter ::= TypeParameterHeader extends..."); }  //$NON-NLS-1$
				consumeTypeParameterWithExtendsAndBounds();
				break;

			case 1006:
				if (DEBUG) { System.out.println("AdditionalBoundList ::= AdditionalBoundList..."); }  //$NON-NLS-1$
				consumeAdditionalBoundList();
				break;

			case 1007:
				if (DEBUG) { System.out.println("AdditionalBound ::= AND ReferenceType"); }  //$NON-NLS-1$
				consumeAdditionalBound();
				break;

			case 1009:
				if (DEBUG) { System.out.println("TypeParameterList1 ::= TypeParameterList COMMA..."); }  //$NON-NLS-1$
				consumeTypeParameterList1();
				break;

			case 1010:
				if (DEBUG) { System.out.println("TypeParameter1 ::= TypeParameterHeader GREATER"); }  //$NON-NLS-1$
				consumeTypeParameter1();
				break;

			case 1011:
				if (DEBUG) { System.out.println("TypeParameter1 ::= TypeParameterHeader extends..."); }  //$NON-NLS-1$
				consumeTypeParameter1WithExtends();
				break;

			case 1012:
				if (DEBUG) { System.out.println("TypeParameter1 ::= TypeParameterHeader extends..."); }  //$NON-NLS-1$
				consumeTypeParameter1WithExtendsAndBounds();
				break;

			case 1014:
				if (DEBUG) { System.out.println("AdditionalBoundList1 ::= AdditionalBoundList..."); }  //$NON-NLS-1$
				consumeAdditionalBoundList1();
				break;

			case 1015:
				if (DEBUG) { System.out.println("AdditionalBound1 ::= AND ReferenceType1"); }  //$NON-NLS-1$
				consumeAdditionalBound1();
				break;

			case 1021:
				if (DEBUG) { System.out.println("UnaryExpression_NotName ::= PLUS PushPosition..."); }  //$NON-NLS-1$
				consumeUnaryExpression(OperatorIds.PLUS);
				break;

			case 1022:
				if (DEBUG) { System.out.println("UnaryExpression_NotName ::= MINUS PushPosition..."); }  //$NON-NLS-1$
				consumeUnaryExpression(OperatorIds.MINUS);
				break;

			case 1025:
				if (DEBUG) { System.out.println("UnaryExpressionNotPlusMinus_NotName ::= TWIDDLE..."); }  //$NON-NLS-1$
				consumeUnaryExpression(OperatorIds.TWIDDLE);
				break;

			case 1026:
				if (DEBUG) { System.out.println("UnaryExpressionNotPlusMinus_NotName ::= NOT..."); }  //$NON-NLS-1$
				consumeUnaryExpression(OperatorIds.NOT);
				break;

			case 1029:
				if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.MULTIPLY);
				break;

			case 1030:
				if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::= NameOrAj MULTIPLY"); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.MULTIPLY);
				break;

			case 1031:
				if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.DIVIDE);
				break;

			case 1032:
				if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::= NameOrAj DIVIDE..."); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.DIVIDE);
				break;

			case 1033:
				if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.REMAINDER);
				break;

			case 1034:
				if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::= NameOrAj REMAINDER"); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.REMAINDER);
				break;

			case 1036:
				if (DEBUG) { System.out.println("AdditiveExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.PLUS);
				break;

			case 1037:
				if (DEBUG) { System.out.println("AdditiveExpression_NotName ::= NameOrAj PLUS..."); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.PLUS);
				break;

			case 1038:
				if (DEBUG) { System.out.println("AdditiveExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.MINUS);
				break;

			case 1039:
				if (DEBUG) { System.out.println("AdditiveExpression_NotName ::= NameOrAj MINUS..."); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.MINUS);
				break;

			case 1041:
				if (DEBUG) { System.out.println("ShiftExpression_NotName ::= ShiftExpression_NotName..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.LEFT_SHIFT);
				break;

			case 1042:
				if (DEBUG) { System.out.println("ShiftExpression_NotName ::= NameOrAj LEFT_SHIFT..."); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.LEFT_SHIFT);
				break;

			case 1043:
				if (DEBUG) { System.out.println("ShiftExpression_NotName ::= ShiftExpression_NotName..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.RIGHT_SHIFT);
				break;

			case 1044:
				if (DEBUG) { System.out.println("ShiftExpression_NotName ::= NameOrAj RIGHT_SHIFT..."); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.RIGHT_SHIFT);
				break;

			case 1045:
				if (DEBUG) { System.out.println("ShiftExpression_NotName ::= ShiftExpression_NotName..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.UNSIGNED_RIGHT_SHIFT);
				break;

			case 1046:
				if (DEBUG) { System.out.println("ShiftExpression_NotName ::= NameOrAj..."); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.UNSIGNED_RIGHT_SHIFT);
				break;

			case 1048:
				if (DEBUG) { System.out.println("RelationalExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.LESS);
				break;

			case 1049:
				if (DEBUG) { System.out.println("RelationalExpression_NotName ::= Name LESS..."); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.LESS);
				break;

			case 1050:
				if (DEBUG) { System.out.println("RelationalExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.GREATER);
				break;

			case 1051:
				if (DEBUG) { System.out.println("RelationalExpression_NotName ::= NameOrAj GREATER..."); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.GREATER);
				break;

			case 1052:
				if (DEBUG) { System.out.println("RelationalExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.LESS_EQUAL);
				break;

			case 1053:
				if (DEBUG) { System.out.println("RelationalExpression_NotName ::= NameOrAj LESS_EQUAL..."); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.LESS_EQUAL);
				break;

			case 1054:
				if (DEBUG) { System.out.println("RelationalExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.GREATER_EQUAL);
				break;

			case 1055:
				if (DEBUG) { System.out.println("RelationalExpression_NotName ::= NameOrAj GREATER_EQUAL"); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.GREATER_EQUAL);
				break;

			case 1057:
				if (DEBUG) { System.out.println("InstanceofExpression_NotName ::= NameOrAj InstanceofRHS"); }  //$NON-NLS-1$
				consumeInstanceOfExpressionWithName();
				break;

			case 1058:
				if (DEBUG) { System.out.println("InstanceofExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeInstanceOfExpression();
				break;

			case 1060:
				if (DEBUG) { System.out.println("EqualityExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeEqualityExpression(OperatorIds.EQUAL_EQUAL);
				break;

			case 1061:
				if (DEBUG) { System.out.println("EqualityExpression_NotName ::= NameOrAj EQUAL_EQUAL..."); }  //$NON-NLS-1$
				consumeEqualityExpressionWithName(OperatorIds.EQUAL_EQUAL);
				break;

			case 1062:
				if (DEBUG) { System.out.println("EqualityExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeEqualityExpression(OperatorIds.NOT_EQUAL);
				break;

			case 1063:
				if (DEBUG) { System.out.println("EqualityExpression_NotName ::= NameOrAj NOT_EQUAL..."); }  //$NON-NLS-1$
				consumeEqualityExpressionWithName(OperatorIds.NOT_EQUAL);
				break;

			case 1065:
				if (DEBUG) { System.out.println("AndExpression_NotName ::= AndExpression_NotName AND..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.AND);
				break;

			case 1066:
				if (DEBUG) { System.out.println("AndExpression_NotName ::= NameOrAj AND..."); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.AND);
				break;

			case 1068:
				if (DEBUG) { System.out.println("ExclusiveOrExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.XOR);
				break;

			case 1069:
				if (DEBUG) { System.out.println("ExclusiveOrExpression_NotName ::= NameOrAj XOR..."); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.XOR);
				break;

			case 1071:
				if (DEBUG) { System.out.println("InclusiveOrExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.OR);
				break;

			case 1072:
				if (DEBUG) { System.out.println("InclusiveOrExpression_NotName ::= NameOrAj OR..."); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.OR);
				break;

			case 1074:
				if (DEBUG) { System.out.println("ConditionalAndExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.AND_AND);
				break;

			case 1075:
				if (DEBUG) { System.out.println("ConditionalAndExpression_NotName ::= NameOrAj AND_AND"); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.AND_AND);
				break;

			case 1077:
				if (DEBUG) { System.out.println("ConditionalOrExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeBinaryExpression(OperatorIds.OR_OR);
				break;

			case 1078:
				if (DEBUG) { System.out.println("ConditionalOrExpression_NotName ::= NameOrAj OR_OR..."); }  //$NON-NLS-1$
				consumeBinaryExpressionWithName(OperatorIds.OR_OR);
				break;

			case 1080:
				if (DEBUG) { System.out.println("ConditionalExpression_NotName ::=..."); }  //$NON-NLS-1$
				consumeConditionalExpression(OperatorIds.QUESTIONCOLON);
				break;

			case 1081:
				if (DEBUG) { System.out.println("ConditionalExpression_NotName ::= NameOrAj QUESTION..."); }  //$NON-NLS-1$
				consumeConditionalExpressionWithName(OperatorIds.QUESTIONCOLON);
				break;

			case 1085:
				if (DEBUG) { System.out.println("AnnotationTypeDeclarationHeaderName ::= Modifiers AT..."); }  //$NON-NLS-1$
				consumeAnnotationTypeDeclarationHeaderName();
				break;

			case 1086:
				if (DEBUG) { System.out.println("AnnotationTypeDeclarationHeaderName ::= Modifiers AT..."); }  //$NON-NLS-1$
				consumeAnnotationTypeDeclarationHeaderNameWithTypeParameters();
				break;

			case 1087:
				if (DEBUG) { System.out.println("AnnotationTypeDeclarationHeaderName ::= AT..."); }  //$NON-NLS-1$
				consumeAnnotationTypeDeclarationHeaderNameWithTypeParameters();
				break;

			case 1088:
				if (DEBUG) { System.out.println("AnnotationTypeDeclarationHeaderName ::= AT..."); }  //$NON-NLS-1$
				consumeAnnotationTypeDeclarationHeaderName();
				break;

			case 1089:
				if (DEBUG) { System.out.println("AnnotationTypeDeclarationHeader ::=..."); }  //$NON-NLS-1$
				consumeAnnotationTypeDeclarationHeader();
				break;

			case 1090:
				if (DEBUG) { System.out.println("AnnotationTypeDeclaration ::=..."); }  //$NON-NLS-1$
				consumeAnnotationTypeDeclaration();
				break;

			case 1092:
				if (DEBUG) { System.out.println("AnnotationTypeMemberDeclarationsopt ::="); }  //$NON-NLS-1$
				consumeEmptyAnnotationTypeMemberDeclarationsopt();
				break;

			case 1093:
				if (DEBUG) { System.out.println("AnnotationTypeMemberDeclarationsopt ::= NestedType..."); }  //$NON-NLS-1$
				consumeAnnotationTypeMemberDeclarationsopt();
				break;

			case 1095:
				if (DEBUG) { System.out.println("AnnotationTypeMemberDeclarations ::=..."); }  //$NON-NLS-1$
				consumeAnnotationTypeMemberDeclarations();
				break;

			case 1096:
				if (DEBUG) { System.out.println("AnnotationMethodHeaderName ::= Modifiersopt..."); }  //$NON-NLS-1$
				consumeMethodHeaderNameWithTypeParameters(true);
				break;

			case 1097:
				if (DEBUG) { System.out.println("AnnotationMethodHeaderName ::= Modifiersopt Type..."); }  //$NON-NLS-1$
				consumeMethodHeaderName(true);
				break;

			case 1098:
				if (DEBUG) { System.out.println("AnnotationMethodHeaderDefaultValueopt ::="); }  //$NON-NLS-1$
				consumeEmptyMethodHeaderDefaultValue();
				break;

			case 1099:
				if (DEBUG) { System.out.println("AnnotationMethodHeaderDefaultValueopt ::= DefaultValue"); }  //$NON-NLS-1$
				consumeMethodHeaderDefaultValue();
				break;

			case 1100:
				if (DEBUG) { System.out.println("AnnotationMethodHeader ::= AnnotationMethodHeaderName"); }  //$NON-NLS-1$
				consumeMethodHeader();
				break;

			case 1101:
				if (DEBUG) { System.out.println("AnnotationTypeMemberDeclaration ::=..."); }  //$NON-NLS-1$
				consumeAnnotationTypeMemberDeclaration();
				break;

			case 1109:
				if (DEBUG) { System.out.println("AnnotationName ::= AT UnannotatableNameOrAj"); }  //$NON-NLS-1$
				consumeAnnotationName();
				break;

			case 1110:
				if (DEBUG) { System.out.println("NormalAnnotation ::= AnnotationName LPAREN..."); }  //$NON-NLS-1$
				consumeNormalAnnotation(false);
				break;

			case 1111:
				if (DEBUG) { System.out.println("MemberValuePairsopt ::="); }  //$NON-NLS-1$
				consumeEmptyMemberValuePairsopt();
				break;

			case 1114:
				if (DEBUG) { System.out.println("MemberValuePairs ::= MemberValuePairs COMMA..."); }  //$NON-NLS-1$
				consumeMemberValuePairs();
				break;

			case 1115:
				if (DEBUG) { System.out.println("MemberValuePair ::= SimpleNameOrAj EQUAL..."); }  //$NON-NLS-1$
				consumeMemberValuePair();
				break;

			case 1116:
				if (DEBUG) { System.out.println("EnterMemberValue ::="); }  //$NON-NLS-1$
				consumeEnterMemberValue();
				break;

			case 1117:
				if (DEBUG) { System.out.println("ExitMemberValue ::="); }  //$NON-NLS-1$
				consumeExitMemberValue();
				break;

			case 1119:
				if (DEBUG) { System.out.println("MemberValue ::= NameOrAj"); }  //$NON-NLS-1$
				consumeMemberValueAsName();
				break;

			case 1122:
				if (DEBUG) { System.out.println("MemberValueArrayInitializer ::=..."); }  //$NON-NLS-1$
				consumeMemberValueArrayInitializer();
				break;

			case 1123:
				if (DEBUG) { System.out.println("MemberValueArrayInitializer ::=..."); }  //$NON-NLS-1$
				consumeMemberValueArrayInitializer();
				break;

			case 1124:
				if (DEBUG) { System.out.println("MemberValueArrayInitializer ::=..."); }  //$NON-NLS-1$
				consumeEmptyMemberValueArrayInitializer();
				break;

			case 1125:
				if (DEBUG) { System.out.println("MemberValueArrayInitializer ::=..."); }  //$NON-NLS-1$
				consumeEmptyMemberValueArrayInitializer();
				break;

			case 1126:
				if (DEBUG) { System.out.println("EnterMemberValueArrayInitializer ::="); }  //$NON-NLS-1$
				consumeEnterMemberValueArrayInitializer();
				break;

			case 1128:
				if (DEBUG) { System.out.println("MemberValues ::= MemberValues COMMA MemberValue"); }  //$NON-NLS-1$
				consumeMemberValues();
				break;

			case 1129:
				if (DEBUG) { System.out.println("MarkerAnnotation ::= AnnotationName"); }  //$NON-NLS-1$
				consumeMarkerAnnotation(false);
				break;

			case 1130:
				if (DEBUG) { System.out.println("SingleMemberAnnotationMemberValue ::= MemberValue"); }  //$NON-NLS-1$
				consumeSingleMemberAnnotationMemberValue();
				break;

			case 1131:
				if (DEBUG) { System.out.println("SingleMemberAnnotation ::= AnnotationName LPAREN..."); }  //$NON-NLS-1$
				consumeSingleMemberAnnotation(false);
				break;

			case 1132:
				if (DEBUG) { System.out.println("RecoveryMethodHeaderName ::= Modifiersopt..."); }  //$NON-NLS-1$
				consumeRecoveryMethodHeaderNameWithTypeParameters();
				break;

			case 1133:
				if (DEBUG) { System.out.println("RecoveryMethodHeaderName ::= Modifiersopt Type..."); }  //$NON-NLS-1$
				consumeRecoveryMethodHeaderName();
				break;

			case 1134:
				if (DEBUG) { System.out.println("RecoveryMethodHeaderName ::= ModifiersWithDefault..."); }  //$NON-NLS-1$
				consumeRecoveryMethodHeaderNameWithTypeParameters();
				break;

			case 1135:
				if (DEBUG) { System.out.println("RecoveryMethodHeaderName ::= ModifiersWithDefault Type"); }  //$NON-NLS-1$
				consumeRecoveryMethodHeaderName();
				break;

			case 1136:
				if (DEBUG) { System.out.println("RecoveryMethodHeader ::= RecoveryMethodHeaderName..."); }  //$NON-NLS-1$
				consumeMethodHeader();
				break;

			case 1137:
				if (DEBUG) { System.out.println("RecoveryMethodHeader ::= RecoveryMethodHeaderName..."); }  //$NON-NLS-1$
				consumeMethodHeader();
				break;

		}
	}




// AspectJ: new method
// TODO - review if this is right, should we make the choice in the java.g file?
protected void consumeQualifiedName() {
	consumeQualifiedName(false);
}




	// Helpers


	private void consumeIntertypeClassHeader() {
		TypeDeclaration typeDecl = (TypeDeclaration) this.astStack[this.astPtr];
		if (this.currentToken == TokenNameLBRACE) {
			typeDecl.bodyStart = this.scanner.currentPosition;
		}
		if (this.currentElement != null) {
			this.restartRecovery = true; // used to avoid branching back into the regular automaton
		}
		// flush the comments related to the class header
		this.scanner.commentPtr = -1;
	}

	private void consumeIntertypeClassDeclaration() {
		int length;
		if ((length = this.astLengthStack[this.astLengthPtr--]) != 0) {
			//there are length declarations
			//dispatch according to the type of the declarations
			dispatchDeclarationInto(length);
		}

		TypeDeclaration typeDecl = (TypeDeclaration) this.astStack[this.astPtr];


		//convert constructor that do not have the type's name into methods
		boolean hasConstructor = typeDecl.checkConstructors((Parser)this);

		//add the default constructor when needed (interface don't have it)
		if (!hasConstructor) {
			switch(TypeDeclaration.kind(typeDecl.modifiers)) {
				case TypeDeclaration.CLASS_DECL :
				case TypeDeclaration.ENUM_DECL :
					boolean insideFieldInitializer = false;
					if (this.diet) {
						for (int i = this.nestedType; i > 0; i--){
							if (this.variablesCounter[i] > 0) {
								insideFieldInitializer = true;
								break;
							}
						}
					}
					typeDecl.createDefaultConstructor(!this.diet || insideFieldInitializer, true);
			}
		}
		//always add <clinit> (will be remove at code gen time if empty)
		if (this.scanner.containsAssertKeyword) {
			typeDecl.bits |= ASTNode.ContainsAssertion;
		}
		typeDecl.addClinit();
		typeDecl.bodyEnd = this.endStatementPosition;
		if (length == 0 && !containsComment(typeDecl.bodyStart, typeDecl.bodyEnd)) {
			typeDecl.bits |= ASTNode.UndocumentedEmptyBlock;
		}

		typeDecl.declarationSourceEnd = flushCommentsDefinedPriorTo(this.endStatementPosition);

	}

	private void consumeIntertypeTypeHeaderNameWithTypeParameters() {
		TypeDeclaration typeDecl = (TypeDeclaration)this.astStack[this.astPtr];

		// consume type parameters
		int length = this.genericsLengthStack[this.genericsLengthPtr--];
		this.genericsPtr -= length;
		System.arraycopy(this.genericsStack, this.genericsPtr + 1, typeDecl.typeParameters = new TypeParameter[length], 0, length);

		typeDecl.bodyStart = typeDecl.typeParameters[length-1].declarationSourceEnd + 1;

		this.listTypeParameterLength = 0;

		if (this.currentElement != null) { // is recovering
			RecoveredType recoveredType = (RecoveredType) this.currentElement;
			recoveredType.pendingTypeParameters = null;

			this.lastCheckPoint = typeDecl.bodyStart;
		}
	}

	private void consumeIntertypeClassHeaderName(boolean b) {
		TypeDeclaration typeDecl = declarationFactory.createIntertypeMemberClassDeclaration(this.compilationUnit.compilationResult);
		if (this.nestedMethod[this.nestedType] == 0) {
			if (this.nestedType != 0) {
				typeDecl.bits |= ASTNode.IsMemberType;
			}
		} else {
			// Record that the block has a declaration for local types
			typeDecl.bits |= ASTNode.IsLocalType;
			markEnclosingMemberWithLocalType();
			blockReal();
		}

		this.display();
		//highlight the name of the type
		long pos = this.identifierPositionStack[this.identifierPtr];
		typeDecl.sourceEnd = (int) pos;
		typeDecl.sourceStart = (int) (pos >>> 32);
		typeDecl.name = this.identifierStack[this.identifierPtr--];
		this.identifierLengthPtr--;

		//onType
		if (b) {
			pushOnGenericsIdentifiersLengthStack(this.identifierLengthStack[this.identifierLengthPtr]);
			//consumeClassOrInterfaceName();
		} else {
			consumeClassOrInterfaceName();
		}
		TypeReference onType = getTypeReference(0);

		declarationFactory.setOnType(typeDecl,onType);

		//compute the declaration source too
		// 'class' and 'interface' push two int positions: the beginning of the class token and its end.
		// we want to keep the beginning position but get rid of the end position
		// it is only used for the ClassLiteralAccess positions.
		typeDecl.declarationSourceStart = this.intStack[this.intPtr--];
		this.intPtr--; // remove the end position of the class token

		typeDecl.modifiersSourceStart = this.intStack[this.intPtr--];
		typeDecl.modifiers = this.intStack[this.intPtr--];
		if (typeDecl.modifiersSourceStart >= 0) {
			typeDecl.declarationSourceStart = typeDecl.modifiersSourceStart;
		}

		// Store secondary info
		if ((typeDecl.bits & ASTNode.IsMemberType) == 0 && (typeDecl.bits & ASTNode.IsLocalType) == 0) {
			if (this.compilationUnit != null && !CharOperation.equals(typeDecl.name, this.compilationUnit.getMainTypeName())) {
				typeDecl.bits |= ASTNode.IsSecondaryType;
			}
		}

		// consume annotations
		int length;
		if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
			System.arraycopy(
				this.expressionStack,
				(this.expressionPtr -= length) + 1,
				typeDecl.annotations = new Annotation[length],
				0,
				length);
		}
		typeDecl.bodyStart = typeDecl.sourceEnd + 1;
		pushOnAstStack(typeDecl);

		this.listLength = 0; // will be updated when reading super-interfaces
		// recovery
		if (this.currentElement != null){
			this.lastCheckPoint = typeDecl.bodyStart;
			this.currentElement = this.currentElement.add(typeDecl, 0);
			this.lastIgnoredToken = -1;
		}
		// javadoc
		typeDecl.javadoc = this.javadoc;
		this.javadoc = null;
		this.display();

	}

	protected ASTNode popPointcutDesignator(String terminator) {
		ASTNode tokens = popPseudoTokens(terminator);
		return declarationFactory.createPointcutDesignator(this, tokens);
	}

	protected ASTNode popPseudoTokens(String terminator) {
		consumePseudoToken(terminator);
		consumePseudoTokens();
		//System.out.println("next token is: " + new String(scanner.getCurrentTokenSource()));

		int length = astLengthStack[astLengthPtr--];
		astPtr -= length;

		//arguments
		ASTNode[] tokens = new ASTNode[length];
		System.arraycopy(astStack, astPtr + 1, tokens, 0, length);
		//md.bodyStart = rParenPos+1;
		listLength = 0; // reset listLength after having read all parameters

		return declarationFactory.createPseudoTokensFrom(tokens,this.compilationUnit.compilationResult());
			//	new PseudoTokens(tokens, makeSourceContext(this.compilationUnit.compilationResult()));
	}

//	private ISourceContext makeSourceContext(CompilationResult compilationResult) {
//		return new EclipseSourceContext(compilationResult);
//	}


	private void swapAstStack() {
		ASTNode top = astStack[astPtr];
		ASTNode next = astStack[astPtr-1];
		astStack[astPtr] = next;
		astStack[astPtr-1] = top;
	}



	/**
	 * Recovery rule for when someone tries to use * or + in an ITD
	 */
	private void consumeInterTypeFieldHeaderIllegallyAttemptingToUseATypePattern(String badToken) {
		consumeInterTypeFieldHeader(false); // make the best of what we did get
		MethodDeclaration errorNode = (MethodDeclaration) astStack[astPtr];
		problemReporter().parseErrorDeleteToken(errorNode.sourceStart -2,  // '+.'
				                                errorNode.sourceStart,
				                                TokenNameIdentifier,
				                                badToken.toCharArray(),
				                                badToken);
	}

	/**
	 * Recovery rule for when someone tries to use * or + in an ITD
	 */
	private void consumeInterTypeConstructorHeaderNameIllegallyUsingTypePattern(String badToken) {
		consumeInterTypeConstructorHeaderName(false,false); // make the best of what we did get
		MethodDeclaration errorNode = (MethodDeclaration) astStack[astPtr];
		problemReporter().parseErrorDeleteToken(errorNode.sourceStart -2,  // '+.'
				                                errorNode.sourceStart,
				                                TokenNameIdentifier,
				                                badToken.toCharArray(),
				                                badToken);
	}

	/**
	 * Recovery rule for when someone tries to use * or + in an ITD
	 */
	private void consumeInterTypeMethodHeaderNameIllegallyUsingTypePattern(String badToken) {
		consumeInterTypeMethodHeaderName(false,false); // make the best of what we did get
		MethodDeclaration errorNode = (MethodDeclaration) astStack[astPtr];
		problemReporter().parseErrorDeleteToken(errorNode.sourceStart -2,  // '+.'
				                                errorNode.sourceStart,
				                                TokenNameIdentifier,
				                                badToken.toCharArray(),
				                                badToken);
	}

	/**
	 * Recovery rule for when around advice is specified without a return type
	 */
	private void consumeAroundHeaderNameMissingReturnType() {
		problemReporter().parseErrorInsertToComplete(scanner.startPosition, scanner.currentPosition, "return type","around advice declaration");
		this.restartRecovery = true;
	}

	/**
	 * Recovery rule for a screwed up declaration
	 */
	private void consumeBadHeader() {
		// we read... modifiersopt QualifiedName LPAREN FormalParameterListopt RPAREN
		problemReporter().parseErrorReplaceTokens(scanner.startPosition, scanner.currentPosition, "valid member declaration");
		this.restartRecovery = true;
	}

@Override
protected void consumeSimpleAssertStatement() {
	super.consumeSimpleAssertStatement();
}

/**
 * this method is called by the parser when processing inter-type declarations. We have
 * just finished parsing the type parameters following the OnType of the ITD. Unfortunatey
 * we parsed them as TypeParameter(s) whereas we're going to create a type reference for the
 * on type (and type references can't have type parameters, only type declarations can). Therefore
 * we replace the TypeParameter(s) with SingleTypeReference(s) so that everything will go
 * smoothly down the line.
 */
private void convertTypeParametersToSingleTypeReferences() {
	for(int typeParameterIndex = 0; typeParameterIndex < genericsLengthStack[genericsLengthPtr]; typeParameterIndex++) {
		TypeParameter tp = (TypeParameter) genericsStack[genericsPtr - typeParameterIndex];
		SingleTypeReference str = new SingleTypeReference(tp.name,tp.declarationSourceStart);
		genericsStack[genericsPtr - typeParameterIndex] = str;
	}
}

// AspectJ: added so the super ctor in TheOriginalJDTParser is visible
public Parser() {}

public Parser(
	ProblemReporter problemReporter,
	boolean optimizeStringLiterals) {
	super(problemReporter, optimizeStringLiterals);
}

// don't try to recover if we're parsing AspectJ constructs
@Override
protected boolean shouldTryToRecover() {
	int index = 0;
	ASTNode node;
	while (index < astStack.length && (node = astStack[index++]) != null) {
		if (!declarationFactory.shouldTryToRecover(node)) {
			return false;
		}
	}
	return true;
}

protected void pushOnAspectIntStack(int pos) {

	int stackLength = this.aspectIntStack.length;
	if (++this.aspectIntPtr >= stackLength) {
		System.arraycopy(
			this.aspectIntStack, 0,
			this.aspectIntStack = new int[stackLength + StackIncrement], 0,
			stackLength);
	}
	this.aspectIntStack[this.aspectIntPtr] = pos;
}
}
