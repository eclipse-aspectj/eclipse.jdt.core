/* *******************************************************************
 * Copyright (c) 2005 Contributors.
 * All rights reserved. 
 * This program and the accompanying materials are made available 
 * under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution and is available at 
 * http://eclipse.org/legal/epl-v10.html 
 *  
 * Contributors: 
 *   Adrian Colyer			Initial implementation
 * ******************************************************************/
package org.aspectj.ajdt.internal.compiler.parser;

import org.eclipse.jdt.internal.compiler.problem.ProblemReporter;
import org.eclipse.jdt.internal.compiler.parser.Parser;
import org.eclipse.jdt.internal.compiler.parser.TheOriginalJDTParserClass;


/**
 * The parser handles pointcut expressions as as stream of pseudo-tokens.
 * If one of these is an identifier and the -warn:assertIdentifier option is 
 * set, an erroneous warning message will be issued that assert should not
 * be used as an identifier (see bug 112756). This aspect tracks whether
 * we are in a pseudo token stream or not, and suppresses the warning if so.
 */
public aspect AllowAssertAndEnumAsIdentifierTokensInPointcutExpressions {

	private boolean inPseudoTokenStream = false;
	
	pointcut raiseAssertAsIdentifierWarning() :
		call(* ProblemReporter.useAssertAsAnIdentifier(..)) &&
		within(TheOriginalJDTParserClass+);

	pointcut raiseEnumAsIdentifierWarning() :
		call(* ProblemReporter.useEnumAsAnIdentifier(..)) &&
		within(TheOriginalJDTParserClass+);
	
	pointcut completingPseudoTokenStream() :
		execution(* Parser.consumePointcut*(..)) ||
		execution(* Parser.consume*Advice*(..));
	
	pointcut processingPseudoToken() : 
		execution(* Parser.consumePseudoToken*(..)) &&
		! completingPseudoTokenStream();
	
	before() : processingPseudoToken() {
		this.inPseudoTokenStream = true;
	}
	
	after() returning : completingPseudoTokenStream() {
		this.inPseudoTokenStream = false;
	}
	
	/**
	 * only raise the warning if we are NOT in a pseudo-token stream
	 */
	void around() : raiseAssertAsIdentifierWarning() || raiseEnumAsIdentifierWarning() {
		if (!inPseudoTokenStream) proceed();
	}
}
