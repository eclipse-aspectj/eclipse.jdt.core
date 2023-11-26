/* *******************************************************************
 * Copyright (c) 2005 Contributors.
 * All rights reserved.
 * This program and the accompanying materials are made available
 * under the terms of the Eclipse Public License v 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/org/documents/epl-2.0/EPL-2.0.txt
 *
 * Contributors:
 *   Adrian Colyer			Initial implementation
 * ******************************************************************/
package org.aspectj.ajdt.internal.compiler.parser;

import org.eclipse.jdt.internal.compiler.problem.ProblemReporter;
import org.eclipse.jdt.internal.compiler.parser.Parser;

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
		within(Parser+);

	pointcut raiseEnumAsIdentifierWarning() :
		call(* ProblemReporter.useEnumAsAnIdentifier(..)) &&
		within(Parser+);

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
