/*******************************************************************************
 * Copyright (c) 2000, 2022 IBM Corporation and others.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.jdt.internal.compiler.parser;

/*An interface that contains static declarations for some basic information
 about the parser such as the number of rules in the grammar, the starting state, etc...*/
public interface ParserBasicInformation {
	// AspectJ: Tokens taken from javadef.java, generated by jikespg according to
	// https://www.eclipse.org/jdt/core/howto/generate%20parser/generateParser.html
	int
		ERROR_SYMBOL      = 145,
		MAX_NAME_LENGTH   = 53,
		NUM_STATES        = 1410,
		NT_OFFSET         = 145,
		SCOPE_UBOUND      = 405,
		SCOPE_SIZE        = 406,
		LA_STATE_OFFSET   = 21522,
		MAX_LA            = 1,
		NUM_RULES         = 1140,
		NUM_TERMINALS     = 145,
		NUM_NON_TERMINALS = 497,
		NUM_SYMBOLS       = 642,
		START_STATE       = 1573,
		EOFT_SYMBOL       = 73,
		EOLT_SYMBOL       = 73,
		ACCEPT_ACTION     = 21521,
		ERROR_ACTION      = 21522;
}
