/* *******************************************************************
 * Copyright (c) 2002 Palo Alto Research Center, Incorporated (PARC).
 *               2003,2004 contributors
 * All rights reserved.
 * This program and the accompanying materials are made available
 * under the terms of the Eclipse Public License v 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/org/documents/epl-2.0/EPL-2.0.txt
 *
 * Contributors:
 *     PARC     initial implementation
 * ******************************************************************/
package org.eclipse.jdt.internal.compiler.parser;

import org.eclipse.jdt.core.compiler.CharOperation;

// AspectJ Extension. The original scanner class is replaced by this one.
public class Scanner extends TheOriginalJDTScannerClass implements TerminalTokens {
	public Scanner(
		boolean tokenizeComments,
		boolean tokenizeWhiteSpace,
		boolean checkNonExternalizedStringLiterals,
		long sourceLevel,
		long complianceLevel,
		char[][] taskTags,
		char[][] taskPriorities,
		boolean isTaskCaseSensitive,
		boolean isPreviewEnabled) {
		super(
			tokenizeComments,
			tokenizeWhiteSpace,
			checkNonExternalizedStringLiterals,
			sourceLevel,
			complianceLevel,
			taskTags,
			taskPriorities,
			isTaskCaseSensitive,
			isPreviewEnabled);
	}

	public Scanner(
			boolean tokenizeComments,
			boolean tokenizeWhiteSpace,
			boolean checkNonExternalizedStringLiterals,
			long sourceLevel,
			long complianceLevel,
			char[][] taskTags,
			char[][] taskPriorities,
			boolean isTaskCaseSensitive) {
			super(
				tokenizeComments,
				tokenizeWhiteSpace,
				checkNonExternalizedStringLiterals,
				sourceLevel,
				complianceLevel,
				taskTags,
				taskPriorities,
				isTaskCaseSensitive,
				false);
		}


	public Scanner(
			boolean tokenizeComments,
			boolean tokenizeWhiteSpace,
			boolean checkNonExternalizedStringLiterals,
			long sourceLevel,
			char[][] taskTags,
			char[][] taskPriorities,
			boolean isTaskCaseSensitive,
			boolean previewEnabled) {
			super(
				tokenizeComments,
				tokenizeWhiteSpace,
				checkNonExternalizedStringLiterals,
				sourceLevel,
				taskTags,
				taskPriorities,
				isTaskCaseSensitive,
				previewEnabled);
		}

	public Scanner(
			boolean tokenizeComments,
			boolean tokenizeWhiteSpace,
			boolean checkNonExternalizedStringLiterals,
			long sourceLevel,
			char[][] taskTags,
			char[][] taskPriorities,
			boolean isTaskCaseSensitive
			) {
			super(
				tokenizeComments,
				tokenizeWhiteSpace,
				checkNonExternalizedStringLiterals,
				sourceLevel,
				taskTags,
				taskPriorities,
				isTaskCaseSensitive,
				false);
		}


	public Scanner() {
		super();
	}




	private static final char[] aspectV = "aspect".toCharArray(); //$NON-NLS-1$
	private static final char[] pointcutV = "pointcut".toCharArray(); //$NON-NLS-1$
	private static final char[] privilegedV = "privileged".toCharArray(); //$NON-NLS-1$
	private static final char[] beforeV = "before".toCharArray(); //$NON-NLS-1$
	private static final char[] afterV = "after".toCharArray(); //$NON-NLS-1$
	private static final char[] aroundV = "around".toCharArray(); //$NON-NLS-1$
	private static final char[] declareV = "declare".toCharArray(); //$NON-NLS-1$



	@Override
	public int scanIdentifierOrKeyword() {
		int kind = super.scanIdentifierOrKeyword();
		if (kind != TerminalTokens.TokenNameIdentifier) return kind;

		char[] contents = getCurrentIdentifierSource();

		//XXX performance here is less than optimal, but code simplicity is pretty damn good
		if (CharOperation.equals(aspectV, contents)) return TerminalTokens.TokenNameaspect;
		else if (CharOperation.equals(pointcutV, contents)) return TerminalTokens.TokenNamepointcut;
		else if (CharOperation.equals(privilegedV, contents)) return TerminalTokens.TokenNameprivileged;
		else if (CharOperation.equals(beforeV, contents)) return TerminalTokens.TokenNamebefore;
		else if (CharOperation.equals(afterV, contents)) return TerminalTokens.TokenNameafter;
		else if (CharOperation.equals(aroundV, contents)) return TerminalTokens.TokenNamearound;
		else if (CharOperation.equals(declareV, contents)) return TerminalTokens.TokenNamedeclare;

		return kind;
	}
}
