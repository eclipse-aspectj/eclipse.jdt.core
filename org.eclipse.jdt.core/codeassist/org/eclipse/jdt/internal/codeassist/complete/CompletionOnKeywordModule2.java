/*******************************************************************************
 * Copyright (c) 2016 IBM Corporation and others.
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
 *
 *******************************************************************************/
package org.eclipse.jdt.internal.codeassist.complete;

import org.eclipse.jdt.internal.compiler.ast.ModuleReference;

public class CompletionOnKeywordModule2 extends ModuleReference implements CompletionOnKeyword {
	private final char[] token;
	private final char[][] possibleKeywords;

	public CompletionOnKeywordModule2(char[] token, long pos, char[][] possibleKeywords) {
		super(new char[][] {token}, new long[] {pos}); // dummy
		this.token = token;
		this.possibleKeywords = possibleKeywords;
		this.sourceStart = (int) (pos>>>32)  ;
		this.sourceEnd = (int) (pos & 0x00000000FFFFFFFFL);
	}

	@Override
	public char[] getToken() {
		return this.token;
	}

	@Override
	public char[][] getPossibleKeywords() {
		return this.possibleKeywords;
	}
}
