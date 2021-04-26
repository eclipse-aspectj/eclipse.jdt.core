// AspectJ Extension
/*******************************************************************************
 * Copyright (c) 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.aspectj.ajdt.internal.compiler;

import org.eclipse.jdt.internal.compiler.Compiler;

/**
 * Factory for creating ICompilerAdapters according to some purpose
 */
public interface ICompilerAdapterFactory {
	ICompilerAdapter getAdapter(Compiler forCompiler);
}
// End AspectJ Extension