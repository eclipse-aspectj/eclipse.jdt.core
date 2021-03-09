/*******************************************************************************
 * Copyright (c) 2010 Contributors
 * and others.  All rights reserved. 
 * This program and the accompanying materials are made available under
 * the terms of the Common Public License v1.0 which accompanies this
 * distribution, available at http://www.eclipse.org/legal/cpl-v1.0.html
 * 
 * Contributors:
 *     Andy Clement
 ******************************************************************************/

package org.eclipse.jdt.internal.compiler.lookup;
// AspectJ Extension

/**
 * This interface is used by SourceTypeBinding to provide a delegated lookup
 * instance. It is used to support AspectJ's inter-type declarations.
 * 
 * These methods are equivalent to those of the same names and sigs in SourceTypeBinding.
 */
public interface ITypeFinder {

	ReferenceBinding getMemberType(char[] typeName);
	ReferenceBinding[] getMemberTypes();

}
