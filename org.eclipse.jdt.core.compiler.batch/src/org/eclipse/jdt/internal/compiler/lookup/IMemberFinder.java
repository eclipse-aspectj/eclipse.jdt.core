/*******************************************************************************
 * Copyright (c) 2002 Palo Alto Research Center, Incorporated (PARC) 
 * and others.
 * All rights reserved.
 * This program and the accompanying materials are made available
 * under the terms of the Eclipse Public License v 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/org/documents/epl-2.0/EPL-2.0.txt
 * 
 * Contributors:
 *     PARC       - initial API and implementation
 ******************************************************************************/

package org.eclipse.jdt.internal.compiler.lookup;
// AspectJ Extension

/**
 * This interface is used by SourceTypeBinding to provide a delegated lookup
 * instance. It is used to support AspectJ's inter-type declarations.
 * 
 * These methods are equivalent to those of the same names and sigs in SourceTypeBinding.
 */
public interface IMemberFinder {

	FieldBinding getField(
                          SourceTypeBinding sourceTypeBinding,
                          char[] fieldName,
                          InvocationSite site,
                          Scope scope);
		
	MethodBinding[] methods(SourceTypeBinding sourceTypeBinding);	
		
	MethodBinding[] getMethods(
                               SourceTypeBinding sourceTypeBinding,
                               char[] methodName);

	MethodBinding getExactMethod(
                                 SourceTypeBinding sourceTypeBinding,
                                 char[] selector,
                                 TypeBinding[] argumentTypes,
                                 CompilationUnitScope refScope);

}
