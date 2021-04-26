/*******************************************************************************
 * Copyright (c) 2002 Palo Alto Research Center, Incorporated (PARC) 
 * and others.  All rights reserved. 
 * This program and the accompanying materials are made available under
 * the terms of the Common Public License v1.0 which accompanies this
 * distribution, available at http://www.eclipse.org/legal/cpl-v1.0.html
 * 
 * Contributors:
 *     PARC       - initial API and implementation
 ******************************************************************************/

package org.eclipse.jdt.internal.compiler.lookup; 

//AspectJ Extension

import org.eclipse.jdt.internal.compiler.ast.ASTNode;

/**
 * This interface is used by SourceTypeBinding to provide a delegated lookup instance. It is used to support AspectJ's inter-type
 * declarations.
 * 
 * These methods are equivalent to those of the same names and sigs in SourceTypeBinding.
 */
public interface IPrivilegedHandler {

	FieldBinding getPrivilegedAccessField(FieldBinding baseField, ASTNode location);

	boolean definesPrivilegedAccessToField(FieldBinding field);

	MethodBinding getPrivilegedAccessMethod(MethodBinding baseMethod, ASTNode location);

	void notePrivilegedTypeAccess(ReferenceBinding type, ASTNode location);
}
