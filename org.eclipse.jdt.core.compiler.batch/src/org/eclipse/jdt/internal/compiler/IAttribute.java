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

package org.eclipse.jdt.internal.compiler;

import org.eclipse.jdt.internal.compiler.codegen.ConstantPool;

// AspectJ Extension
/**
 * Represents an Attribute for a Java .class file.
 */
public interface IAttribute {

	/**
	 * Returns the name of the attribute.
	 */
	char[] getNameChars();

	/**
	 * @param nameIndex the index into this class's constant pool for this attribute's name.
	 * @param constantPool
	 * 
	 * @return all of the bytes to represent this attribute in the .class file.
	 */
	byte[] getAllBytes(short nameIndex, ConstantPool constantPool);

}
