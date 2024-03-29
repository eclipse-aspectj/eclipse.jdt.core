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
package org.eclipse.jdt.internal.compiler.util;

import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.internal.compiler.lookup.ModuleBinding;

public final class HashtableOfModule {
	// to avoid using Enumerations, walk the individual tables skipping nulls
	public char[] keyTable[];
	public ModuleBinding valueTable[];

	public int elementSize; // number of elements in the table
	int threshold;
	public HashtableOfModule() {
		this(3); // usually not very large
	}
	public HashtableOfModule(int size) {
		this.elementSize = 0;
		this.threshold = size; // size represents the expected number of elements
		int extraRoom = (int) (size * 1.75f);
		if (this.threshold == extraRoom)
			extraRoom++;
		this.keyTable = new char[extraRoom][];
		this.valueTable = new ModuleBinding[extraRoom];
	}
	public boolean containsKey(char[] key) {
		int length = this.keyTable.length,
				index = CharOperation.hashCode(key) % length;
		int keyLength = key.length;
		char[] currentKey;
		while ((currentKey = this.keyTable[index]) != null) {
			if (currentKey.length == keyLength && CharOperation.equals(currentKey, key))
				return true;
			if (++index == length) {
				index = 0;
			}
		}
		return false;
	}
	public ModuleBinding get(char[] key) {
		int length = this.keyTable.length,
				index = CharOperation.hashCode(key) % length;
		int keyLength = key.length;
		char[] currentKey;
		while ((currentKey = this.keyTable[index]) != null) {
			if (currentKey.length == keyLength && CharOperation.equals(currentKey, key))
				return this.valueTable[index];
			if (++index == length) {
				index = 0;
			}
		}
		return null;
	}
	public ModuleBinding put(char[] key, ModuleBinding value) {
		int length = this.keyTable.length,
				index = CharOperation.hashCode(key) % length;
		int keyLength = key.length;
		char[] currentKey;
		while ((currentKey = this.keyTable[index]) != null) {
			if (currentKey.length == keyLength && CharOperation.equals(currentKey, key))
				return this.valueTable[index] = value;
			if (++index == length) {
				index = 0;
			}
		}
		this.keyTable[index] = key;
		this.valueTable[index] = value;

		// assumes the threshold is never equal to the size of the table
		if (++this.elementSize > this.threshold)
			rehash();
		return value;
	}
	private void rehash() {
		HashtableOfModule newHashtable = new HashtableOfModule(this.elementSize * 2); // double the number of expected elements
		char[] currentKey;
		for (int i = this.keyTable.length; --i >= 0;)
			if ((currentKey = this.keyTable[i]) != null)
				newHashtable.put(currentKey, this.valueTable[i]);

		this.keyTable = newHashtable.keyTable;
		this.valueTable = newHashtable.valueTable;
		this.threshold = newHashtable.threshold;
	}
	public int size() {
		return this.elementSize;
	}
	@Override
	public String toString() {
		String s = ""; //$NON-NLS-1$
		ModuleBinding pkg;
		for (ModuleBinding binding : this.valueTable)
			if ((pkg = binding) != null)
				s += pkg.toString() + "\n"; //$NON-NLS-1$
		return s;
	}
}
