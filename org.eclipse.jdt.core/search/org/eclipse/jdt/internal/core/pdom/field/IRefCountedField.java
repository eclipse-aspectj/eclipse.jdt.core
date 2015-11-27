/*******************************************************************************
 * Copyright (c) 2015 Google, Inc and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Stefan Xenos (Google) - Initial implementation
 *******************************************************************************/
package org.eclipse.jdt.internal.core.pdom.field;

import org.eclipse.jdt.internal.core.pdom.PDOM;

/**
 * @since 3.12
 */
public interface IRefCountedField {
	/**
	 * Returns true if this field knows of any remaining incoming references to this object. This is
	 * used by the implementation of {@link FieldManyToOne} to determine whether or not
	 * a refcounted object should be deleted after a reference is removed.
	 * <p>
	 * Implementations should return false if the refcount is 0 or true if the refcount
	 * is nonzero.
	 */	
	public boolean hasReferences(PDOM pdom, long record);
}