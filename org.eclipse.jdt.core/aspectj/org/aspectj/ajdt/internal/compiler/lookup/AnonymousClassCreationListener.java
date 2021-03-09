/* *******************************************************************
 * Copyright (c) 2005 Contributors.
 * All rights reserved. 
 * This program and the accompanying materials are made available 
 * under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution and is available at 
 * http://eclipse.org/legal/epl-v10.html 
 *  
 * Contributors: 
 *   Adrian Colyer			Initial implementation
 * ******************************************************************/
package org.aspectj.ajdt.internal.compiler.lookup;

import org.eclipse.jdt.internal.compiler.lookup.LocalTypeBinding;

/**
 * @author colyer
 * Callback interface implemented by clients that want to find out about
 * anonymous types discovered during block resolution.
 */
public interface AnonymousClassCreationListener {

	void anonymousTypeBindingCreated(LocalTypeBinding aBinding);

}
