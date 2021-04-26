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
 * Anonymous classes are not discovered until the block in which they are
 * declared is resolved. That's after we normally add types into the weaver's
 * type map, so we need to register anonymous types as we discover them.
 */
public aspect AnonymousClassPublisher {

	private AnonymousClassCreationListener listener;  // philosophically there should be a list of
	                                                   // these, but pragmatically we know there is
	                                                   // only one.
	
	public void setAnonymousClassCreationListener(AnonymousClassCreationListener aListener) {
		this.listener = aListener;
	}
	
	pointcut establishingSignatureOfLocalType(LocalTypeBinding aBinding) : 
		execution(* LocalTypeBinding.setConstantPoolName(..)) && this(aBinding);
	
	after(LocalTypeBinding aBinding) returning : 
		establishingSignatureOfLocalType(aBinding) {
		if (listener != null) listener.anonymousTypeBindingCreated(aBinding);		
	}
	
}
