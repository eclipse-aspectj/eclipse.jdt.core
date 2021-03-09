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

import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding;
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding;
//import org.eclipse.jdt.internal.compiler.lookup.ParameterizedFieldBinding;

/**
 * Many routines in the JDT compiler use fieldBinding.declaringClass to get
 * the class that owns a field in order to perform checks etc.
 * For inter-type declared fields, the declaringClass will be the aspect,
 * and the owningClass will be the onType of the ITD. We need to ensure that
 * JDT always sees the appropriate type.
 */
public aspect OwningClassSupportForFieldBindings {

	/**
	 * This method is overriden by InterTypeFieldBinding to return the onType instead
	 */
	public ReferenceBinding FieldBinding.getOwningClass() {
		return declaringClass; 
	}
	
	/**
	 * ParameterizedFieldBindings are backed by the "original" method
	 * they parameterize. The real owning class is the one owned by that. 
	 */
//	public ReferenceBinding ParameterizedFieldBinding.getOwningClass() {
//		return original().getOwningClass();
//   }
	
   /**
    * This aspect handles the switch from declaringClass to owningClass()
    */
   declare warning : call(* FieldBinding.getOwningClass())  
                       && !within(OwningClassSupportForFieldBindings)
                     : "owningClass() support is handled by OwningClassSupportForFieldBindings aspect"; //$NON-NLS-1$
                       
   pointcut accessingDeclaringTypeOfAFieldBinding(FieldBinding aBinding) :
	   get(* FieldBinding.declaringClass) && target(aBinding);
   
   pointcut redirectedDeclaringClassAccesses(FieldBinding aBinding) :
	   accessingDeclaringTypeOfAFieldBinding(aBinding) && 
	   !within(OwningClassSupportForFieldBindings) &&
	   !withincode(* FieldBinding.canBeSeenBy(..)) && // must be based on aspect type here
	   !withincode(FieldBinding.new(..));             // allow binding to initialise properly
   
   Object around(FieldBinding aBinding) : redirectedDeclaringClassAccesses(aBinding) { 
	   return aBinding.getOwningClass();
   }
}
