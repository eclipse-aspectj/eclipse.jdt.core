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

import org.eclipse.jdt.internal.compiler.lookup.MethodBinding;
//import org.eclipse.jdt.internal.compiler.lookup.ParameterizedMethodBinding;
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding;

/**
 * Many routines in the JDT compiler use methodBinding.declaringClass to get
 * the class that owns a method in order to perform checks etc.
 * For inter-type declared methods, the declaringClass will be the aspect,
 * and the owningClass will be the onType of the ITD. We need to ensure that
 * JDT always sees the appropriate type.
 */
public aspect OwningClassSupportForMethodBindings {

	/**
	 * This method is overriden by InterTypeMethodBinding to return the onType instead
	 */
	public ReferenceBinding MethodBinding.getOwningClass() {
		return declaringClass;
	}
	
//	/**
//	 * ParameterizedMethodBindings are backed by the "original" method
//	 * they parameterize. The real owning class is the one owned by that. 
//	 */
//	public ReferenceBinding ParameterizedMethodBinding.getOwningClass() {
//		if (this.declaringClass == original().declaringClass) {
//			// the declaring class is unchanged across this method binding and
//			// its backing ("original") method binding, therefore it is safe
//			// to use the owningClass() of the original
//			return original().getOwningClass();
//		} else {
//			// the declaring class has been changed across this method binding
//			// and its original, so we mustn't go back to the original method
//			// for the answer, just use whatever we've got.
//			// This situation can happen if e.g. a ParameterizedGenericMethodBinding
//			// has as its original method a ParameterizedMethodBinding
//			return declaringClass;
//		}
//     }
	
   /**
    * This aspect handles the switch from declaringClass to owningClass()
    */
   declare warning : call(* MethodBinding.getOwningClass())  
                       && !within(OwningClassSupportForMethodBindings)
                     : "owningClass() support is handled by OwningClassSupportForMethodBindings aspect"; //$NON-NLS-1$
                       
   pointcut accessingDeclaringTypeOfAMethodBinding(MethodBinding aBinding) :
	   get(* MethodBinding.declaringClass) && target(aBinding);
   
   pointcut redirectedDeclaringClassAccesses(MethodBinding aBinding) :
	   accessingDeclaringTypeOfAMethodBinding(aBinding) && 
	   !within(OwningClassSupportForMethodBindings) &&
	   !withincode(* MethodBinding.canBeSeenBy(..)) && // must be based on aspect type here
	   !withincode(MethodBinding.new(..));             // allow binding to initialise properly
   
   Object around(MethodBinding aBinding) : redirectedDeclaringClassAccesses(aBinding) { 
	   return aBinding.getOwningClass();
   }
}
