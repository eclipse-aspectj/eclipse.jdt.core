/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.jdt.internal.compiler.ast;

import org.eclipse.jdt.internal.compiler.ASTVisitor;
import org.eclipse.jdt.internal.compiler.lookup.ArrayBinding;
import org.eclipse.jdt.internal.compiler.lookup.BlockScope;
import org.eclipse.jdt.internal.compiler.lookup.ClassScope;
import org.eclipse.jdt.internal.compiler.lookup.CompilationUnitScope;
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding;
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding;

/**
 * MemberValuePair node
 */
public class MemberValuePair extends ASTNode {
	
	public char[] name;
	public Expression value;
	public MethodBinding binding;
	
	public MemberValuePair(char[] token, int sourceStart, int sourceEnd, Expression value) {
		this.name = token;
		this.sourceStart = sourceStart;
		this.sourceEnd = sourceEnd;
		this.value = value;
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jdt.internal.compiler.ast.ASTNode#print(int, java.lang.StringBuffer)
	 */
	public StringBuffer print(int indent, StringBuffer output) {
		output
			.append(name)
			.append(" = "); //$NON-NLS-1$
		value.print(indent, output);
		return output;
	}
	
	public void resolveTypeExpecting(BlockScope scope, TypeBinding requiredType) {
		
		if (requiredType == null) 
			return;
		if (this.value == null) 
			return;

		this.value.setExpectedType(requiredType); // needed in case of generic method invocation
		if (this.value instanceof ArrayInitializer) {
			ArrayInitializer initializer = (ArrayInitializer) this.value;
			if ((initializer.resolveTypeExpecting(scope, this.binding.returnType)) != null) {
				this.value.resolvedType = initializer.binding = (ArrayBinding) this.binding.returnType;
			}			
		} else {
			this.value.resolveType(scope);
		}
		TypeBinding valueType = this.value.resolvedType;
		if (valueType == null)
			return;

		if (!valueType.isCompatibleWith(requiredType)) {
			if (!(requiredType.isArrayType() && requiredType.dimensions() == 1 && valueType.isCompatibleWith(requiredType.leafComponentType()))) {
				scope.problemReporter().typeMismatchError(valueType, requiredType, this.value);
				return; // may allow to proceed to find more errors at once
			}
		} else {
			scope.compilationUnitScope().recordTypeConversion(requiredType.leafComponentType(), valueType.leafComponentType());
			this.value.computeConversion(scope, requiredType, valueType);				
		}
		
		// annotation methods can only return base types, String, Class, enum type, annotation types and arrays of these
		checkAnnotationMethodType: {
			TypeBinding leafType = requiredType.leafComponentType();
			switch (leafType.erasure().id) {
				case T_byte :
				case T_short :
				case T_char :
				case T_int :
				case T_long :
				case T_float :
				case T_double :
				case T_boolean :
				case T_JavaLangString :
					if (this.value instanceof ArrayInitializer) {
						ArrayInitializer initializer = (ArrayInitializer) this.value;
						final Expression[] expressions = initializer.expressions;
						if (expressions != null) {
							for (int i =0, max = expressions.length; i < max; i++) {
								if (expressions[i].constant == NotAConstant) {
									scope.problemReporter().annotationValueMustBeConstant(this.binding.declaringClass, this.name, expressions[i]);
								}
							}
						}
					} else if (this.value.constant == NotAConstant) {
						scope.problemReporter().annotationValueMustBeConstant(this.binding.declaringClass, this.name, this.value);
					}
					break checkAnnotationMethodType;
				case T_JavaLangClass :
					if (!(this.value instanceof ClassLiteralAccess)) {
						scope.problemReporter().annotationValueMustBeClassLiteral(this.binding.declaringClass, this.name, this.value);
					}
					break checkAnnotationMethodType;
			}
			if (leafType.isEnum()) {
				break checkAnnotationMethodType;
			}
			if (leafType.isAnnotationType()) {
				break checkAnnotationMethodType;
			}
		}
	}
	
	public void traverse(ASTVisitor visitor, BlockScope scope) {
		if (visitor.visit(this, scope)) {
			if (this.value != null) {
				this.value.traverse(visitor, scope);
			}
		}
		visitor.endVisit(this, scope);
	}
	public void traverse(ASTVisitor visitor, ClassScope scope) {
		if (visitor.visit(this, scope)) {
			if (this.value != null) {
				this.value.traverse(visitor, scope);
			}
		}
		visitor.endVisit(this, scope);
	}
	public void traverse(ASTVisitor visitor, CompilationUnitScope scope) {
		if (visitor.visit(this, scope)) {
			if (this.value != null) {
				this.value.traverse(visitor, scope);
			}
		}
		visitor.endVisit(this, scope);
	}
}
