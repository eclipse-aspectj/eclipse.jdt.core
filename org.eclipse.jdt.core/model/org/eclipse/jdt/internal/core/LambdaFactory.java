/*******************************************************************************
 * Copyright (c) 2014 IBM Corporation and others.
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
 *******************************************************************************/

package org.eclipse.jdt.internal.core;

import org.eclipse.jdt.core.ILocalVariable;
import org.eclipse.jdt.core.Signature;
import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.internal.compiler.ast.Argument;
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding;
import org.eclipse.jdt.internal.core.util.DeduplicationUtil;

public class LambdaFactory {

	public static LambdaExpression createLambdaExpression(JavaElement parent, org.eclipse.jdt.internal.compiler.ast.LambdaExpression lambdaExpression) {
		if (isBinaryMember(parent)){
			return new BinaryLambdaExpression(parent, lambdaExpression);
		} else {
			return new LambdaExpression(parent, lambdaExpression);
		}
	}

	public static LambdaExpression createLambdaExpression(JavaElement parent, String interphase, int sourceStart, int sourceEnd, int arrowPosition) {
		if (isBinaryMember(parent)){
			return new BinaryLambdaExpression(parent, interphase, sourceStart, sourceEnd, arrowPosition);
		} else {
			return new LambdaExpression(parent, interphase, sourceStart, sourceEnd, arrowPosition);
		}
	}

	public static LambdaMethod createLambdaMethod(JavaElement parent, org.eclipse.jdt.internal.compiler.ast.LambdaExpression lambdaExpression) {
		int length;
		String [] parameterTypes = new String[length = lambdaExpression.descriptor.parameters.length];
		for (int i = 0; i < length; i++)
			parameterTypes[i] = getTypeSignature(lambdaExpression.descriptor.parameters[i]);
		String [] parameterNames = new String[length];
		for (int i = 0; i < length; i++)
			parameterNames[i] = DeduplicationUtil.toString(lambdaExpression.arguments[i].name);
		String returnType = getTypeSignature(lambdaExpression.descriptor.returnType);
		String selector = DeduplicationUtil.toString(lambdaExpression.descriptor.selector);
		String key = DeduplicationUtil.toString(lambdaExpression.descriptor.computeUniqueKey());
		LambdaMethod lambdaMethod = createLambdaMethod(parent, selector, key, lambdaExpression.sourceStart, lambdaExpression.sourceEnd, lambdaExpression.arrowPosition, parameterTypes, parameterNames, returnType);
		ILocalVariable [] parameters = new ILocalVariable[length = lambdaExpression.arguments.length];
		for (int i = 0; i < length; i++) {
			Argument argument = lambdaExpression.arguments[i];
			String signature = DeduplicationUtil.toString(lambdaExpression.descriptor.parameters[i].signature());
			parameters[i] = new LocalVariable(
					lambdaMethod,
					DeduplicationUtil.toString(argument.name),
					argument.declarationSourceStart,
					argument.declarationSourceEnd,
					argument.sourceStart,
					argument.sourceEnd,
					signature,
					null, // we are not hooking up argument.annotations ATM,
					argument.modifiers,
					true);
		}
		lambdaMethod.elementInfo.arguments = parameters;
		return lambdaMethod;
	}

	public static LambdaMethod createLambdaMethod(JavaElement parent, String selector, String key, int sourceStart, int sourceEnd, int arrowPosition, String [] parameterTypes, String [] parameterNames, String returnType) {
		SourceMethodInfo info = null;
		boolean isBinary = (parent instanceof BinaryLambdaExpression);
		info = new SourceMethodInfo();
		info.setSourceRangeStart(sourceStart);
		info.setSourceRangeEnd(sourceEnd);
		info.setFlags(0);
		info.setNameSourceStart(sourceStart);
		info.setNameSourceEnd(arrowPosition);
		int length;
		char[][] argumentNames = new char[length = parameterNames.length][];
		for (int i = 0; i < length; i++)
			argumentNames[i] = DeduplicationUtil.intern(parameterNames[i].toCharArray());
		info.setArgumentNames(argumentNames);
		info.setReturnType(DeduplicationUtil.intern(Signature.toCharArray(returnType.toCharArray())));
		info.setExceptionTypeNames(CharOperation.NO_CHAR_CHAR);
		info.arguments = null; // will be updated shortly, parent has to come into existence first.

		return isBinary ? new BinaryLambdaMethod(parent, selector, key, sourceStart, parameterTypes, parameterNames, returnType, info) :
				new LambdaMethod(parent, selector, key, sourceStart, parameterTypes, parameterNames, returnType, info);
	}

	private static String getTypeSignature(TypeBinding type) {
		char[] signature = type.genericTypeSignature();
		signature = CharOperation.replaceOnCopy(signature, '/', '.');
		return DeduplicationUtil.toString(signature);
	}

	private static boolean isBinaryMember(JavaElement element) {
		return element instanceof BinaryMember;
	}
}
