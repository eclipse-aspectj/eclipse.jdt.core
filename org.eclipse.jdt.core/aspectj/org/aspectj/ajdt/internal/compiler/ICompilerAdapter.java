// AspectJ Extension
/*******************************************************************************
 * Copyright (c) 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.aspectj.ajdt.internal.compiler;

import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration;
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit;

/**
 * An ICompilerAdapter will be called by the compiler at strategic
 * points during compilation, allowing the adapter to perform additional
 * processing as required.
 */
public interface ICompilerAdapter {
	
	void afterDietParsing(CompilationUnitDeclaration[] units);
	
	void beforeCompiling(ICompilationUnit[] sourceUnits);
	void afterCompiling(CompilationUnitDeclaration[] units);
	
	void beforeProcessing(CompilationUnitDeclaration unit);
	void afterProcessing(CompilationUnitDeclaration unit, int unitIndex);
	
	void beforeResolving(CompilationUnitDeclaration unit);
	void afterResolving(CompilationUnitDeclaration unit);

	void beforeAnalysing(CompilationUnitDeclaration unit);
	void afterAnalysing(CompilationUnitDeclaration unit);
	
	void beforeGenerating(CompilationUnitDeclaration unit);
	void afterGenerating(CompilationUnitDeclaration unit);
	
//	void beforeResolving(CompilationUnitDeclaration unit,
//						 ICompilationUnit sourceUnit,
//						 boolean verifyMethods,
//						 boolean analyzeCode,
//						 boolean generateCode);
//	void afterResolving(CompilationUnitDeclaration unit,
//						ICompilationUnit sourceUnit,
//						boolean verifyMethods,
//						boolean analyzeCode,
//						boolean generateCode);		
}


// End AspectJ Extension