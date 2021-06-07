/* *******************************************************************
 * Copyright (c) 2005 Contributors.
 * All rights reserved.
 * This program and the accompanying materials are made available
 * under the terms of the Eclipse Public License v 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/org/documents/epl-2.0/EPL-2.0.txt
 *
 * Contributors:
 *   Adrian Colyer			Initial implementation
 * ******************************************************************/
package org.aspectj.ajdt.internal.compiler;

import org.eclipse.jdt.internal.compiler.Compiler;
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration;
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit;
import org.eclipse.jdt.internal.compiler.problem.AbortCompilation;

import java.util.Stack;

/**
 * This aspect implements the necessary hooks around the JDT compiler to allow AspectJ to do its
 * job
 */
public privileged aspect CompilerAdapter {

	/**
	 * A default adapter factory for circumstances when this code is used outside of AspectJ
	 * Seems overkill?? Will that ever happen???
	 */
	private static ICompilerAdapterFactory adapterFactory =
		new ICompilerAdapterFactory() {
				@Override
				public ICompilerAdapter getAdapter(Compiler forCompiler) {
					return new DefaultCompilerAdapter(forCompiler);
				}
		};

	/**
	 * Called by AspectJ to inform the JDT of the AspectJ compiler adapter factor to use.
	 */
	public static void setCompilerAdapterFactory(ICompilerAdapterFactory factory) {
		adapterFactory = factory;
	}

  /**
   * If annotation processing is enabled method Compiler.compile(..) could be called recursively.
   * So here we introduce stack of adapters instead of single adapter to drive on compilation events.
   */
  private final Stack<ICompilerAdapter> compilerAdapterStack = new Stack<ICompilerAdapter>();

	pointcut dietParsing(Compiler compiler):
		execution(void Compiler.beginToCompile(ICompilationUnit[])) && this(compiler);

	pointcut compiling(Compiler compiler, ICompilationUnit[] sourceUnits) :
		execution(* Compiler.compile(..)) && args(sourceUnits) && this(compiler);

	pointcut processing(CompilationUnitDeclaration unit, int index) :
		execution(* Compiler.process(..)) && args(unit,index);

	pointcut resolving(CompilationUnitDeclaration unit) :
		call(* CompilationUnitDeclaration.resolve(..)) && target(unit) && within(Compiler);

	pointcut analysing(CompilationUnitDeclaration unit) :
		call(* CompilationUnitDeclaration.analyseCode(..)) && target(unit) && within(Compiler);

	pointcut generating(CompilationUnitDeclaration unit) :
		call(* CompilationUnitDeclaration.generateCode(..)) && target(unit) && within(Compiler);

	before(Compiler compiler, ICompilationUnit[] sourceUnits) : compiling(compiler, sourceUnits) {
		final ICompilerAdapter compilerAdapter = adapterFactory.getAdapter(compiler);
		compilerAdapterStack.push(compilerAdapter);
		compilerAdapter.beforeCompiling(sourceUnits);
	}

	after(Compiler compiler) returning : compiling(compiler, ICompilationUnit[]) {
		try {
			final ICompilerAdapter compilerAdapter = compilerAdapterStack.pop();
			compilerAdapter.afterCompiling(compiler.unitsToProcess);
		} catch (AbortCompilation e) {
			compiler.handleInternalException(e, null);
		} catch (Error e) {
			compiler.handleInternalException(e, null, null);
			throw e; // rethrow
		} catch (RuntimeException e) {
			compiler.handleInternalException(e, null, null);
			throw e; // rethrow
		} finally {
			if (compilerAdapterStack.isEmpty())
				compiler.reset();
		}
	}

	before(CompilationUnitDeclaration unit, int index) : processing(unit,index) {
		compilerAdapterStack.peek().beforeProcessing(unit);
	}

	after(Compiler compiler) returning(): dietParsing(compiler){
		compilerAdapterStack.peek().afterDietParsing(compiler.unitsToProcess);
	}

	// We want this to run even in the erroneous case to ensure 'compiled:' gets out...
	after(CompilationUnitDeclaration unit, int index) : processing(unit, index) {
		compilerAdapterStack.peek().afterProcessing(unit,index);
	}

	before(CompilationUnitDeclaration unit) : resolving(unit) {
		compilerAdapterStack.peek().beforeResolving(unit);
	}

	after(CompilationUnitDeclaration unit) returning : resolving(unit) {
		compilerAdapterStack.peek().afterResolving(unit);
	}

	before(CompilationUnitDeclaration unit) : analysing(unit) {
		compilerAdapterStack.peek().beforeAnalysing(unit);
	}

	after(CompilationUnitDeclaration unit) returning : analysing(unit) {
		compilerAdapterStack.peek().afterAnalysing(unit);
	}

	before(CompilationUnitDeclaration unit) : generating(unit) {
		compilerAdapterStack.peek().beforeGenerating(unit);
	}

	after(CompilationUnitDeclaration unit) returning : generating(unit) {
		compilerAdapterStack.peek().afterGenerating(unit);
	}
}
