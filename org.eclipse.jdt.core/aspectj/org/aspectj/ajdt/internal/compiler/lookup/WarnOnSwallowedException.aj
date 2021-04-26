/********************************************************************
 * Copyright (c) 2006 Contributors.
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

import org.eclipse.jdt.internal.compiler.ast.Block;
import org.eclipse.jdt.internal.compiler.ast.TryStatement;
import org.eclipse.jdt.internal.compiler.lookup.BlockScope;

/**
 * We get asked for this capability so often, I thought I would simply
 * extend the compiler to support it.
 * 
 * @author Adrian
 * @since 1.5.1
 */
public aspect WarnOnSwallowedException {

	pointcut resolvingATryStatement(TryStatement tryStatement, BlockScope inScope)		
		: execution(* TryStatement.resolve(..)) &&
		  this(tryStatement) &&
		  args(inScope,..);
	
	after(TryStatement tryStatement, BlockScope inScope) returning 
		: resolvingATryStatement(tryStatement,inScope) {
		if (tryStatement.catchBlocks != null) {
			for (int i = 0; i < tryStatement.catchBlocks.length; i++) {
				Block catchBlock = tryStatement.catchBlocks[i];
				if (catchBlock.isEmptyBlock() || catchBlock.statements.length == 0) {
					warnOnEmptyCatchBlock(catchBlock,inScope);
				}
			}
		}
	}
		
	private void warnOnEmptyCatchBlock(Block catchBlock, BlockScope inScope) {
		inScope.problemReporter().swallowedException(catchBlock.sourceStart(),catchBlock.sourceEnd());
	}
}
