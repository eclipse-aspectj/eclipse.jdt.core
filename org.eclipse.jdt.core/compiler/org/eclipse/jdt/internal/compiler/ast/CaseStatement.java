/*******************************************************************************
 * Copyright (c) 2000, 2021 IBM Corporation and others.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * This is an implementation of an early-draft specification developed under the Java
 * Community Process (JCP) and is made available for testing and evaluation purposes
 * only. The code is not compatible with any specification of the JCP.
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.jdt.internal.compiler.ast;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jdt.internal.compiler.ASTVisitor;
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileConstants;
import org.eclipse.jdt.internal.compiler.codegen.BranchLabel;
import org.eclipse.jdt.internal.compiler.codegen.CodeStream;
import org.eclipse.jdt.internal.compiler.flow.FlowContext;
import org.eclipse.jdt.internal.compiler.flow.FlowInfo;
import org.eclipse.jdt.internal.compiler.impl.Constant;
import org.eclipse.jdt.internal.compiler.impl.IntConstant;
import org.eclipse.jdt.internal.compiler.impl.JavaFeature;
import org.eclipse.jdt.internal.compiler.lookup.Binding;
import org.eclipse.jdt.internal.compiler.lookup.BlockScope;
import org.eclipse.jdt.internal.compiler.lookup.ExtraCompilerModifiers;
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding;
import org.eclipse.jdt.internal.compiler.lookup.LocalVariableBinding;
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding;
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding;

public class CaseStatement extends Statement {

	static final int CASE_CONSTANT = 1;
	static final int CASE_PATTERN  = 2;

	public BranchLabel targetLabel;
	public Expression[] constantExpressions; // case with multiple expressions
	public BranchLabel[] targetLabels; // for multiple expressions
	public boolean isExpr = false;
	private int caseLabelElements = 0; // TODO: Think of moving it up to switch statement for mem optimization
	/* package */ int patternIndex = -1; // points to first pattern var index [only one pattern variable allowed now - should be 0]

public CaseStatement(Expression constantExpression, int sourceEnd, int sourceStart) {
	this(sourceEnd, sourceStart, constantExpression != null ? new Expression[] {constantExpression} : null);
}

public CaseStatement(int sourceEnd, int sourceStart, Expression[] constantExpressions) {
	this.constantExpressions = constantExpressions;
	this.sourceEnd = sourceEnd;
	this.sourceStart = sourceStart;
	initPatterns();
}

private void initPatterns() {
	int l = this.constantExpressions == null ? 0 : this.constantExpressions.length;
	for (int i = 0; i < l; ++i) {
		Expression e = this.constantExpressions[i];
		if (e instanceof Pattern) {
			this.patternIndex = i;
			break;
		}
	}
}

@Override
public FlowInfo analyseCode(
	BlockScope currentScope,
	FlowContext flowContext,
	FlowInfo flowInfo) {
	if (this.constantExpressions != null) {
		for (Expression e : this.constantExpressions) {
			analyseConstantExpression(currentScope, flowContext, flowInfo, e);
		}
	}
	return flowInfo;
}
private void analyseConstantExpression(
		BlockScope currentScope,
		FlowContext flowContext,
		FlowInfo flowInfo,
		Expression e) {
	if (e.constant == Constant.NotAConstant
			&& !e.resolvedType.isEnum()) {
		boolean caseNullorDefaultAllowed =
				JavaFeature.PATTERN_MATCHING_IN_SWITCH.isSupported(currentScope.compilerOptions())
				&& (e instanceof NullLiteral || e instanceof FakeDefaultLiteral);
		if (!caseNullorDefaultAllowed)
			currentScope.problemReporter().caseExpressionMustBeConstant(e);
	}
	e.analyseCode(currentScope, flowContext, flowInfo);
}

@Override
public boolean containsPatternVariable() {
	return this.patternIndex != -1;
}
@Override
public StringBuffer printStatement(int tab, StringBuffer output) {
	printIndent(tab, output);
	if (this.constantExpressions == null) {
		output.append("default "); //$NON-NLS-1$
		output.append(this.isExpr ? "->" : ":"); //$NON-NLS-1$ //$NON-NLS-2$
	} else {
		output.append("case "); //$NON-NLS-1$
		for (int i = 0, l = this.constantExpressions.length; i < l; ++i) {
			this.constantExpressions[i].printExpression(0, output);
			if (i < l -1) output.append(',');
		}
		output.append(this.isExpr ? " ->" : " :"); //$NON-NLS-1$ //$NON-NLS-2$
	}
	return output;
}

/**
 * Case code generation
 *
 */
@Override
public void generateCode(BlockScope currentScope, CodeStream codeStream) {
	if ((this.bits & ASTNode.IsReachable) == 0) {
		return;
	}
	int pc = codeStream.position;
	if (this.targetLabels != null) {
		for (int i = 0, l = this.targetLabels.length; i < l; ++i) {
			this.targetLabels[i].place();
		}
	}
	if (this.targetLabel != null)
		this.targetLabel.place();
	casePatternExpressionGenerateCode(currentScope, codeStream);
	codeStream.recordPositionsFrom(pc, this.sourceStart);
}

private void casePatternExpressionGenerateCode(BlockScope currentScope, CodeStream codeStream) {
	if (this.patternIndex != -1) {
		LocalVariableBinding local = currentScope.findVariable(SwitchStatement.SecretPatternVariableName, null);
		codeStream.load(local);
		Pattern patternExpression = ((Pattern) this.constantExpressions[this.patternIndex]);
		patternExpression.generateCode(currentScope, codeStream);
	}
}

/**
 * No-op : should use resolveCase(...) instead.
 */
@Override
public void resolve(BlockScope scope) {
	// no-op : should use resolveCase(...) instead.
}

/**
 * Returns the constant intValue or ordinal for enum constants. If constant is NotAConstant, then answers Float.MIN_VALUE
 * see org.eclipse.jdt.internal.compiler.ast.Statement#resolveCase(org.eclipse.jdt.internal.compiler.lookup.BlockScope, org.eclipse.jdt.internal.compiler.lookup.TypeBinding, org.eclipse.jdt.internal.compiler.ast.SwitchStatement)
 */
@Override
public Constant[] resolveCase(BlockScope scope, TypeBinding switchExpressionType, SwitchStatement switchStatement) {
	if (containsPatternVariable()) {
		return resolveWithPatternVariablesInScope(this.patternVarsWhenTrue, scope, switchExpressionType, switchStatement);
	}
	return resolveCasePrivate(scope, switchExpressionType, switchStatement);
}
public Constant[] resolveWithPatternVariablesInScope(LocalVariableBinding[] patternVariablesInScope,
		BlockScope scope,
		TypeBinding switchExpressionType,
		SwitchStatement switchStatement) {
	if (patternVariablesInScope != null) {
		for (LocalVariableBinding binding : patternVariablesInScope) {
			binding.modifiers &= ~ExtraCompilerModifiers.AccPatternVariable;
		}
		Constant[] constants = resolveCasePrivate(scope, switchExpressionType, switchStatement);
		for (LocalVariableBinding binding : patternVariablesInScope) {
			binding.modifiers |= ExtraCompilerModifiers.AccPatternVariable;
		}
		return constants;
	} else {
		return resolveCasePrivate(scope, switchExpressionType, switchStatement);
	}
}
private Expression getFirstValidExpression(BlockScope scope, SwitchStatement switchStatement) {
	assert this.constantExpressions != null;
	Expression ret = null;
	int patternCaseLabelCount = 0;
	int typePatternCount = 0;
	int defaultCaseLabelCount = 0;
	int nullCaseLabelCount = 0;

	boolean patternSwitchAllowed = JavaFeature.PATTERN_MATCHING_IN_SWITCH.isSupported(scope.compilerOptions());
	if (patternSwitchAllowed) {
		for (Expression e : this.constantExpressions) {
			 if (e instanceof FakeDefaultLiteral) {
				 flagDuplicateDefault(scope, switchStatement);
				 if (patternCaseLabelCount > 0) {
					 scope.problemReporter().switchPatternBothPatternAndDefaultCaseLabelsNotAllowed(e);
				 }
				 ++defaultCaseLabelCount;
				 continue;
			}
			if (e instanceof Pattern) {
				if (patternCaseLabelCount++ > 0) {
					scope.problemReporter().switchPatternOnlyOnePatternCaseLabelAllowed(e);
				} else if (defaultCaseLabelCount > 0) {
					scope.problemReporter().switchPatternBothPatternAndDefaultCaseLabelsNotAllowed(e);
				}
				if (e instanceof TypePattern) {
					++typePatternCount;
				} else if (nullCaseLabelCount > 0 ) {
					scope.problemReporter().switchPatternBothNullAndNonTypePatternNotAllowed(e);
				}
			} else if (e instanceof NullLiteral) {
				if (switchStatement.nullCase == null)
					switchStatement.nullCase = this;

				if (nullCaseLabelCount++ > 0) {
					// TODO: Decide whether we need to have a more fine-grain element level error flagging for null specifically
//					continue;
				}
				if ((patternCaseLabelCount - typePatternCount) > 0) {
					scope.problemReporter().switchPatternBothNullAndNonTypePatternNotAllowed(e);
				}
			}
			if (ret == null) ret = e;
		}
	} else {
		for (Expression e : this.constantExpressions) {
			if (e instanceof Pattern
					|| e instanceof NullLiteral
					|| e instanceof FakeDefaultLiteral) {
				scope.problemReporter().validateJavaFeatureSupport(JavaFeature.PATTERN_MATCHING_IN_SWITCH,
						e.sourceStart, e.sourceEnd);
				continue;
			}
			if (ret == null) ret = e;
		}
	}
	return ret;
}
private Constant[] resolveCasePrivate(BlockScope scope, TypeBinding switchExpressionType, SwitchStatement switchStatement) {
	// switchExpressionType maybe null in error case
	scope.enclosingCase = this; // record entering in a switch case block
	if (this.constantExpressions == null) {
		flagDuplicateDefault(scope, switchStatement);
		return Constant.NotAConstantList;
	}
	Expression constExpr = getFirstValidExpression(scope, switchStatement);
	if (constExpr == null) {
		return Constant.NotAConstantList;
	}

	// add into the collection of cases of the associated switch statement
	switchStatement.cases[switchStatement.caseCount++] = this;
	if (switchExpressionType != null && switchExpressionType.isEnum() && (constExpr instanceof SingleNameReference)) {
		((SingleNameReference) constExpr).setActualReceiverType((ReferenceBinding)switchExpressionType);
	}

	TypeBinding caseType = constExpr.resolveType(scope);
	if (caseType == null || switchExpressionType == null) return Constant.NotAConstantList;
	// tag constant name with enum type for privileged access to its members

	List<Constant> cases = new ArrayList<>();
	for (Expression e : this.constantExpressions) {
		if (e != constExpr) {
			if (switchExpressionType.isEnum() && (e instanceof SingleNameReference)) {
				((SingleNameReference) e).setActualReceiverType((ReferenceBinding)switchExpressionType);
			} else if (e instanceof FakeDefaultLiteral) {
				continue; // already processed
			}
			e.resolveType(scope);
		}
		Constant con = resolveConstantExpression(scope, caseType, switchExpressionType, switchStatement, e);
		if (con != Constant.NotAConstant) {
			cases.add(con);
		}
	}
	this.resolveWithPatternVariablesInScope(this.getPatternVariablesWhenTrue(), scope);
	if (cases.size() > 0) {
		return cases.toArray(new Constant[cases.size()]);
	}

	return Constant.NotAConstantList;
}

private void flagDuplicateDefault(BlockScope scope, SwitchStatement switchStatement) {
	// remember the default case into the associated switch statement
	if (switchStatement.defaultCase != null)
		scope.problemReporter().duplicateDefaultCase(this);

	// on error the last default will be the selected one ...
	switchStatement.defaultCase = this;
}
public void collectPatternVariablesToScope(LocalVariableBinding[] variables, BlockScope scope) {
	if (!containsPatternVariable()) {
		return;
	}
	for (Expression e : this.constantExpressions) {
		e.collectPatternVariablesToScope(variables, scope);
		LocalVariableBinding[] patternVariables = e.getPatternVariablesWhenTrue();
		addPatternVariablesWhenTrue(patternVariables);
	}
}
private boolean testForMixedLabelKind(int kind) {
	int kinds = this.caseLabelElements;
	this.caseLabelElements |= kind;
	return kinds != 0 && (kinds & kind) != kind;
}
public Constant resolveConstantExpression(BlockScope scope,
											TypeBinding caseType,
											TypeBinding switchExpressionType,
											SwitchStatement switchStatement,
											Expression expression) {

	boolean patternSwitchAllowed = JavaFeature.PATTERN_MATCHING_IN_SWITCH.isSupported(scope.compilerOptions());
	if (patternSwitchAllowed) {
		if (expression instanceof Pattern) {
			if (testForMixedLabelKind(CASE_PATTERN)) {
				scope.problemReporter().switchPatternConstantWithPatternIncompatible(expression);
			}
			return resolveConstantExpression(scope, caseType, switchExpressionType,
					switchStatement,(Pattern) expression);
		} else if (expression instanceof NullLiteral) {
			if (!(switchExpressionType instanceof ReferenceBinding)) {
				scope.problemReporter().typeMismatchError(TypeBinding.NULL, switchExpressionType, expression, null);
			}
			switchStatement.containsCaseNull = true;
			return IntConstant.fromValue(-1);
		} else if (expression instanceof FakeDefaultLiteral) {
			// do nothing
		} else {
			if (testForMixedLabelKind(CASE_CONSTANT)) {
				scope.problemReporter().switchPatternConstantWithPatternIncompatible(expression);
				return Constant.NotAConstant;
			}
			if (switchStatement.isNonTraditional) {
				if (!expression.isConstantValueOfTypeAssignableToType(caseType, switchExpressionType)) {
					scope.problemReporter().switchPatternConstantCaseLabelIncompatible(expression, switchExpressionType);
					return Constant.NotAConstant;
				}
			}
		}
	}
	boolean boxing = !patternSwitchAllowed ||
			switchStatement.isAllowedType(switchExpressionType);

	if (expression.isConstantValueOfTypeAssignableToType(caseType, switchExpressionType)
			||(caseType.isCompatibleWith(switchExpressionType)
				&& !(expression instanceof StringLiteral))) {
		if (caseType.isEnum()) {
			if (((expression.bits & ASTNode.ParenthesizedMASK) >> ASTNode.ParenthesizedSHIFT) != 0) {
				scope.problemReporter().enumConstantsCannotBeSurroundedByParenthesis(expression);
			}

			if (expression instanceof NameReference
					&& (expression.bits & ASTNode.RestrictiveFlagMASK) == Binding.FIELD) {
				NameReference reference = (NameReference) expression;
				FieldBinding field = reference.fieldBinding();
				if ((field.modifiers & ClassFileConstants.AccEnum) == 0) {
					 scope.problemReporter().enumSwitchCannotTargetField(reference, field);
				} else 	if (reference instanceof QualifiedNameReference) {
					 scope.problemReporter().cannotUseQualifiedEnumConstantInCaseLabel(reference, field);
				}
				return IntConstant.fromValue(field.original().id + 1); // (ordinal value + 1) zero should not be returned see bug 141810
			}
		} else {
			return expression.constant;
		}
	} else if (boxing && isBoxingCompatible(caseType, switchExpressionType, expression, scope)) {
		// constantExpression.computeConversion(scope, caseType, switchExpressionType); - do not report boxing/unboxing conversion
		return expression.constant;
	}
	scope.problemReporter().typeMismatchError(caseType, switchExpressionType, expression, switchStatement.expression);
	return Constant.NotAConstant;
}

private Constant resolveConstantExpression(BlockScope scope,
		TypeBinding caseType,
		TypeBinding switchExpressionType,
		SwitchStatement switchStatement,
		Pattern e) {
	Constant constant = Constant.NotAConstant;
	TypeBinding type = e.resolveType(scope);
	if (type != null) {
		constant = IntConstant.fromValue(switchStatement.caseLabelElements.size());
		switchStatement.caseLabelElements.add(e);
		if (e.resolvedType != null) {
			// 14.30.2 at compile-time we "resolve" the pattern with respect to the (compile-time) type
			// of the expression being pattern matched
			TypeBinding pb = e.resolveAtType(scope, switchStatement.expression.resolvedType);
			if (pb != null) switchStatement.caseLabelElementTypes.add(pb);
		}
	}
	return constant;
}

/* package */ void patternCaseRemovePatternLocals(CodeStream codeStream) {
	for (Expression e : this.constantExpressions) {
		if (e instanceof Pattern) {
			e.traverse(new ASTVisitor() {
				@Override
				public boolean visit(TypePattern typePattern, BlockScope scope) {
					LocalDeclaration local = typePattern.getPatternVariableIntroduced();
					if (local != null && local.binding != null)
						codeStream.removeVariable(local.binding);
					return false; // No deeper than this on this node
				}
			}, (BlockScope) null);
		}
	}
}
@Override
public void traverse(ASTVisitor visitor, 	BlockScope blockScope) {
	if (visitor.visit(this, blockScope)) {
		if (this.constantExpressions != null) {
			for (Expression e : this.constantExpressions) {
				e.traverse(visitor, blockScope);
			}
		}

	}
	visitor.endVisit(this, blockScope);
}
}
