/*******************************************************************************
 * Copyright (c) 2021 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
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
package org.eclipse.jdt.core.tests.compiler.regression;

import java.util.Map;

import org.eclipse.jdt.internal.compiler.classfmt.ClassFileConstants;
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions;
import org.eclipse.jdt.internal.compiler.impl.JavaFeature;

import junit.framework.Test;

public class PreviewFeatureTest extends AbstractRegressionTest9 {

	static {
//		TESTS_NUMBERS = new int [] { 40 };
//		TESTS_RANGE = new int[] { 1, -1 };
//		TESTS_NAMES = new String[] { "test001"};
	}

	public static Class<?> testClass() {
		return PreviewFeatureTest.class;
	}
	public static Test suite() {
		return buildMinimalComplianceTestSuite(testClass(), F_16);
	}
	public PreviewFeatureTest(String testName){
		super(testName);
	}

	/*
	 * Preview API, --enable-preview=false, SuppressWarning=No
	 */
	public void _test001() {
		Map<String, String> options = getCompilerOptions();
		String old = options.get(CompilerOptions.OPTION_EnablePreviews);
		options.put(CompilerOptions.OPTION_EnablePreviews, CompilerOptions.DISABLED);
		try {
			runNegativeTest(
					new String[] {
							"X.java",
							"import javax.lang.model.element.Modifier;\n"+
							"public class X {\n"+
									"    Zork z = null;\n" +
									"public Modifier getModifier() {\n"+
									"		return Modifier.SEALED;\n"+
									"	}\n"+
									"	public Class<?>[] getPermittedClasses() {\n"+
									"		return this.getClass().getPermittedSubclasses();\n"+
									"	}\n"+
									"}\n",
					},
					"----------\n" +
					"1. ERROR in X.java (at line 3)\n" +
					"	Zork z = null;\n" +
					"	^^^^\n" +
					"Zork cannot be resolved to a type\n" +
					"----------\n" +
					"2. WARNING in X.java (at line 5)\n" +
					"	return Modifier.SEALED;\n" +
					"	       ^^^^^^^^^^^^^^^\n" +
					"You are using an API that is part of a preview feature and may be removed in future\n" +
					"----------\n" +
					"3. WARNING in X.java (at line 8)\n" +
					"	return this.getClass().getPermittedSubclasses();\n" +
					"	       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n" +
					"You are using an API that is part of a preview feature and may be removed in future\n" +
					"----------\n",
					null,
					true,
					options);
		} finally {
			options.put(CompilerOptions.OPTION_EnablePreviews, old);
		}
	}
	/*
	 * Preview API, --enable-preview=false, SuppressWarning=yes
	 */
	public void _test002() {
		Map<String, String> options = getCompilerOptions();
		String old = options.get(CompilerOptions.OPTION_EnablePreviews);
		options.put(CompilerOptions.OPTION_EnablePreviews, CompilerOptions.DISABLED);
		try {
			runNegativeTest(
					new String[] {
							"X.java",
							"import javax.lang.model.element.Modifier;\n"+
							"@SuppressWarnings(\"preview\")\n"+
							"public class X {\n"+
									"    Zork z = null;\n" +
									"public Modifier getModifier() {\n"+
									"		return Modifier.SEALED;\n"+
									"	}\n"+
									"	public Class<?>[] getPermittedClasses() {\n"+
									"		return this.getClass().getPermittedSubclasses();\n"+
									"	}\n"+
									"}\n",
					},
					"----------\n" +
					"1. ERROR in X.java (at line 4)\n" +
					"	Zork z = null;\n" +
					"	^^^^\n" +
					"Zork cannot be resolved to a type\n" +
					"----------\n",
					null,
					true,
					options);
		} finally {
			options.put(CompilerOptions.OPTION_EnablePreviews, old);
		}
	}
	/*
	 * Preview API, --enable-preview=true, SuppressWarning=No
	 */
	public void test003() {
		Map<String, String> options = getCompilerOptions();
		if (this.complianceLevel < ClassFileConstants.getLatestJDKLevel())
			return;
		String old = options.get(CompilerOptions.OPTION_EnablePreviews);
		options.put(CompilerOptions.OPTION_EnablePreviews, CompilerOptions.ENABLED);
		try {
			runNegativeTest(
					new String[] {
							"X.java",
							"import javax.lang.model.element.Modifier;\n"+
							"public class X {\n"+
									"    Zork z = null;\n" +
									"public Modifier getModifier() {\n"+
									"		return Modifier.SEALED;\n"+
									"	}\n"+
									"	public Class<?>[] getPermittedClasses() {\n"+
									"		return this.getClass().getPermittedSubclasses();\n"+
									"	}\n"+
									"}\n",
					},
					"----------\n" +
					"1. ERROR in X.java (at line 3)\n" +
					"	Zork z = null;\n" +
					"	^^^^\n" +
					"Zork cannot be resolved to a type\n" +
					"----------\n",
					null,
					true,
					options);
		} finally {
			options.put(CompilerOptions.OPTION_EnablePreviews, old);
		}
	}
	/*
	 * Preview API, --enable-preview=true, SuppressWarning=Yes
	 */
	public void test004() {
		if (this.complianceLevel < ClassFileConstants.getLatestJDKLevel())
			return;
		Map<String, String> options = getCompilerOptions();
		String old = options.get(CompilerOptions.OPTION_EnablePreviews);
		options.put(CompilerOptions.OPTION_EnablePreviews, CompilerOptions.ENABLED);
		try {
			runNegativeTest(
					new String[] {
							"X.java",
							"import javax.lang.model.element.Modifier;\n"+
							"@SuppressWarnings(\"preview\")\n"+
							"public class X {\n"+
									"    Zork z = null;\n" +
									"public Modifier getModifier() {\n"+
									"		return Modifier.SEALED;\n"+
									"	}\n"+
									"	public Class<?>[] getPermittedClasses() {\n"+
									"		return this.getClass().getPermittedSubclasses();\n"+
									"	}\n"+
									"}\n",
					},
					"----------\n" +
					"1. ERROR in X.java (at line 4)\n" +
					"	Zork z = null;\n" +
					"	^^^^\n" +
					"Zork cannot be resolved to a type\n" +
					"----------\n",
					null,
					true,
					options);
		} finally {
			options.put(CompilerOptions.OPTION_EnablePreviews, old);
		}
	}
	public void test005() {
		if (this.complianceLevel < ClassFileConstants.JDK16)
			return;
		Map<String, String> options = getCompilerOptions();
		String old = options.get(CompilerOptions.OPTION_EnablePreviews);
		if (this.complianceLevel == ClassFileConstants.getLatestJDKLevel())
			options.put(CompilerOptions.OPTION_EnablePreviews, CompilerOptions.ENABLED);
		try {
			if (this.complianceLevel < ClassFileConstants.getLatestJDKLevel())
				assertFalse(JavaFeature.PATTERN_MATCHING_IN_SWITCH.isSupported(new CompilerOptions(options)));
			else
				assertTrue(JavaFeature.PATTERN_MATCHING_IN_SWITCH.isSupported(new CompilerOptions(options)));
		} finally {
			options.put(CompilerOptions.OPTION_EnablePreviews, old);
		}
	}
}
