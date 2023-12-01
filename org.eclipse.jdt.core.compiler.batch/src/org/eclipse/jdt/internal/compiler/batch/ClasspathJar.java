// AspectJ Extension
/*******************************************************************************
 * Copyright (c) 2000, 2018 IBM Corporation and others.
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
 *     Stephan Herrmann - Contribution for
 *								Bug 440477 - [null] Infrastructure for feeding external annotations into compilation
 *								Bug 440687 - [compiler][batch][null] improve command line option for external annotations
 *******************************************************************************/
package org.eclipse.jdt.internal.compiler.batch;

// this file has a number of changes made by ASC to prevent us from
// leaking OS resources by keeping jars open longer than needed.

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.internal.compiler.batch.FileSystem.Classpath;
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileReader;
import org.eclipse.jdt.internal.compiler.classfmt.ClassFormatException;
import org.eclipse.jdt.internal.compiler.classfmt.ExternalAnnotationDecorator;
import org.eclipse.jdt.internal.compiler.classfmt.ExternalAnnotationProvider;
import org.eclipse.jdt.internal.compiler.env.AccessRuleSet;
import org.eclipse.jdt.internal.compiler.env.IModule;
import org.eclipse.jdt.internal.compiler.env.NameEnvironmentAnswer;
import org.eclipse.jdt.internal.compiler.env.IBinaryType;
import org.eclipse.jdt.internal.compiler.lookup.TypeConstants;
import org.eclipse.jdt.internal.compiler.lookup.BinaryTypeBinding.ExternalAnnotationStatus;
import org.eclipse.jdt.internal.compiler.util.ManifestAnalyzer;
import org.eclipse.jdt.internal.compiler.util.SuffixConstants;
import org.eclipse.jdt.internal.compiler.util.Util;

@SuppressWarnings({"rawtypes", "unchecked"})
public class ClasspathJar extends ClasspathLocation {

	// AspectJ Extension
	/**
	 * ASC 23112004
	 * These fields and related logic throughout the class enable us to cope with very long
	 * classpaths.  Normally all the archives on the classpath were openede and left open
	 * for the duration of a compile - this doesn't scale since you will start running out
	 * of file handles when you have extremely long classpaths (e.g. 4000 jars).  These
	 * fields enable us to keep track of how many are currently open and if you attempt to
	 * open more than some defined limit, it will close some that you haven't used for a
	 * while before opening the new one.  The limit is tailorable through the
	 *   org.aspectj.weaver.openarchives
	 * system property, the default is 1000 which means most users will never exercise
	 * this logic.  The only change outside of this class related to this feature is that
	 * the FileSystem class now constructs a ClasspathJar object by passing in a File
	 * rather than a ZipFile - it is then the responsibility of this class to
	 * open and manage the ZipFile.
	 */
	private static int maxOpenArchives = 1000;
	private static final int MAXOPEN_DEFAULT = 1000;
	private static final List<ClasspathJar> openArchives = new ArrayList<>();
	// End AspectJ Extension

protected File file;
protected ZipFile zipFile;
protected ZipFile annotationZipFile;
protected boolean closeZipFileAtEnd;
protected Set<String> packageCache;
protected List<String> annotationPaths;

// AspectJ Extension
static {
	String openarchivesString = getSystemPropertyWithoutSecurityException("org.aspectj.weaver.openarchives",Integer.toString(MAXOPEN_DEFAULT));
	maxOpenArchives=Integer.parseInt(openarchivesString);
	if (maxOpenArchives<20) maxOpenArchives=1000;
}
// End AspectJ Extension

public ClasspathJar(File file, boolean closeZipFileAtEnd,
		AccessRuleSet accessRuleSet, String destinationPath) {
	super(accessRuleSet, destinationPath);
	this.file = file;
	this.closeZipFileAtEnd = closeZipFileAtEnd;
}

@Override
public List<Classpath> fetchLinkedJars(FileSystem.ClasspathSectionProblemReporter problemReporter) {
	// expected to be called once only - if multiple calls desired, consider
	// using a cache
	try {
		initialize();
		ArrayList<Classpath> result = new ArrayList<>();
		// AspectJ Extension
		try {
			ensureOpen();
		}
		catch (IOException e) {
			throw new RuntimeException(e);
		}
		// End AspectJ Extension
		ZipEntry manifest = this.zipFile.getEntry(TypeConstants.META_INF_MANIFEST_MF);
		if (manifest != null) { // non-null implies regular file
			ManifestAnalyzer analyzer = new ManifestAnalyzer();
			boolean success;
			try (InputStream inputStream = this.zipFile.getInputStream(manifest)) {
				success = analyzer.analyzeManifestContents(inputStream);
			}
			List calledFileNames = analyzer.getCalledFileNames();
			if (problemReporter != null) {
				if (!success || analyzer.getClasspathSectionsCount() == 1 &&  calledFileNames == null) {
					problemReporter.invalidClasspathSection(getPath());
				} else if (analyzer.getClasspathSectionsCount() > 1) {
					problemReporter.multipleClasspathSections(getPath());
				}
			}
			if (calledFileNames != null) {
				Iterator calledFilesIterator = calledFileNames.iterator();
				String directoryPath = getPath();
				int lastSeparator = directoryPath.lastIndexOf(File.separatorChar);
				directoryPath = directoryPath.substring(0, lastSeparator + 1); // potentially empty (see bug 214731)
				while (calledFilesIterator.hasNext()) {
					File linkedFile = new File(directoryPath + (String) calledFilesIterator.next());
					if (linkedFile.isFile()) {
						result.add(new ClasspathJar(linkedFile, this.closeZipFileAtEnd, this.accessRuleSet, this.destinationPath));
					}
				}
			}
		}
		return result;
	} catch (IOException | IllegalArgumentException e) {
		// JRE 9 could throw an IAE if the path is incorrect. We are to ignore such
		// linked jars
		return null;
	}
}
@Override
public NameEnvironmentAnswer findClass(char[] typeName, String qualifiedPackageName, String moduleName, String qualifiedBinaryFileName) {
	// AspectJ Extension
	try {
		ensureOpen();
	}
	catch (IOException e) {
		throw new RuntimeException(e);
	}
	// End AspectJ Extension
	return findClass(typeName, qualifiedPackageName, moduleName, qualifiedBinaryFileName, false);
}
@Override
public NameEnvironmentAnswer findClass(char[] typeName, String qualifiedPackageName, String moduleName, String qualifiedBinaryFileName, boolean asBinaryOnly) {
	if (!isPackage(qualifiedPackageName, moduleName))
		return null; // most common case

	// AspectJ Extension
	try {
		ensureOpen();
	}
	catch (IOException e) {
		throw new RuntimeException(e);
	}
	// End AspectJ Extension
	try {
		IBinaryType reader = ClassFileReader.read(this.zipFile, qualifiedBinaryFileName);
		if (reader != null) {
			char[] modName = this.module == null ? null : this.module.name();
			if (reader instanceof ClassFileReader) {
				ClassFileReader classReader = (ClassFileReader) reader;
				if (classReader.moduleName == null)
					classReader.moduleName = modName;
				else
					modName = classReader.moduleName;
			}
			searchPaths:
			if (this.annotationPaths != null) {
				String qualifiedClassName = qualifiedBinaryFileName.substring(0, qualifiedBinaryFileName.length()-SuffixConstants.EXTENSION_CLASS.length()-1);
				for (String annotationPath : this.annotationPaths) {
					try {
						if (this.annotationZipFile == null) {
							this.annotationZipFile = ExternalAnnotationDecorator.getAnnotationZipFile(annotationPath, null);
						}
						reader = ExternalAnnotationDecorator.create(reader, annotationPath, qualifiedClassName, this.annotationZipFile);

						if (reader.getExternalAnnotationStatus() == ExternalAnnotationStatus.TYPE_IS_ANNOTATED) {
							break searchPaths;
						}
					} catch (IOException e) {
						// don't let error on annotations fail class reading
					}
				}
				// location is configured for external annotations, but no .eea found, decorate in order to answer NO_EEA_FILE:
				reader = new ExternalAnnotationDecorator(reader, null);
			}
			return new NameEnvironmentAnswer(reader, fetchAccessRestriction(qualifiedBinaryFileName), modName);
		}
	} catch (ClassFormatException | IOException e) {
		// treat as if class file is missing
	}
	return null;
}
@Override
public boolean hasAnnotationFileFor(String qualifiedTypeName) {
	if (this.zipFile == null)
		return false;
	// AspectJ Extension
	try {
		ensureOpen();
	}
	catch (IOException e) {
		throw new RuntimeException(e);
	}
	// End AspectJ Extension
	return this.zipFile.getEntry(qualifiedTypeName+ExternalAnnotationProvider.ANNOTATION_FILE_SUFFIX) != null;
}
@Override
public char[][][] findTypeNames(final String qualifiedPackageName, String moduleName) {
	if (!isPackage(qualifiedPackageName, moduleName))
		return null; // most common case
	final char[] packageArray = qualifiedPackageName.toCharArray();
	final ArrayList answers = new ArrayList();
	// AspectJ Extension
	try {
		ensureOpen();
	}
	catch (IOException e) {
		throw new RuntimeException(e);
	}
	// End AspectJ Extension
	nextEntry : for (Enumeration e = this.zipFile.entries(); e.hasMoreElements(); ) {
		String fileName = ((ZipEntry) e.nextElement()).getName();

		// add the package name & all of its parent packages
		int last = fileName.lastIndexOf('/');
		if (last > 0) {
			// extract the package name
			String packageName = fileName.substring(0, last);
			if (!qualifiedPackageName.equals(packageName))
				continue nextEntry;
			int indexOfDot = fileName.lastIndexOf('.');
			if (indexOfDot != -1) {
				String typeName = fileName.substring(last + 1, indexOfDot);
				answers.add(
					CharOperation.arrayConcat(
						CharOperation.splitOn('/', packageArray),
						typeName.toCharArray()));
			}
		}
	}
	int size = answers.size();
	if (size != 0) {
		char[][][] result = new char[size][][];
		answers.toArray(result);
		return result;
	}
	return null;
}

@Override
public void initialize() throws IOException {
	ensureOpen();
	// AspectJ - start - replace next bit with above
	if (this.zipFile == null) {
		this.zipFile = new ZipFile(this.file);
	}
	// AspectJ - end
}
void acceptModule(ClassFileReader reader) {
	if (reader != null) {
		acceptModule(reader.getModuleDeclaration());
	}
}
void acceptModule(byte[] content) {
	if (content == null)
		return;
	ClassFileReader reader = null;
	try {
		reader = new ClassFileReader(content, IModule.MODULE_INFO_CLASS.toCharArray());
	} catch (ClassFormatException e) {
		e.printStackTrace();
	}
	if (reader != null && reader.getModuleDeclaration() != null) {
		acceptModule(reader);
	}
}
protected void addToPackageCache(String fileName, boolean endsWithSep) {
	int last = endsWithSep ? fileName.length() : fileName.lastIndexOf('/');
	while (last > 0) {
		// extract the package name
		String packageName = fileName.substring(0, last);
		if (this.packageCache.contains(packageName))
			return;
		this.packageCache.add(packageName);
		last = packageName.lastIndexOf('/');
	}
}
@Override
public synchronized char[][] getModulesDeclaringPackage(String qualifiedPackageName, String moduleName) {
	if (this.packageCache != null)
		return singletonModuleNameIf(this.packageCache.contains(qualifiedPackageName));
	// AspectJ Extension
	try {
		ensureOpen();
	}
	catch (IOException e) {
		throw new RuntimeException(e);
	}
	// End AspectJ Extension
	this.packageCache = new HashSet<>(41);
	this.packageCache.add(Util.EMPTY_STRING);

	for (Enumeration e = this.zipFile.entries(); e.hasMoreElements(); ) {
		String fileName = ((ZipEntry) e.nextElement()).getName();
		addToPackageCache(fileName, false);
	}
	return singletonModuleNameIf(this.packageCache.contains(qualifiedPackageName));
}
@Override
public boolean hasCompilationUnit(String qualifiedPackageName, String moduleName) {
	qualifiedPackageName += '/';
	// AspectJ Extension
	try {
		ensureOpen();
	}
	catch (IOException e) {
		throw new RuntimeException(e);
	}
	// End AspectJ Extension
	for (Enumeration<? extends ZipEntry> e = this.zipFile.entries(); e.hasMoreElements(); ) {
		String fileName = e.nextElement().getName();
		if (fileName.startsWith(qualifiedPackageName) && fileName.length() > qualifiedPackageName.length()) {
			String tail = fileName.substring(qualifiedPackageName.length());
			if (tail.indexOf('/') != -1)
				continue;
			if (tail.toLowerCase().endsWith(SUFFIX_STRING_class))
				return true;
		}
	}
	return false;
}

@Override
public char[][] listPackages() {
	Set<String> packageNames = new HashSet<>();
	// AspectJ Extension
	try {
		ensureOpen();
	}
	catch (IOException e) {
		throw new RuntimeException(e);
	}
	// End AspectJ Extension
	for (Enumeration<? extends ZipEntry> e = this.zipFile.entries(); e.hasMoreElements(); ) {
		String fileName = e.nextElement().getName();
		int lastSlash = fileName.lastIndexOf('/');
		if (lastSlash != -1 && fileName.toLowerCase().endsWith(SUFFIX_STRING_class))
			packageNames.add(fileName.substring(0, lastSlash).replace('/', '.'));
	}
	return packageNames.stream().map(String::toCharArray).toArray(char[][]::new);
}

@Override
public void reset() {
	super.reset();
	if (this.closeZipFileAtEnd) {
		if (this.zipFile != null) {
		// AspectJ Extension
		// old code:
		//try {
		//	this.zipFile.close(); // AspectJ Extension - dont do this
		//} catch(IOException e) {
		//	// ignore
		//}
		//this.zipFile = null;
		// new code:
		close();
		}
		// End AspectJ Extension
		if (this.annotationZipFile != null) {
			try {
				this.annotationZipFile.close();
			} catch(IOException e) {
				// ignore
			}
			this.annotationZipFile = null;
		}
	}
	this.packageCache = null;
	this.annotationPaths = null;
}
@Override
public String toString() {
	return "Classpath for jar file " + this.file.getPath(); //$NON-NLS-1$
}
@Override
public char[] normalizedPath() {
	if (this.normalizedPath == null) {
		String path2 = this.getPath();
		char[] rawName = path2.toCharArray();
		if (File.separatorChar == '\\') {
			CharOperation.replace(rawName, '\\', '/');
		}
		this.normalizedPath = CharOperation.subarray(rawName, 0, CharOperation.lastIndexOf('.', rawName));
	}
	return this.normalizedPath;
}
@Override
public String getPath() {
	if (this.path == null) {
		try {
			this.path = this.file.getCanonicalPath();
		} catch (IOException e) {
			// in case of error, simply return the absolute path
			this.path = this.file.getAbsolutePath();
		}
	}
	return this.path;
}
@Override
public int getMode() {
	return BINARY;
}

@Override
public IModule getModule() {
	return this.module;
}

// AspectJ Extension
protected void ensureOpen() throws IOException {
	if (zipFile != null) return; // If it is not null, the zip is already open
	final int openArchivesSize = openArchives.size();
	if (openArchivesSize >= maxOpenArchives) {
		closeSomeArchives(openArchivesSize / 10); // Close 10% of those open
	}
	zipFile = new ZipFile(file);
	openArchives.add(this);
}

private void closeSomeArchives(int n) {
	for (int i = 0; i < Math.min(n, openArchives.size()); i++) {
		try {
			ClasspathJar openArchive = openArchives.get(0);
			// FIXME:
			//   It is unclear how null entries can get into the list, because they are added via 'openArchives.add(this)',
			//   but null values are certainly permitted in an ArrayList. Because the error
			//     java.lang.NullPointerException: Cannot invoke
			//       "org.aspectj.org.eclipse.jdt.internal.compiler.batch.ClasspathJar.close()" because the return value of
			//       "java.util.List.get(int)" is null
			//   definitely occurred while compiling AJDT in IntelliJ IDEA, it seems that somehow we need this safeguard.
			if (openArchive != null)
				openArchive.close();
		}
		// Just in case 'openArchive' is changed concurrently
		catch (IndexOutOfBoundsException ignored) { }
	}
}

public void close() {
	if (zipFile == null) return;
	try {
		openArchives.remove(this);
		zipFile.close();
	} catch (IOException ioe) {
		ioe.printStackTrace();
	} finally {
		zipFile = null;
	}
}

// Copes with the security manager
private static String getSystemPropertyWithoutSecurityException (String aPropertyName, String aDefaultValue) {
	try {
		return System.getProperty(aPropertyName, aDefaultValue);
	} catch (SecurityException ex) {
		return aDefaultValue;
	}
}
// End AspectJ Extension

}
