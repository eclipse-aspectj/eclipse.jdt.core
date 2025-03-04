# AspectJ JDT Core

## What is it?

This is an AspectJ-enhanced fork of [Eclipse JDT Core](https://github.com/eclipse-jdt/eclipse.jdt.core).
The AspectJ Compiler (AJC) builds upon the Eclipse Java Compiler (ECJ) and its annotation processing (APT) support,
adding native [AspectJ](https://github.com/eclipse-aspectj/aspectj) syntax parsing and compilation to the JDT Core batch
compiler.

The rest of JDT Core is mostly ignored, and the subset of classes necessary for inclusion into AspectJ Core is built
separately by Maven, completely bypassing the upstream build configuration.

## How is it built?

The main build file is [`org.eclipse.jdt.core/pom.xml`](org.eclipse.jdt.core/pom.xml). There, skip the commented-out
original content at the beginning of the file, which is kept around for easier upstream merging. Go right to the section
starting like this:

```xml
  <groupId>org.aspectj</groupId>
  <artifactId>org.eclipse.jdt.core</artifactId>
  <version>...</version>

  <name>JDT Core for AspectJ</name>
  <description>
    AspectJ forks Eclipse Java Development Tools (JDT) Core, utilising the Eclipse Compiler (ECJ) + APT classes as the
    foundation for the AspectJ Compiler (AJC).
  </description>
```

[Contributions are always welcome!](https://github.com/eclipse-jdt/.github/blob/main/CONTRIBUTING.md)

Please bear in mind that this project is almost entirely developed by volunteers.
If you do not provide the implementation yourself (or pay someone to do it for you), the bug might never get fixed.
If it is a serious bug, other people than you might care enough to provide a fix.

[![Create Eclipse Development Environment for JDT Core](https://download.eclipse.org/oomph/www/setups/svg/JDT_Core.svg)](
https://www.eclipse.org/setups/installer/?url=https://raw.githubusercontent.com/eclipse-jdt/eclipse.jdt.core/master/org.eclipse.jdt.core.setup/JdtCoreConfiguration.setup&show=true
"Click to open Eclipse-Installer Auto Launch or drag into your running installer")

## License

[Eclipse Public License (EPL) v2.0](https://www.eclipse.org/legal/epl-2.0/)

## Links

- https://github.com/eclipse-jdt/eclipse.jdt.core/wiki
- https://github.com/eclipse-jdt/.github/blob/main/CONTRIBUTING.md
- https://projects.eclipse.org/projects/eclipse.jdt
