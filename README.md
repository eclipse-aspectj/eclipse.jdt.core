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

## License

[Eclipse Public License (EPL) v2.0](https://www.eclipse.org/legal/epl-2.0/)
