# AspectJ JDT Core

This is an AspectJ-enhanced fork of [Eclipse JDT Core](https://github.com/eclipse-jdt/eclipse.jdt.core).
The AspectJ Compiler (AJC) builds upon the Eclipse Java Compiler (ECJ) and its annotation processing (APT) support,
adding native AspectJ syntax parsing and compilation to the JDT Core batch compiler.

The rest of JDT Core is mostly ignored, and the subset of classes necessary for inclusion into AspectJ Core is built
separately by Maven, completely bypassing the upstream build configuration.

## License

[Eclipse Public License (EPL) v2.0](https://www.eclipse.org/legal/epl-2.0/)
