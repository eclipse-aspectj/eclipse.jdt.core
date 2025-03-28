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

## How is it updated for a new version of Java?

This involves bringing over the latest JDT and merging it into this project.  This is a work
in progress description of the steps:

- checkout this repository, the AspectJ variant of JDT core:\
`git clone git@github.com:eclipse-aspectj/eclipse.jdt.core.git`

- add a remote for the real JDT called upstream\
`git remote add upstream git@github.com:eclipse-jdt/eclipse.jdt.core.git`

Now look at the latest commit in the real JDT

`git log --oneline remotes/upstream/master  | head`\

For example, for Java24 on a day at the end of March-2025:
```
557e0013ed Add tests to test propagation of annotations from record components to other declared targets (#3856)
```

*Note:* relying here on the fact that JDT is incredibly robust with mountains of tests. So master branch is not
unstable. (Personally I find the tagging strategy in JDT a bit confusing, so I use this rather than a tag)

*Note:* when picking up a new version of Java, you do want to check one of the commits to JDT is merging in their `BETA_JAVAXX` branch. For example:

```
git log --oneline remotes/upstream/master  | grep BETA_JAVA24 | head

857fbaee9e bumping up version post BETA_JAVA24 merge (#3839)
f3590a3a0c Merge remote-tracking branch 'origin/BETA_JAVA24'
```

### We are ready to start the merge

- create a branch to work in\
`git checkout -b java-24`

- merge the real JDT into this branch\
`git merge remotes/upstream/master`

- this will not go perfectly smoothly and how painful it is can vary :) 
  Your branch will now be in a half merged state with conflicts to sort out.


### Updating JDT dependencies

- ((TBD needs more detail)) see the `<dependencyManagement>` section in `org.eclipse.jdt.core/pom.xml` 


### Repeat this loop:

- check what is still a problem using\
`git status`\
There will be many entries in the `Changes to be committed:` - these are
where files have been merged successfully.\
Your work will be in the `Unmerged paths:` section, where changes in upstream have clashed with changes AspectJ makes to the same files.

- pick one of the files in `Unmerged paths` (don't pick `java.g`, `parser*.rsc`, `Parser` at this point, we will cover those later), open it and look for the problems usually with
traditional git 'clash' headers around it (so search for `<<<<` if you can't just see the syntax
errors in your IDE)

- fix it ((TBD - add some examples here))

- add it for committing\
`git add <fileWithNoMergeConflicts>.java`

- repeat this loop until you only have those files we were ignoring before.

### Updating java.g

Whenever tweaks are made to the java language (new constructs, new keywords), this file changes.
Resolving the clashes within it are not usually too difficult, the important things to remember are:
((TBD - fill in the important things))

Once cleared of conflicts, we need to run some tooling to generate code, the JDT team have
done something amazing recently and made it much less manual than it used to be (hurrah!)

- First, we need jikespg:
```
git clone https://github.com/jikespg/jikespg.git
cd jikespg/src
make
```

- Now we use the new script:
```
cd org.eclipse.jdt.core.compiler.batch/scripts
javac -classpath ../bin:. GenerateParserScript.java
java -DJIKESPG=<pathToJikespgExecutableBuiltAbove> -classpath ../bin:. GenerateParserScript
```
run the script (notice the ../bin which causes it to include the eclipse.jdt.core target folder for a build I already ran because I had eclipse.jdt.core imported into my IDE)

- This will generate the parser rule code that goes in the `Parser` class, generate
the `parser*.rsc` and `readableNames.properties` files (and move them to the right location), update the `TerminalToken` class and update the `ParserBasicInformation` class.

If all that works you need to `git add` all those pieces. If it doesn't work, fix your `java.g` file again and repeat.

### Testing

Congratulations!  I do not usually do the actual commit at this point because I want to test the above. 

- Update the version you want this to be. Open `org.eclipse.jdt.core/pom.xml` and find the version around line 136, as I update right now to Java 24, I change that to:
`<version>1.9.24</version>`

- Build it into the local maven repo with:\
`mvn clean install`

- Go to your clone of AspectJ and in the AspectJ parent `pom.xml` find the reference:\
`<jdt.core.version>1.9.24</jdt.core.version>` and update it to your just built version.

- Now run some tests, for example `SanityTests19`.  There is much more to do on the AspectJ side (and that may get added here later) but at this point if the sanity tests are passing, your patched JDT compiler isn't totally broken!

- See the other `AspectJ_JDK_Update` tags across the AspectJ project and fix those up too.

- In particular create new tests for a new version of Java, follow the instructions at the top of the most recent `AllTestsAspectJ<version>.java` file to create new test sources.

- If everything is still passing, you are good to start committing the patched JDT into your branch and merge your branch into master. In the `eclipse.jdt.core` folder:
```
git commit
Title: Merging in commit XXXXXXXXXX on D-MMM-YYYY to give Java NN support
Body: Anything unusual about the commit, for example for Java24 it included
the switch from ints to enums for the terminal tokens, which impacted a lot
of the AspectJ extension code.

git checkout aspectj
git merge java-24
git tag V1_9_24
```

## License

[Eclipse Public License (EPL) v2.0](https://www.eclipse.org/legal/epl-2.0/)

## Links

- https://github.com/eclipse-jdt/eclipse.jdt.core/wiki
- https://github.com/eclipse-jdt/.github/blob/main/CONTRIBUTING.md
- https://projects.eclipse.org/projects/eclipse.jdt
