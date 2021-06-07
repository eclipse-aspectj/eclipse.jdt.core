#!/usr/bin/bash

# Copyright (c) 2021 Contributors.
# All rights reserved.
# This program and the accompanying materials are made available
# under the terms of the Eclipse Public License v 2.0
# which accompanies this distribution, and is available at
# https://www.eclipse.org/org/documents/epl-2.0/EPL-2.0.txt

# List all 'jdtDepends' JARs used by build.xml.
#
# Caveat: There might be multiple versions per JAR.
# Select the one you need, possibly the latest if you are actively developing AspectJ JDT Core.
#
# Use the script like this (Windows Git Bash example):
# ./list-jdtDepends.sh /c/Program\ Files/Eclipse/2019-12
#
# Then open the file and copy the '<zipfileset .../>' lines to build.xml
#
# Can be something like "/c/Program\ Files/Eclipse/2019-12" or,
# if using Eclipse Installer (by Oomph), something like "/c/Users/me/.p2/pool"
export eclipseHome="$1"

export pluginsDir="$eclipseHome/plugins"
cd "$pluginsDir"

{
  ls -1 org.eclipse.core.contenttype_*.jar
  ls -1 org.eclipse.core.filesystem_*.jar
  ls -1 org.eclipse.core.jobs_*.jar
  ls -1 org.eclipse.core.resources_*.jar
  ls -1 org.eclipse.core.runtime_*.jar
  ls -1 org.eclipse.equinox.app_*.jar
  ls -1 org.eclipse.equinox.common_*.jar
  ls -1 org.eclipse.equinox.preferences_*.jar
  ls -1 org.eclipse.equinox.registry_*.jar
  ls -1 org.eclipse.osgi_*.jar
  ls -1 org.eclipse.text_*.jar
} | sed -E 's#(.*)#<zipfileset src="${plugins.dir}/\1"/>#'

cd - > /dev/null
