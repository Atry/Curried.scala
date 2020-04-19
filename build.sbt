ThisBuild / organization := "com.yang-bo"

publish / skip  := true

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val Curried = crossProject(JSPlatform, JVMPlatform).build
