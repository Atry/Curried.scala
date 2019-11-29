ThisBuild / organization := "com.yang-bo"

publish / skip  := true

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val Curried = crossProject(JVMPlatform)

lazy val CurriedJVM = Curried.jvm

ThisBuild / scalaVersion := "0.20.0-RC1"