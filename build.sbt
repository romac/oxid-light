
name := "oxid"

version := "0.1.0"

scalaVersion := "2.12.5"

resolvers += "uuverifiers" at "http://logicrunch.it.uu.se:4096/~wv/maven/"

libraryDependencies += "ch.epfl.lara" %% "inox" % "1.1.0-77-gcda28de"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

scalacOptions += "-Ypartial-unification"

