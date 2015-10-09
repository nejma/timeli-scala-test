autoScalaLibrary := false

scalaVersion := "2.11.7"

version := "0.0.1"

organization := "io.timeli"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq (
	"com.typesafe.play" %% "play-json" % "2.4.3",
	"org.scalatest" %% "scalatest" % "2.2.5" % "test",
	"junit" % "junit" % "4.12" % "test"
      )

