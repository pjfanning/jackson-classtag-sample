name := "jackson-classtag-sample"

version := "0.1"

scalaVersion := "2.13.6"

resolvers += Resolver.sonatypeRepo("snapshots")

val jacksonVersion = "2.13.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % jacksonVersion
)