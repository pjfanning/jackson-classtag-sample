name := "jackson-classtag-sample"

version := "0.1"

scalaVersion := "2.13.6"

scalacOptions += "-Vimplicits"

//resolvers += Resolver.sonatypeRepo("snapshots")

val jacksonVersion = "2.12.3"

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion
)