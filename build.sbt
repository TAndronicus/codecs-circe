course := "progfun2"
assignment := "codecs"

scalaVersion := "2.13.1"
scalacOptions ++= Seq("-deprecation")
libraryDependencies ++= Seq(
  "org.typelevel" %% "jawn-parser" % "0.14.2",
  "org.scalacheck" %% "scalacheck" % "1.14.2" % Test,
  "com.novocode" % "junit-interface" % "0.11" % Test
)

val circeVersion = "0.12.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")