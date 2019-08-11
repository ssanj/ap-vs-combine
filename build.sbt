name := "ap-vs-combine"

organization := "blog"

version := "0.0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
 "org.typelevel" %% "cats-effect" % "1.3.1",
 "net.ssanj" %% "boon" % "0.0.9-b01" % Test
)

testFrameworks += new TestFramework("boon.sbt.BoonFramework")

lazy val commonCompilerOptions =
  Seq(
      "-unchecked",
      "-encoding", "UTF-8",
      "-deprecation",
      "-feature",
      "-Ypartial-unification"
    )

scalacOptions ++= 
  commonCompilerOptions ++ 
  Seq(
      "-Xfatal-warnings",
      "-Xlint:_",
      "-Ywarn-dead-code",
      "-Ywarn-inaccessible",
      "-Ywarn-unused-import",
      "-Ywarn-infer-any",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
     )

scalacOptions in (Compile, console) := commonCompilerOptions

scalacOptions in (Test, console) := commonCompilerOptions

