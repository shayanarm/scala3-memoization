inThisBuild(List(
  organization := "io.github.shayanarm",
  homepage := Some(url("https://github.com/shayanarm/scala3-memoization")),
  // Alternatively License.Apache2 see https://github.com/sbt/librarymanagement/blob/develop/core/src/main/scala/sbt/librarymanagement/License.scala
  licenses := List("MIT" -> url("https://opensource.org/licenses/MIT")),
  developers := List(
    Developer(
      "shayanarm",
      "Amir Shayan Armaghan",
      "amirshayan.armaghan@gmail.com",
      url("https://www.linkedin.com/in/amir-shayan-armaghan-6019b488/")
    )
  )
))

name := "scala3-memoization"
version := "0.1.0"
crossScalaVersions := Seq("3.3.1")
scalaVersion := crossScalaVersions.value.head

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.9" % Test,
) 

scalacOptions ++= Seq(
    // "-Yretain-trees"
)
