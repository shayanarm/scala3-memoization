name := "Memoize"
version := "0.1.0"
scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.9" % Test,
) 

scalacOptions ++= Seq(
    // "-Yretain-trees"
)