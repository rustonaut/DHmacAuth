name := "dHMACAuth"

version := "0.1"

organization := "link.naicode"

version := "0.1"

scalaVersion := "2.11.2"

//dependencies for spec
libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.4.2" % "test"
)

//and setting for spec
scalacOptions in Test ++= Seq("-Yrangepos")


//dependencies for spray+akka
libraryDependencies ++= {
  val akkaV = "2.3.5"
  val sprayV = "1.3.1"
  Seq(
    //the resolver is added as plugin
    "io.spray" %% "spray-can" % sprayV,
    "io.spray" %% "spray-routing" % sprayV,
    "io.spray" %% "spray-testkit" % sprayV % "test",
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-testkit" % akkaV % "test"
  )
}

//its the spray resolver plugin, or?
Revolver.settings

scalacOptions in (Compile, doc) ++= Seq("-doc-root-content", baseDirectory.value+"/README.md")

scalacOptions in (Compile, doc) ++= Seq("-doc-title", "My Wonderful Module")
