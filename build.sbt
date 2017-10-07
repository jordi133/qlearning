
lazy val scalaV = "2.12.3"
lazy val scalaTestV = "3.0.1"
lazy val scalaCheckV = "1.13.4"

lazy val root = (project in file("."))
  .settings(
    name := "Q-Learning",
    scalaVersion := scalaV,
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % scalaCheckV % "test",
      "org.scalactic" %% "scalactic" % scalaTestV % "test",
      "org.scalatest" %% "scalatest" % scalaTestV % "test",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
      "com.storm-enroute" %% "scalameter" % "0.8.2",
      "junit" % "junit" % "4.8.1" % "test"
    ),
    Defaults.itSettings)
  .configs(IntegrationTest)

testFrameworks += new TestFramework(
  "org.scalameter.ScalaMeterFramework")

logBuffered := false

parallelExecution in IntegrationTest := false