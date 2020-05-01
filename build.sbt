ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"


val circeVersion = "0.12.3"
val scanamoVersion = "1.0.0-M12-1"
val awsJavaSdkVersion = "1.11.762"
val commonDeps = Seq(
  "org.scalatest" %% "scalatest" % "3.1.1" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.1" % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % "3.1.1.1" % Test,
)

lazy val root = (project in file("."))
  .settings(
    name := "skull",
    libraryDependencies ++= commonDeps,
  )
  .aggregate(core, lambda, devServer, integration, cli)

lazy val core = (project in file("core"))
  .settings(
    name := "core",
    libraryDependencies ++=
      Seq(
        "io.circe" %% "circe-core" % circeVersion,
        "io.circe" %% "circe-generic" % circeVersion,
        "io.circe" %% "circe-parser" % circeVersion,
        "org.scanamo" %% "scanamo" % scanamoVersion,
      ) ++ commonDeps,
  )

lazy val lambda = (project in file("lambda"))
  .settings(
    name := "core",
    libraryDependencies ++=
      Seq(
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
        "com.amazonaws" % "aws-lambda-java-core" % "1.2.0",
        "com.amazonaws" % "aws-lambda-java-events" % "2.2.7",
      ) ++ commonDeps,
  )
  .dependsOn(core)

lazy val integration = (project in file("integration"))
  .settings(
    name := "integration",
    libraryDependencies ++=
      Seq(
        "org.scanamo" %% "scanamo-testkit" % scanamoVersion % Test,
        "com.amazonaws" % "aws-java-sdk-dynamodb" % awsJavaSdkVersion % Test,
      ) ++ commonDeps,
    // start DynamoDB for tests
    dynamoDBLocalDownloadDir := file(".dynamodb-local"),
    dynamoDBLocalPort := 8042,
    startDynamoDBLocal := startDynamoDBLocal.dependsOn(compile in Test).value,
    test in Test := (test in Test).dependsOn(startDynamoDBLocal).value,
    testOnly in Test := (testOnly in Test).dependsOn(startDynamoDBLocal).evaluated,
    testOptions in Test += dynamoDBLocalTestCleanup.value,
  )
  .dependsOn(core % "compile->compile;test->test")
  .aggregate(core)

lazy val devServer = (project in file("devserver"))
  .settings(
    name := "devserver",
    libraryDependencies ++=
        Seq(
          "io.javalin" % "javalin" % "3.6.0",
          "org.slf4j" % "slf4j-simple" % "1.8.0-beta4",
          "org.slf4j" % "slf4j-api" % "1.8.0-beta4",
          "org.scanamo" %% "scanamo-testkit" % scanamoVersion,
          "com.amazonaws" % "aws-java-sdk-dynamodb" % awsJavaSdkVersion,
        ) ++ commonDeps,
    fork in run := true,
    connectInput in run := true,
    outputStrategy := Some(StdoutOutput),
    // start DynamoDB on run
    dynamoDBLocalDownloadDir := file(".dynamodb-local"),
    dynamoDBLocalPort := 8042,
    startDynamoDBLocal := startDynamoDBLocal.dependsOn(compile in Compile).value,
    (run in Compile) := (run in Compile).dependsOn(startDynamoDBLocal).evaluated,
  )
  .dependsOn(core)

lazy val cli = (project in file("cli"))
  .settings()
  .dependsOn(core)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
