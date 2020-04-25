ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"


val circeVersion = "0.12.3"
val commonDeps = Seq(
  "org.scalatest" %% "scalatest" % "3.1.1" % Test
)

lazy val root = (project in file("."))
  .settings(
    name := "skull",
    libraryDependencies ++= commonDeps,
  )
  .aggregate(api)

lazy val api = (project in file("api"))
  .settings(
    name := "api",
    libraryDependencies ++=
      Seq(
        "io.circe" %% "circe-core" % circeVersion,
        "io.circe" %% "circe-generic" % circeVersion,
        "io.circe" %% "circe-parser" % circeVersion,
        "com.amazonaws" % "aws-lambda-java-core" % "1.2.0",
        "com.amazonaws" % "aws-lambda-java-events" % "2.2.7",
        "org.scanamo" %% "scanamo" % "1.0.0-M12-1",
      ) ++ commonDeps,
  )

lazy val integration = (project in file("integration"))
  .settings(
    name := "integration",
    libraryDependencies ++=
      Seq(
        "org.scanamo" %% "scanamo-testkit" % "1.0.0-M12-1" % Test,
        "com.amazonaws" % "aws-java-sdk-dynamodb" % "1.11.762" % Test,
      ) ++ commonDeps,
    // start DynamoDB for tests
    dynamoDBLocalDownloadDir := file(".dynamodb-local"),
    dynamoDBLocalPort := 8042,
    startDynamoDBLocal := startDynamoDBLocal.dependsOn(compile in Test).value,
    test in Test := (test in Test).dependsOn(startDynamoDBLocal).value,
    testOnly in Test := (testOnly in Test).dependsOn(startDynamoDBLocal).evaluated,
    testOptions in Test += dynamoDBLocalTestCleanup.value,
  )
  .dependsOn(api % "compile->compile;test->test")
  .aggregate(api)

lazy val devServer = (project in file("devserver"))
  .settings(
    name := "devserver",
    libraryDependencies ++=
        Seq(
          "io.javalin" % "javalin" % "3.6.0",
          "org.slf4j" % "slf4j-simple" % "1.8.0-beta4",
          "org.slf4j" % "slf4j-api" % "1.8.0-beta4",
          "org.scanamo" %% "scanamo-testkit" % "1.0.0-M12-1",
          "com.amazonaws" % "aws-java-sdk-dynamodb" % "1.11.762",
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
  .dependsOn(api)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
