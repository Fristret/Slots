name := "Slots"

scalaVersion := "2.13.8"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Ymacro-annotations",
)

val circeVersion = "0.13.0"
val http4sVersion = "0.21.22"
val scalaTestVersion = "3.2.10"
val doobieVersion = "0.13.4"
val NewTypeVersion = "0.4.4"
val catsVersion = "2.6.1"
val catsEffectVersion = "2.5.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-jdk-http-client" % "0.3.6",
  "org.http4s" %% "http4s-json4s-native" % http4sVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "io.jvm.uuid" %% "scala-uuid" % "0.3.1",
  "org.tpolecat" %% "doobie-core" % doobieVersion,
  "org.tpolecat" %% "doobie-postgres-circe" % doobieVersion,
  "org.tpolecat" %% "doobie-hikari" % doobieVersion,
)

//addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.0")