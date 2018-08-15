enablePlugins(JavaAppPackaging)

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finch-core" % "0.21.0",
  "com.github.finagle" %% "finch-circe" % "0.21.0",
  "io.circe" %% "circe-generic" % "0.9.0",
  "com.twitter" %% "twitter-server" % "18.6.0",
  "com.twitter" %% "finagle-stats" % "18.6.0",
  "com.twitter" %% "twitter-server-logback-classic" % "18.6.0",
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "com.typesafe" % "config" % "1.3.2"
)
