organization := "pl.org.miki"
name := "scalatron-hackathon-mk2"

libraryDependencies ++= {
  Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.2",
    "org.slf4j"       %   "slf4j-api"     % "1.7.7",
    "org.scalatest" %% "scalatest" % "2.2.1" % "test"
  )
}

