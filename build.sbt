lazy val root = (project in file(".")).
  settings(
      name := "HomNAND",
      version := "1.0",
      scalaVersion := "2.11.6",
      libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
      libraryDependencies += "com.codecommit" %% "gll-combinators" % "2.3",
scalacOptions ++= Seq("-feature")
      )

retrieveManaged := true
