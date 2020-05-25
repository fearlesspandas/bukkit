resolvers++=Seq(
  "Spigot Snapshots" at "https://hub.spigotmc.org/nexus/content/repositories/snapshots",
   "Sonatype" at "https://oss.sonatype.org/content/repositories/snapshots/"
)
// https://mvnrepository.com/artifact/org.apache.spark/spark-core
libraryDependencies ++= Seq("org.spigotmc" % "spigot-api" % "1.15.1-R0.1-SNAPSHOT" % "provided")

// https://mvnrepository.com/artifact/net.liftweb/lift-json
libraryDependencies += "net.liftweb" %% "lift-json" % "3.4.1"

// https://mvnrepository.com/artifact/com.fasterxml.jackson.module/jackson-module-scala
libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.11.0"

assemblyMergeStrategy in assembly := {
  case "module-info.class" => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}