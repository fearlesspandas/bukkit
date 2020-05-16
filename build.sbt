resolvers++=Seq(
  "Spigot Snapshots" at "https://hub.spigotmc.org/nexus/content/repositories/snapshots",
   "Sonatype" at "https://oss.sonatype.org/content/repositories/snapshots/"
)
// https://mvnrepository.com/artifact/org.apache.spark/spark-core
libraryDependencies ++= Seq("org.spigotmc" % "spigot-api" % "1.15.1-R0.1-SNAPSHOT" % "provided")

// https://mvnrepository.com/artifact/net.liftweb/lift-json
libraryDependencies += "net.liftweb" %% "lift-json" % "3.4.1"
