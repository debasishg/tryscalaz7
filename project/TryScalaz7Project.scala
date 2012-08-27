import sbt._
import Keys._

object TryScalaz7Project extends Build
{
  lazy val root = Project("TryScalaz7", file(".")) settings(coreSettings : _*)

  lazy val commonSettings: Seq[Setting[_]] = Seq(
    organization := "net.debasishg",
    version := "0.0.1",
    scalaVersion := "2.10.0-M7",
    scalacOptions ++= Seq("-deprecation", "-unchecked")
  )

  lazy val coreSettings = commonSettings ++ Seq(
    name := "TryScalaz7",

    libraryDependencies ++= Seq(
      "commons-pool"       % "commons-pool"     % "1.6",
      "junit"              % "junit"            % "4.8.1"            % "test",
      "org.scalatest"      % "scalatest_2.10.0-M7"  % "1.9-2.10.0-M7-B1"            % "test",
      // "org.scalacheck"     %% "scalacheck"      % "1.9"              % "test",
      "org.scalaz"         % "scalaz-core_2.10.0-M6" % "7.0.0-M2",
      "org.scalaz"         % "scalaz-effect_2.10.0-M6" % "7.0.0-M2"),


    parallelExecution in Test := false,
    publishTo <<= version { (v: String) => 
      val nexus = "https://oss.sonatype.org/" 
      if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2") 
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { repo => false },
    pomExtra := (
      <url>https://github.com/debasishg/tryscalaz7</url>
      <licenses>
        <license>
          <name>Apache 2.0 License</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:debasishg/tryscalaz7.git</url>
        <connection>scm:git:git@github.com:debasishg/tryscalaz7.git</connection>
      </scm>
      <developers>
        <developer>
          <id>debasishg</id>
          <name>Debasish Ghosh</name>
          <url>http://debasishg.blogspot.com</url>
        </developer>
      </developers>),
    unmanagedResources in Compile <+= baseDirectory map { _ / "LICENSE" }
  )

}

