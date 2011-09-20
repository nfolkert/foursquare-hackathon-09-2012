import sbt._

class ScalaFoursquareProject(info: ProjectInfo) extends DefaultWebProject(info) {
  override def managedStyle = ManagedStyle.Maven
  override def packageSrcJar= defaultJarPath("-sources.jar")

  val liftVer = "2.4-M1"

  val liftWebkit = "net.liftweb" %% "lift-webkit" % liftVer % "compile->default" withSources()
  val liftMapper = "net.liftweb" %% "lift-mapper" % liftVer % "compile->default"

  val liftCommon = "net.liftweb" %% "lift-common" % liftVer % "compile" withSources()
  val liftUtil = "net.liftweb" %% "lift-util" % liftVer % "compile" withSources()
  val liftJson = "net.liftweb" %% "lift-json" % liftVer % "compile" withSources()

  val liftMongo       = "net.liftweb" %% "lift-mongodb" % liftVer % "compile" withSources() intransitive()
  val liftMongoRecord = "net.liftweb" %% "lift-mongodb-record" % liftVer % "compile" withSources() intransitive()
  val mongo        = "org.mongodb" % "mongo-java-driver" % "2.5.3a_fs" withSources()

  val scalajCollection = "org.scalaj" %% "scalaj-collection" % "1.1"
  val scalajHttp = "org.scalaj" %% "scalaj-http" % "0.2.8" % "compile" withSources()

  val junit = "junit" % "junit" % "4.8.2" % "test" withSources()
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.5" % "test" withSources()

  val servlet = "javax.servlet" % "servlet-api" % "2.5" % "provided"
  val jetty6 = "org.mortbay.jetty" % "jetty" % "6.1.25" % "test,provided"
  val jettyutils = "org.mortbay.jetty" % "jetty-util" % "6.1.25" % "test,provided"

  val logback = "ch.qos.logback" % "logback-classic" % "0.9.26"

  val h2db = "com.h2database" % "h2" % "1.2.138"

  val bryanjswift = "Bryan J Swift Repository" at "http://repos.bryanjswift.com/maven2/"
  val junitInterface = "com.novocode" % "junit-interface" % "0.6" % "test"
  override def testFrameworks = super.testFrameworks ++ List(new TestFramework("com.novocode.junit.JUnitFrameworkNoMarker"))
}
