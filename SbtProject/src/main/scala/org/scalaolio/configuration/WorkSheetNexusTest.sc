val nexus1 =
  Nexus(
      "Root"
    , List(
          "CommandLineArgs."                //things which must be calculated by the launching script; ex: web service's server's host name and port
        , """app/conf/config.json - AppHost.""" //things specific to the particular application's install location; ex: JDBC Connection details
        , "config.json - JvmClassPathRoot." //things specific to the application deployment file; ex: logging prefixing
        , "config.json - JdbcDatabase"      //all other configuration, including the ability to "override" values from above "prefixed" datasets
      )
  )
val nexusTransformAsOfNow1 =
  nexus1.get.currentTransformDateTimeStamped
val transformCommandLineArgs =
  Transform(
    List(
        "CommandLineArgs.host.name" -> "aliAws01"
      , "CommandLineArgs.host.port" -> "8443"
    )
  )
val nexus2 =
  nexus1.get.addOrReplace(Nexus.TransformNamed(transformCommandLineArgs.get, "CommandLineArgs.").get)
val nexusTransformAsOfNow2 =
  nexus2.get.currentTransformDateTimeStamped
val transformAppHost =
  Transform(
    List(
        "AppHost.jdbc.url.driverName" -> "org.h2.Driver"
      , "AppHost.jdbc.url.connectionUrl" -> "jdbc:h2:tcp://localhost/~/test"
      , "AppHost.jdbc.url.username" -> "sa"
      , "AppHost.jdbc.url.password" -> "sa"
      //, "AppHost.jdbc.url.suppressSQLExceptionLogging" -> "false"
      //, "AppHost.jdbc.jndi.name" -> "jndi:coreDb"
    )
  )
val nexus3 =
  nexus2.get.addOrReplace(Nexus.TransformNamed(transformAppHost.get, """app/conf/config.json - AppHost.""").get)
val nexusTransformAsOfNow3 =
  nexus3.get.currentTransformDateTimeStamped
val transformJdbcDatabase =
  Transform(
    List(
        "readOnly.akka.system.name" -> "host-actor-system"
      , "readOnly.akka.system.globalTimeout" -> "10,SECONDS"
      , "readOnly.akka.service.admin.name" -> "service-admin"
      , "readOnly.akka.service.admin.interface" -> "template=>[CommandLineArgs.host.name]"
      , "readOnly.akka.service.admin.port" -> "8081"
      , "readOnly.akka.service.saas.name" -> "service-saas"
      , "readOnly.akka.service.saas.interface" -> "localhost"
      , "readOnly.akka.service.saas.port" -> "8443"
      , "CommandLineArgs.host.name" -> "com.assetlocationintelligence.aliAws01"
    )
  )
val nexus4 =
  nexus3.get.addOrReplace(Nexus.TransformNamed(transformJdbcDatabase.get, "config.json - JdbcDatabase").get)
val nexusTransformAsOfNow4 =
  nexus4.get.currentTransformDateTimeStamped
val subset =
  nexus4.get.subset("")
val valueHost =
  subset.get.valueTyped.string("CommandLineArgs.host.name")
val valueInterface =
  subset.get.valueTyped.string("readOnly.akka.service.admin.interface")
