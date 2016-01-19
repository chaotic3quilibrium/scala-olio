import scala.util.{Failure, Success, Try}
import org.scalaolio.configuration.{Transform, Nexus}
val tryNexusClean =
  Nexus.tryApply(
      "WorkSheetTestTemplateWedge"
    , List(
          "COMMAND_LINE_ARGS"
        , "JVM_CLASSPATH_CONFIGURATION"
        , "SERVER_APP_CONF"
        , "DATA_STORE"
      )
    //, templateProfile = None
    , optionTemplateWedgeKeyExceptionResolver =
      Some(
        (subset, key, valueSanTemplatePrefix) => {
          val prefix =
            key.takeWhile(_ != '.')
          if (prefix.nonEmpty)
            subset.nexus.currentTransformDateTimeStamped.transform.valueTypedMap.valueByKey.get(prefix + "." + valueSanTemplatePrefix)
          else
            None
        }
      )
  )
def tryAddOrUpdateToNexus(
    tryNexus: Try[Nexus]
  , keyAndValuesPrefixed: List[(String, String)]
  , sourceName: String
): Try[Nexus] =
  for {
    nexus <-
      tryNexus
    transform <-
      Transform(keyAndValuesPrefixed)
    transformNamed <-
      Nexus.TransformNamed.tryApply(transform, sourceName)
    nexusNew <-
      nexus.tryAddOrReplace(transformNamed)
  } yield nexusNew

val tryNexusAddedCommandLineArgs =
  tryAddOrUpdateToNexus(
      tryNexusClean
    , List(
          "commandlineargs.host.name" -> "aliAws01"
        , "commandlineargs.host.ip" -> "127.0.0.1"
        , "merged.host.name" -> "aliAws01"
        , "merged.host.ip" -> "127.0.0.1"
      )
    , "COMMAND_LINE_ARGS"
  )
val tryNexusAddedServerAppConf =
  tryAddOrUpdateToNexus(
    tryNexusClean
    , List(
        "serverappconf.akka.service.saas.name" -> "service-saas"
      , "serverappconf.akka.service.saas.interface" -> "template=>[host.ip]"
      , "serverappconf.akka.service.saas.port" -> "8443"
      , "merged.akka.service.saas.name" -> "service-saas"
      , "merged.akka.service.saas.interface" -> "template=>[host.ip]"
      , "merged.akka.service.saas.port" -> "8443"
    )
    , "SERVER_APP_CONF"
  )
def toMap(tryNexus: Try[Nexus], keyPrefix: String = "", retainKeyPrefix: Boolean = false): Try[Map[String, String]] =
  for {
    nexus <-
      tryNexus
    subsetRoot <-
      nexus.trySubset(keyPrefix, retainKeyPrefix)
  } yield subsetRoot.toMap
val tryContent =
  for {
    valueByKey <-
      toMap(tryNexusAddedServerAppConf)
  } yield {
    for {
      (key, value) <-
        valueByKey
    } yield key + " -> " + (if (key.toLowerCase.endsWith(".password")) "********" else value)
  }
val contentAsString =
  tryContent match {
    case Success(lines) =>
      lines.toList.sorted.mkString("\n")
    case Failure(e) =>
      s"failed to produce content - ${e.getMessage}"
  }
