val q = 1
//
import org.scalaolio.java.lang.Class_._
//
object AJim {
  object BJim {
    object CJim1
    class CJim2
  }
}
val cJim1 = AJim.BJim.CJim1
val cJim2 = new AJim.BJim.CJim2
val source1: Class[_] = cJim1.getClass
val source2: Class[_] = cJim2.getClass
val rawNameClass = source1.getName
val rawNamePackages = source1.getPackage.getName
val rawNameObjects = rawNameClass.stripPrefix(rawNamePackages + ".")
val packages = rawNamePackages.split("""\.""").toList
val objects = rawNameObjects.split("""\$""").toList
val cn1 = Name(source1)
val cn1sn = cn1.simpleName
val cn1iso = cn1.isScalaObject
val cn1ns = cn1.nameSpace
val cn1fn = cn1.fullName
val cn2 = Name(source2)
val cn2sn = cn2.simpleName
val cn2iso = cn2.isScalaObject
val cn2ns = cn2.nameSpace
val cn2fn = cn2.fullName
val x = convertClassToName(source1).simpleName
val y = source1.simpleName
