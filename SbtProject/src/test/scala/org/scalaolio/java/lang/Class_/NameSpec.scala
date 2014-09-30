package org.scalaolio.java.lang.Class_

import org.scalatest._

class NameSpec extends FlatSpec with Matchers {
  "An explicit Name" should "return valid values for scala.Byte" in {
    val sourceClass = Byte.getClass
    val name = Name(sourceClass)
    name.source should be (sourceClass)
    name.objects should be (List("Byte"))
    name.packages should be (Some(List("scala")))
    name.toString should be ("Name(class scala.Byte$,List(Byte),Some(List(scala)))")
    name.toStringUnmasked should be (name.toString)
    name.isJavaArray should be (false)
    name.simpleName should be ("Byte")
    name.isScalaObject should be (true)
    name.isParentAPackage should be (true)
    name.nameSpace should be (Some("scala"))
  }
  it should "return valid values for java.lang.String" in {
    val sourceClass = "Jim".getClass
    val name = Name(sourceClass)
    name.source should be (sourceClass)
    name.objects should be (List("String"))
    name.packages should be (Some(List("java", "lang")))
    name.toString should be ("Name(class java.lang.String,List(String),Some(List(java, lang)))")
    name.toStringUnmasked should be (name.toString)
    name.isJavaArray should be (false)
    name.simpleName should be ("String")
    name.isScalaObject should be (false)
    name.isParentAPackage should be (true)
    name.nameSpace should be (Some("java.lang"))
  }
  it should "return valid values for JVM array" in {
    val sourceClass = Array(1, 2, 3, 4, 5).getClass
    sourceClass.getName should be ("[I")
    val name = Name(sourceClass)
    name.source should be (sourceClass)
    name.objects should be (List("Array"))
    name.packages should be (None)
    name.toString should be ("Name(class [I,List(Array),None)")
    name.toStringUnmasked should be (name.toString)
    name.isJavaArray should be (true)
    name.simpleName should be ("Array")
    name.isScalaObject should be (false)
    name.isParentAPackage should be (true)
    name.nameSpace should be (None)
  }

  object A {
    object B {
      object C
      class C
    }
  }

  it should "throw InternalError if java.lang.Class.getSimpleName is called" in {
    val sourceClass = A.B.C.getClass
    a [InternalError] should be thrownBy {
      sourceClass.getSimpleName
    }
  }
  it should "return valid values for object A.B.C (companion object)" in {
    val sourceClass = A.B.C.getClass
    val name = Name(sourceClass)
    name.source should be (sourceClass)
    name.objects should be (List("NameSpec", "A", "B", "C"))
    name.packages should be (Some(List("org", "scalaolio", "java", "lang", "Class_")))
    name.toString should be ("Name(class org.scalaolio.java.lang.Class_.NameSpec$A$B$C$,List(NameSpec, A, B, C),Some(List(org, scalaolio, java, lang, Class_)))")
    name.toStringUnmasked should be (name.toString)
    name.isJavaArray should be (false)
    name.simpleName should be ("C")
    name.isScalaObject should be (true)
    name.isParentAPackage should be (false)
    name.nameSpace should be (Some("org.scalaolio.java.lang.Class_.NameSpec$A$B"))
  }
  it should "return valid values for instance A.B.C (class)" in {
    val sourceClass = (new A.B.C).getClass
    val name = Name(sourceClass)
    name.source should be (sourceClass)
    name.objects should be (List("NameSpec", "A", "B", "C"))
    name.packages should be (Some(List("org", "scalaolio", "java", "lang", "Class_")))
    name.toString should be ("Name(class org.scalaolio.java.lang.Class_.NameSpec$A$B$C,List(NameSpec, A, B, C),Some(List(org, scalaolio, java, lang, Class_)))")
    name.toStringUnmasked should be (name.toString)
    name.isJavaArray should be (false)
    name.simpleName should be ("C")
    name.isScalaObject should be (false)
    name.isParentAPackage should be (false)
    name.nameSpace should be (Some("org.scalaolio.java.lang.Class_.NameSpec$A$B"))
  }

  object D {
    object E {
      object F
      class F
    }
  }

  "An implicit Name" should "return valid values for JVM array" in {
    val source = Array(1, 2, 3, 4, 5)
    source.getClass.source should be (source.getClass)
    source.getClass.objects should be (List("Array"))
    source.getClass.packages should be (None)
    source.getClass.toString should be ("class [I") //Name.toString masked
    source.getClass.toStringUnmasked should be ("Name(class [I,List(Array),None)") //Name.toString masked
    source.getClass.isJavaArray should be (true)
    source.getClass.simpleName should be ("Array")
    source.getClass.isScalaObject should be (false)
    source.getClass.isParentAPackage should be (true)
    source.getClass.nameSpace should be (None)
  }

  it should "return valid values for object A.B.C (companion object)" in {
    val source = D.E.F
    source.getClass.source should be (source.getClass)
    source.getClass.objects should be (List("NameSpec", "D", "E", "F"))
    source.getClass.packages should be (Some(List("org", "scalaolio", "java", "lang", "Class_")))
    source.getClass.toString should be ("class org.scalaolio.java.lang.Class_.NameSpec$D$E$F$")
    source.getClass.toStringUnmasked should be (source.getClass.toString)
    source.getClass.isJavaArray should be (false)
    source.getClass.simpleName should be ("F")
    source.getClass.isScalaObject should be (true)
    source.getClass.isParentAPackage should be (false)
    source.getClass.nameSpace should be (Some("org.scalaolio.java.lang.Class_.NameSpec$D$E"))
  }
}
