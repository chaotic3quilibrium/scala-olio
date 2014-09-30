/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.java.lang.Class_                          **
**   Name:      package.scala                                           **
**                                                                      **
** Description:                                                         **
**  Substitutes exception and null prone Java classes and methods with  **
**  Scala artifacts (like Try and Option) which play much more          **
**  consistently with other Scala idioms                                **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2014 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.java.lang

/** This package object serves to ease idiomatic Scala interactions
 *  with java.lang.Class.
 */
package object Class_ {

  /** This companion object/class pair Name is from the case class
   *  pattern template encapsulating the 'String' based name methods
   *  of java.lang.Class. Due to the way the Scala compiler generate
   *  JVM classes, the getSimpleName method of getClass can throw an
   *  exception when called on an Scala object is nested within another
   *  Scala object. This implementation falls back to using other more
   *  reliable methods of Class.
   */
  object Name {
    override def toString = apply(this.getClass).simpleName

    /** Holds all instantiations of Name indexed by java.lang.Class.
     */
    @volatile
    private var cache: Map[Class[_], Name] = Map()

    /** Holds "special case" identifier for Java array
     */
    private val JAVA_ARRAY: List[String] = List("Array")

    /** Fetches Name from internal cache if already generated,
     *  otherwise a new Name instance is generated and cached.
     *
     *  @param source the java.lang.Class which is to be Name
     *                encapsulated
     *  @return       the cached instance
     */
    def apply(source: Class[_]): Name = {
      this.synchronized {
        val result = cache.get(source)
        result match {
          case Some(name) =>
            name
          case None =>
            val rawNamePackages: Option[String] = {
              def fetchPackageAsGetNamePrefix: Option[String] = {
                val index = source.getName.lastIndexOf(".")
                if (index > -1)
                  Some(source.getName.take(index))
                else
                  //no package was provided
                  //ex: Array(1, 2).getName returns "[I"
                  None
              }
              //- DO NOT USE Some(source.getPackage) as it can validly
              //  return Some(null) - yup; all sorts of stupid here
              //- getPackage can return null; so using
              //  Option(source.getPackage) will return either Some()
              //  for actual non-null values or None for a null value
              Option(source.getPackage) match {
                case Some(packageGet) =>
                  //packageGet.getName might also be an empty String
                  //  even though getName can show a package
                  if (packageGet.getName.nonEmpty)
                    Some(packageGet.getName)
                  else
                    fetchPackageAsGetNamePrefix
                case None =>
                  fetchPackageAsGetNamePrefix
              }
            }
            val rawNameObjects =
              rawNamePackages match {
                case Some(rawNamePackagesGet) =>
                  source.getName.stripPrefix(rawNamePackagesGet + ".")
                case None =>
                  source.getName
              }
            val objects =
              if (rawNameObjects.startsWith("["))
                JAVA_ARRAY //special case for Java's array
              else
                rawNameObjects.split( """\$""").toList
            val packages =
              rawNamePackages match {
                case Some(rawNamePackagesGet) =>
                  Some(rawNamePackagesGet.split( """\.""").toList)
                case None =>
                  None
              }
            cache += (source -> new Name(source, objects, packages))
            cache(source)
        }
      }
    }

    /** Part of the case class pattern template providing the
     *  implementation for the final class 'Name'.
     */
    sealed private[Name] abstract case class Impl(
        source: Class[_]
      , objects: List[String]
      , packages: Option[List[String]]
    )
  }

  /** The final class of the case class pattern template which provides
   *  idiomatic Scala access to java.lang.Class. The simpleName method
   *  is guaranteed to never throw an exception, unlike its Java
   *  counterpart, java.lang.Class.getSimpleName.
   *
   *  @param source   the java.lang.Class which to be Name encapsulated
   *  @param objects  the non-empty ordered list of Scala object(s)
   *                  within which this particular class/object/trait
   *                  is nested, including this particular class's
   *                  name as the last element
   *  @param packages contains either Some(list) where list is a
   *                  non-empty ordered list of package name(s), or
   *                  None indicating no package was defined (ex: Java
   *                  arrays)
   */
  final class Name private[Name] (
      source: Class[_]
    , objects: List[String]
    , packages: Option[List[String]]
  ) extends Name.Impl(source, objects, packages) {
    /** Provides Name for class name replacing Impl from the case
     *  class's toString. Provided as a unique val (as opposed to only
     *  overriding toString) to enable access when using via an
     *  implicit masks this class's toString.
     *
     *  @return standard case class toString output modified to replace
     *          Impl with Name
     */
    lazy val toStringUnmasked: String =
      Name.toString + super.toString.dropWhile(_ != '(')

    /** Modified to override Impl.toString implementation
     *
     *  @return toStringUnmasked
     */
    override def toString: String =
      toStringUnmasked

    /** val indicating whether the source is a Java array.
     */
    val isJavaArray: Boolean = objects == Name.JAVA_ARRAY

    /** val containing String value of class/object/trait without
     *  generating an exception from java.lang.Class.getSimpleName
     *  (includes working correctly within Scala Worksheet).
     */
    val simpleName: String = objects.reverse.head

    /** val indicating whether the source is a Scala
     *  object (true) or not (false - either a Java class or Scala
     *  class/trait).
     */
    val isScalaObject: Boolean = source.getName.endsWith("$")

    private val rawNameSpace: Option[String] =
      if (!isJavaArray) {
        val getNameClipped =
          source.getName.stripSuffix(
            simpleName + (if (isScalaObject) "$" else "")
          )
        if (getNameClipped.nonEmpty)
          Some(getNameClipped)
        else
          None
      }
      else
        None //special case for Java's array

    /** val indicating whether the parent of this is a package (true)
     *  or a Scala class/object/trait (false)
     */
    val isParentAPackage: Boolean =
      rawNameSpace match {
        case Some(rawNameSpaceGet) =>
          rawNameSpaceGet.endsWith(".")
        case None =>
          true
      }

    /** val contains Some(String) where String is the fully specified
     *  name space for the Java class or Scala class/object/trait with
     *  simpleName removed
     */
    val nameSpace: Option[String] =
      rawNameSpace match {
        case Some(rawNameSpaceGet) =>
          Some(rawNameSpaceGet.take(rawNameSpaceGet.size - 1))
        case None =>
          None
      }
  }

  /** Implicit method to enable simple conversion from java.lang.Class
   *  to Name
   *
   *  @param source the java.lang.Class which to be Name encapsulated
   *  @return       the cached Name instance
   */
  implicit def convertClassToName(source: Class[_]): Name =
    Name(source)
}
/*
This Scala class is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

To see details of the GPLv3 License, please see
<http://www.gnu.org/copyleft/gpl.html>.
To see details of the GNU General Public License, please see
<http://www.gnu.org/licenses/>.

If you would like to obtain a custom/different/commercial license for
this, please send an email with your request to
<jim.oflaherty.jr@gmail.com>.
*/
