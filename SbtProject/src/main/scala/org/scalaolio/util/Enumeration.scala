/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util                                      **
**   Name:      Enumeration.scala                                       **
**                                                                      **
** Description:                                                         **
**  DIY Scala Enumeration (closest possible Java Enum equivalent with   **
**  guaranteed pattern matching exhaustiveness checking).               **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2014 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util

import org.scalaolio.java.lang.Class_.Name

import scala.reflect.runtime.universe.TypeTag
import scala.util.{Failure, Success, Try}

import org.scalaolio.java.lang.Class_
import org.scalaolio.Trait_

/**
 *
 */
trait Enumeration {
  /**
   *
   */
  type Member <: MemberBase

  private lazy val memberSealedTraitDescendantNames: Try[Set[String]] =
      Trait_.fetchSealedTraitDescendantsViaTraitSymbol(typeTagMember.tpe.typeSymbol) match {
        case Some(symbols) => Success(symbols.map(_.toString.stripPrefix("object ")))
        case None => Failure(new IllegalStateException("reflection failed to obtain descendants of sealed trait Member"))
      }

  @volatile
  private var addFailure: Option[Throwable] = None
  @volatile
  private var isAddClosed: Boolean = false
  @volatile
  private var membersTempInternal: List[Member] = Nil
  @volatile
  private var memberNamesLowerCaseAsSetInternal: Set[String] = Set()
  final private def add(member: Member): Try[Unit] = {
    def postAddFailure(throwable: Throwable): Try[Unit] = {
      val failure = Failure(throwable)
      addFailure = Some(throwable)
      failure
    }
    this.synchronized {
      addFailure match {
        case Some(throwable) => Failure(throwable)
        case None =>
          if (!isAddClosed)
            memberSealedTraitDescendantNames.flatMap(memberSealedTraitDescendantNamesFm => {
              val memberSealedTraitDescendantNamesFmLowerCase = memberSealedTraitDescendantNamesFm.map(_.toLowerCase)
              val className = Name(getClass)
              if (className.fullName.toLowerCase == member.nameSpace.getOrElse("").toLowerCase)
                if (!memberNamesLowerCaseAsSetInternal.contains(member.name.toLowerCase)) {
                  membersTempInternal = member :: membersTempInternal
                  memberNamesLowerCaseAsSetInternal = memberNamesLowerCaseAsSetInternal + member.name.toLowerCase
                  isAddClosed = memberNamesLowerCaseAsSetInternal == memberSealedTraitDescendantNamesFmLowerCase
                  if (isAddClosed)
                    if (orderedSet.nonEmpty) {
                      val orderedSetAsRealSet = orderedSet.toSet
                      if (orderedSet.size == orderedSetAsRealSet.size) {
                        val orderedSetNamesLowerCase = orderedSetAsRealSet.map(_.name.toLowerCase)
                        if (orderedSetNamesLowerCase == memberSealedTraitDescendantNamesFmLowerCase)
                          Success(())
                        else {
                          val inOnlyOrderedSet =
                            orderedSetNamesLowerCase.--(memberSealedTraitDescendantNamesFmLowerCase)
                          val inOnlyMemberSealedTraitDescendantNamesFm =
                            memberSealedTraitDescendantNamesFmLowerCase.--(orderedSetNamesLowerCase)
                          postAddFailure(
                            new IllegalStateException(
                                s"orderedSetNamesLowerCase [${orderedSetNamesLowerCase.mkString(",")}] is not equal to "
                              + s"memberSealedTraitDescendantNamesFmLowerCase"
                              + s" [${memberSealedTraitDescendantNamesFmLowerCase.mkString(",")}]"
                              + ( if (inOnlyOrderedSet.nonEmpty)
                                      s" - orderedSetNamesLowerCase contains values [${inOnlyOrderedSet.mkString(",")}]"
                                    + s" not in memberSealedTraitDescendantNamesFmLowerCase"
                                  else ""
                                )
                              + ( if (inOnlyMemberSealedTraitDescendantNamesFm.nonEmpty)
                                      s" - memberSealedTraitDescendantNamesFmLowerCase contains values"
                                    + s" [${inOnlyMemberSealedTraitDescendantNamesFm.mkString(",")}] not in"
                                    + s" orderedSetNamesLowerCase"
                                  else ""
                                )
                            )
                          )
                        }
                      }
                      else
                        postAddFailure(
                          new IllegalStateException(
                              s"orderedSet.size [${orderedSet.size}] must be equal to orderedSetAsRealSet.size"
                            + s" [${orderedSetAsRealSet.size}] (isAddClosed is true)"
                          )
                        )
                    }
                    else
                      postAddFailure(new IllegalStateException("orderedSet must not be empty (isAddClosed is true)"))
                  else
                  if (memberNamesLowerCaseAsSetInternal.size != memberSealedTraitDescendantNamesFmLowerCase.size)
                    Success(())
                  else {
                    val inOnlyMemberNamesLowerCaseAsSetInternal =
                      memberNamesLowerCaseAsSetInternal.--(memberSealedTraitDescendantNamesFmLowerCase)
                    val inOnlyMemberSealedTraitDescendantNamesFm =
                      memberSealedTraitDescendantNamesFmLowerCase.--(memberNamesLowerCaseAsSetInternal)
                    postAddFailure(
                      new IllegalStateException(
                          s"while being the same size [${memberNamesLowerCaseAsSetInternal.size}], both"
                        + s" memberNamesLowerCaseAsSetInternal"
                        + s" [${memberNamesLowerCaseAsSetInternal.mkString(",")}] and"
                        + s" memberSealedTraitDescendantNamesFmLowerCase"
                        + s" [${memberSealedTraitDescendantNamesFmLowerCase.mkString(",")}] are not equal (not sure how"
                        + s" this is even possible)"
                        + ( if (inOnlyMemberNamesLowerCaseAsSetInternal.nonEmpty)
                                s" - memberNamesLowerCaseAsSetInternal contains values"
                              + s" [${inOnlyMemberNamesLowerCaseAsSetInternal.mkString(",")}] not in"
                              + s" memberSealedTraitDescendantNamesFmLowerCase"
                            else ""
                          )
                        + ( if (inOnlyMemberSealedTraitDescendantNamesFm.nonEmpty)
                                s" - memberSealedTraitDescendantNamesFmLowerCase contains values"
                              + s" [${inOnlyMemberSealedTraitDescendantNamesFm.mkString(",")}] not in"
                              + s" memberNamesLowerCaseAsSetInternal"
                            else ""
                          )
                      )
                    )
                  }
                }
                else
                  postAddFailure(
                    new IllegalArgumentException(
                        s"attempting to add member with name [${member.name}] which was previously "
                      + s"(case insensitively) added [${member.name.toLowerCase}]"
                    )
                  )
              else
                postAddFailure(
                  new IllegalArgumentException(
                      s"member [${member.name}] (with nameSpace [${member.nameSpace.getOrElse("<None>")}]) must be declared"
                    + s" inside of the derived Enumeration's nameSpace [${className.fullName}]"
                  )
                )
            }
            )
          else
            postAddFailure(
              new IllegalStateException(s"attempting to add member [${member.name}] after isAddClosed was set true")
            )
      }
    }
  }

  private lazy val membersInternal: Try[List[Member]] = {
    this.synchronized {
      addFailure match {
        case Some(throwable) => Failure(throwable)
        case None =>
          if (isAddClosed)
            Success(orderedSet)
          else
          if (orderedSet.nonEmpty) {
            val orderedSetAsRealSet = orderedSet.toSet
            if (orderedSet.size == orderedSetAsRealSet.size) {
              val orderedSetNamesLowerCase = orderedSetAsRealSet.map(_.name.toLowerCase)
              memberSealedTraitDescendantNames.flatMap(memberSealedTraitDescendantNamesFm => {
                val memberSealedTraitDescendantNamesFmLowerCase = memberSealedTraitDescendantNamesFm.map(_.toLowerCase)
                val inOnlyOrderedSet = orderedSetNamesLowerCase.--(memberSealedTraitDescendantNamesFmLowerCase)
                val inOnlyMemberSealedTraitDescendantNamesFm =
                  memberSealedTraitDescendantNamesFmLowerCase.--(orderedSetNamesLowerCase)
                Failure(
                  new IllegalStateException(
                      s"orderedSetNamesLowerCase [${orderedSetNamesLowerCase.mkString(",")}] is not equal to "
                    + s"memberSealedTraitDescendantNamesFmLowerCase"
                    + s"[${memberSealedTraitDescendantNamesFmLowerCase.mkString(",")}]"
                    + ( if (inOnlyOrderedSet.nonEmpty)
                            s" - orderedSetNamesLowerCase contains values [${inOnlyOrderedSet.mkString(",")}] not in"
                          + s" memberSealedTraitDescendantNamesFmLowerCase"
                        else ""
                      )
                    + ( if (inOnlyMemberSealedTraitDescendantNamesFm.nonEmpty)
                            s" - memberSealedTraitDescendantNamesFmLowerCase contains values"
                          + s" [${inOnlyMemberSealedTraitDescendantNamesFm.mkString(",")}] not in orderedSetNamesLowerCase"
                        else ""
                      )
                  )
                )
              })
            }
            else
              Failure(
                new IllegalStateException(
                    s"orderedSet.size [${orderedSet.size}] must be equal to orderedSetAsRealSet.size"
                  + s" [${orderedSetAsRealSet.size}] (isAddClosed is true)"
                )
              )
          }
          else
            Failure(new IllegalStateException("orderedSet must not be empty"))
      }
    }
  }

  /**
   *
   */
  final lazy val ordinalRange: Range =
    ordinalRangeLift match {
      case Success(range) => range
      case Failure(exception) => throw exception
    }

  /**
   *
   */
  final lazy val ordinalRangeLift: Try[Range] =
    if (ordinalStep != 0)
      membersLift.flatMap(membersFm =>
        Success(ordinalStart.until(ordinalStart + (membersFm.size * ordinalStep), ordinalStep))
      )
    else
      Failure(new IllegalStateException(s"ordinalStep must not be equal to 0"))

  /**
   *
   */
  final lazy val ordinalByMember: Map[Member, Int] =
    ordinalByMemberLift match {
      case Success(map) => map
      case Failure(exception) => throw exception
    }

  /**
   *
   */
  final lazy val ordinalByMemberLift: Try[Map[Member, Int]] =
    membersInternal.flatMap(
      membersInternalFm => {
        ordinalRangeLift.flatMap(
          ordinalRangeFm =>
            Success(membersInternalFm.zip(ordinalRangeFm).toMap)
        )
      }
    )

  /**
   *
   */
  final lazy val memberByOrdinal: Map[Int, Member] =
    memberByOrdinalLift match {
      case Success(map) => map
      case Failure(exception) => throw exception
    }

  /**
   *
   */
  final lazy val memberByOrdinalLift: Try[Map[Int, Member]] =
    ordinalByMemberLift.flatMap(
      ordinalByMemberFm => {
        val ordinalAndMemberPairs =
          ordinalByMemberFm.map(
            memberAndOrdinalPair =>
              (memberAndOrdinalPair._2, memberAndOrdinalPair._1)
          )
        Success(ordinalAndMemberPairs.toMap)
      }
    )

  /**
   *
   */
  private lazy val memberNames: String =
    membersLift match {
      case Success(membersLiftGet) => membersLiftGet.map(_.name).mkString(",")
      case Failure(exception) => exception.getMessage
    }

  /**
   *
   *  @param name
   *  @return
   */
  final def memberByName(name: String): Member =
    memberByNameLift(name) match {
      case Success(member) => member
      case Failure(exception) => throw exception
    }

  /**
   *
   *  @param name
   *  @return
   */
  final def memberByNameLift(name: String): Try[Member] =
    memberByNameUpperCaseLift.flatMap(
      map =>
        map.get(name.toUpperCase) match {
          case Some(member) =>
            Success(member)
          case None =>
            Failure(new IllegalArgumentException(s"name [$name.toUpperCase] not found in member names [$memberNames]"))
        }
    )

  /**
   *
   */
  final lazy val memberByNameUpperCase: Map[String, Member] =
    memberByNameUpperCaseLift match {
      case Success(map) => map
      case Failure(exception) => throw exception
    }

  /**
   *
   */
  final lazy val memberByNameUpperCaseLift: Try[Map[String, Member]] =
    membersInternal.flatMap(
      membersInternalFm => {
        val nameUpperCaseAndMemberPairs =
          membersInternalFm.map(
            member =>
              (member.name.toUpperCase, member)
          )
        Success(nameUpperCaseAndMemberPairs.toMap)
      }
    )

  /**
   *
   *  @return
   */
  final def members: List[Member] =
    membersLift match {
      case Success(list) => list
      case Failure(exception) => throw exception
    }

  /**
   *
   *  @return
   */
  final def membersLift: Try[List[Member]] = membersInternal

  /**
   *
   */
  final lazy val status: Try[Unit] =
    membersLift.flatMap(_ => Success(()))

  /**
   *
   *  @return
   */
  protected def typeTagMember: TypeTag[_]

  /**
   *
   *  @return
   */
  protected def orderedSet: List[Member]

  /**
   *
   *  @return
   */
  protected def ordinalStart = 0

  /**
   *
   *  @return
   */
  protected def ordinalStep = 1 //requirement: cannot equal 0

  /**
   *
   */
  protected trait MemberBase {
    /**
     *
     */
    self: Member =>

    /**
     *
     *  @return
     */
    private def readResolve(): Object =
      memberByName(name)

    /**
     *
     */
    final val (nameSpace: Option[String], name: String) = {
      val className = Class_.Name(getClass)
      (className.nameSpace, className.simpleName)
    }

    /**
     *
     */
    final lazy val ordinal: Int =
      ordinalLift match {
        case Success(int) => int
        case Failure(exception) => throw exception
      }

    /**
     *
     */
    final lazy val ordinalLift: Try[Int] =
      Enumeration.this.status match {
        case Success(_) =>
          ordinalByMemberLift match {
            case Success(map) => Success(map(this))
            case Failure(exception) => Failure(exception)
          }
        case Failure(exception) =>
          Failure(
            new IllegalStateException(s"access denied - Enumeration.status is a Failure - ${exception.getMessage}")
          )
      }

    /**
     *
     */
    final val status: Try[Unit] = add(this)
  }
}
/*
This Scala file is free software: you can redistribute it and/or
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
