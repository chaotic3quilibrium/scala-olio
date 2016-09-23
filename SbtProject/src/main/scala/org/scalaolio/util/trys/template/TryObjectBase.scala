/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util.trys.template                        **
**   Name:      TryObjectBase.scala                                     **
**                                                                      **
** Description:                                                         **
**  Encodes the core try/catch execution pathways in the `apply` method **
**  and also captures the companion object singleton common             **
**  functionality                                                       **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util.trys.template

/** The `TryObjectBase` type encodes the core try/catch execution pathways in the `apply` method and also captures the companion object singleton common functionality.
  *
  * @author Jim O'Flaherty
  * @since 2.11
  */
trait TryObjectBase[T <: Throwable] {
  def successT[V]: V => TryBase[T, V]
  def failureT[V]: (T, List[(FailureBase[Throwable, Any], Option[Serializable])]) => TryBase[T, V]

  protected val classT: Class[_] =
    new Throwable().getClass

  protected def isInstanceValid(t: Throwable): Boolean =
    classT.isInstance(t)

  /** Attempts to constructs a descendant of `TryBase[T, V]` using the by-name parameter `vToEvaluate`.
    * - If the execution of `vToEvaluate` completes without throwing an instance of 'java.lang.Throwable':
    *   - An instance of `SuccessBase[T, V]` is returned
    *   - The values of the three optional parameters; `failureEnclosingContext`, `failureTEvent` and `optionThrowableEvent` are ignored
    * - Else
    *   - It can be assumed the execution of `vToEvaluate` has thrown an instance of 'java.lang.Throwable' (the root class of all `throw`able exceptions within the JVM) or one of its descendants.
    *   - If the thrown instance is of type `T` (or any one of its descendants):
    *     - An instance of `FailureBase[T, V]` is created, which can include:
    *       - The optional parameter `failureEnclosingContext` which provides a means of attaching additional local context to the returned `FailureBase[T, V]`.
    *         - For example, it could be used to collect a `List[FailureBase[Throwable, W]\]` arising from the processing of a `List[V]`; i.e. a convenient way to package up all the `List[FailureBase[Throwable, W]\]` and place them into a single summarizing `FailureBase[T, V]` where it contains a reference via field `enclosingContext` which contains the original `List[FailureBase[Throwable, W]\]`.
    *       - The optional implicit parameter `failureTEvent` which provides a means of dynamically executing code after the `FailureBase[T, V]` has been created but before it has been returned.
    *         - For example, it could be used to emit a logging statement, or a logging message to a queue, or push a record into a DB, etc.
    *       - The optional implicit parameter `optionThrowableEvent` is ignored.
    *   - Else
    *     - It can be assumed the thrown instance is not of type `T` (or any one of its descendants).
    *     - If the optional implicit parameter `optionThrowableEvent` is defined as `nonEmpty`:
    *       - If the function `optionThrowableEvent.get._1` (isThrowableEventInstanceValid) returns `true` when passed the throwable instance:
    *         - The function `optionThrowableEvent.get._2` (throwableEvent) which provides a means of dynamically executing code before the throwable is rethrown.
    *           - For example, it could be used to emit a logging statement, or a logging message to a queue, or push a record into a DB, etc.
    *       - Else
    *         - It can be assumed the `java.lang.Throwable` was unable to be successfully captured by the `catch` block.
    *         - Execution aborts and begins to unwind up the call chain until it either reaches another `catch` block or reaches the root causing the thread to halt.
    *     - Else
    *       - It can be assumed the `java.lang.Throwable` was unable to be successfully captured by the `catch` block.
    *       - Execution aborts and begins to unwind up the call chain until it either reaches another `catch` block or reaches the root causing the thread to halt.
    * @author Jim O'Flaherty
    * @since 2.11
    */
  def apply[V](
      vToEvaluate: => V
    , failureEnclosingContext: List[(FailureBase[Throwable, Any], Option[Serializable])] =
        Nil
  ) (
      implicit failureTEvent: FailureBase[T, V] => Unit =
        (_: Any) => ()
    , optionThrowableEvent: Option[(Throwable => Boolean, Throwable => Unit)] =
        None
  ): TryBase[T, V] =
    optionThrowableEvent match {
      case Some((isThrowableEventInstanceValid, throwableEvent)) =>
        try
          successT(vToEvaluate)
        catch {
          case t: Throwable if isInstanceValid(t) =>
            val failureTTemp =
              failureT[V](t.asInstanceOf[T], failureEnclosingContext)
            failureTEvent(failureTTemp.asInstanceOf[FailureBase[T, V]])
            failureTTemp
          case throwable: Throwable if isThrowableEventInstanceValid(throwable) =>
            throwableEvent(throwable)
            throw throwable
        }
      case None =>
        try
          successT(vToEvaluate)
        catch {
          case t: Throwable if isInstanceValid(t) =>
            val failureTTemp =
              failureT[V](t.asInstanceOf[T], failureEnclosingContext)
            failureTEvent(failureTTemp.asInstanceOf[FailureBase[T, V]])
            failureTTemp
        }
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
