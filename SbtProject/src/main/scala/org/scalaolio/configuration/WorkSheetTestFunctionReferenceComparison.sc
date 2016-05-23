val x = 1
//
//two concrete methods
object ConcreteMethods1 {
  def intToStringNone(string: String): Option[Int] =
    None

  def intToStringSome1(string: String): Option[Int] =
    Some(1)
}
//def someMethodFailureCompileTime(
//    parseFunctionReference: String => Option[Int]
//): (Boolean, Boolean) = {
//  (
//      parseFunctionReference == ConcreteMethods1.intToStringNone
//    , parseFunctionReference == ConcreteMethods1.intToStringSome1
//    )
//}
def someMethodFailureRuntime(
    parseFunctionReference: String => Option[Int]
): (Boolean, Boolean) = {
  val intToStringNoneFunctionReference: String => Option[Int] =
    ConcreteMethods1.intToStringNone
  val intToStringSome1FunctionReference: String => Option[Int] =
    ConcreteMethods1.intToStringSome1
  (
      parseFunctionReference == intToStringNoneFunctionReference
    , parseFunctionReference == intToStringSome1FunctionReference
  )
}
val someMethodNoneFailureRuntime =
  someMethodFailureRuntime(ConcreteMethods1.intToStringNone)
  //want (true, false), but get (false, false)
val someMethodSome1FailureRuntime =
  someMethodFailureRuntime(ConcreteMethods1.intToStringSome1)
  //want (false, true), but get (false, false)

object ConcreteMethods2 {
  def intToStringNone(string: String): Option[Int] =
    None

  def intToStringSome1(string: String): Option[Int] =
    Some(1)

  val intToStringNoneFunctionReference: String => Option[Int] =
    intToStringNone

  val intToStringSome1FunctionReference: String => Option[Int] =
    intToStringSome1
}
def someMethodSuccess(
    parseFunctionReference: String => Option[Int]
): (Boolean, Boolean) = {
  (
      parseFunctionReference == ConcreteMethods2.intToStringNoneFunctionReference
    , parseFunctionReference == ConcreteMethods2.intToStringSome1FunctionReference
  )
}
val someMethodNoneSuccess =
  someMethodSuccess(ConcreteMethods2.intToStringNoneFunctionReference)
val someMethodSome1Success =
  someMethodSuccess(ConcreteMethods2.intToStringSome1FunctionReference)