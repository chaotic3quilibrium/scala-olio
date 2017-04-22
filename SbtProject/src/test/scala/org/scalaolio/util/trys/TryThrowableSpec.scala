package org.scalaolio.util.trys

import org.scalatest.{Matchers, FlatSpec}

class TryThrowableSpec extends FlatSpec with Matchers {
  "Providing a simple String to TryThrowable" should "returns a SuccessThrowable" in {
    val value =
      "716 Lake Carolyn Pkwy Apt 231, Irving, Tx 75039"
    val tryThrowable =
      TryThrowable(value)
    tryThrowable match {
      case SuccessThrowable(v) =>
        assert(v === value)
      case FailureThrowable(e, _) =>
        fail(s"should not be a Failure - ${e.getMessage}")
    }
  }

  "Providing a simple Exception to TryThrowable" should "returns a FailureThrowable" in {
    val value =
      "716 Lake Carolyn Pkwy Apt 231, Irving, Tx 75039"
    val tryThrowable =
      TryThrowable(value.toInt)
    tryThrowable match {
      case SuccessThrowable(v) =>
        fail(s"should not be a Success [$v]")
      case FailureThrowable(e, _) =>
        assert(e.getClass.getName === "java.lang.NumberFormatException")
        assert(e.getMessage === "For input string: \"716 Lake Carolyn Pkwy Apt 231, Irving, Tx 75039\"")
    }
  }
}
