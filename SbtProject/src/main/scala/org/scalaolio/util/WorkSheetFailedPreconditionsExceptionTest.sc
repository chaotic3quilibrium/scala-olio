import org.scalaolio.util.FailedPreconditionsException
import org.scalaolio.util.FailedPreconditionsException.FailedPreconditionMustBeNonEmptyList

val fp1 = FailedPreconditionMustBeNonEmptyList("test message 2")
val fps1 = FailedPreconditionsException(fp1)
val fps2 = FailedPreconditionsException(List(fp1, fp1))
val fps3 = FailedPreconditionsException(Nil)