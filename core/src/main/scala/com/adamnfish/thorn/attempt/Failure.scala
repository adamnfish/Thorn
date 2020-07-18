package com.adamnfish.thorn.attempt


case class FailedAttempt(failures: List[Failure]) {
  def statusCode: Int = failures.map(_.statusCode).max
  def logString: String = failures.map { failure =>
    List(
      Some(failure.message),
      failure.context.map(c => s"context: $c"),
      failure.exception.map(e => "err: " + e.getStackTrace.mkString("\n")),
      failure.exception.flatMap(e => Option(e.getCause).map(c => "caused by: " + c.getStackTrace.mkString("\n")))
    ).flatten.mkString(" | ")
  }.mkString(", ")
}
object FailedAttempt {
  def apply(error: Failure): FailedAttempt = {
    FailedAttempt(List(error))
  }
  def apply(errors: Seq[Failure]): FailedAttempt = {
    FailedAttempt(errors.toList)
  }
}

case class Failure(
  message: String,
  friendlyMessage: String,
  statusCode: Int,
  context: Option[String] = None,
  exception: Option[Throwable] = None
) {
  def asAttempt = FailedAttempt(this)
}
