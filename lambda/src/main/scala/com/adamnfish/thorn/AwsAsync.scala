package com.adamnfish.thorn

import com.amazonaws.AmazonWebServiceRequest
import com.amazonaws.handlers.AsyncHandler

import scala.concurrent.{Future, Promise}


private class AwsAsyncPromiseHandler [A, R <: AmazonWebServiceRequest] (promise: Promise[A]) extends AsyncHandler [R, A] {
  def onError(ex: Exception): Unit = promise failure ex
  def onSuccess(r: R, x: A): Unit = promise success x
}

object AwsAsync {
  def awsToScala[A, R <: AmazonWebServiceRequest]
      (fn: Function2[R, AsyncHandler[R, A], java.util.concurrent.Future[A]]): Function1[R, Future[A]] = { req =>

    val p = Promise[A]()
    fn(req, new AwsAsyncPromiseHandler(p))
    p.future
  }
}
