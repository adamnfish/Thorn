package com.adamnfish.skull.logic

import com.adamnfish.skull.models.{Message, PlayerAddress, Response, Status, Welcome}


object Responses {
  def tbd[A <: Message](): Response[A] = {
    ???
  }

  def empty: Response[Nothing] = {
    Response(None, Map.empty)
  }

  def ok(): Response[Status] = {
    Response(
      Some(Status("ok")),
      Map.empty
    )
  }
}
