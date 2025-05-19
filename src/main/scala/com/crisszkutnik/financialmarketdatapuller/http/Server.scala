package com.crisszkutnik.financialmarketdatapuller.http

import cats.effect.Async
import cats.syntax.all._
import com.comcast.ip4s.*
import fs2.io.net.Network
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.middleware.Logger

object Server:

  def run[F[_]: Async: Network]: F[Nothing] = {
    for {
      client <- EmberClientBuilder.default[F].build
      helloWorldAlg = HelloWorld.impl[F]
      tickerController = TickerController.impl[F]

      // Combine Service Routes into an HttpApp.
      // Can also be done via a Router if you
      // want to extract segments not checked
      // in the underlying routes.
      httpApp = (
        HelloWorldRoutes.helloWorldRoutes[F](helloWorldAlg) <+>
        TickerRoutes.tickerRoutes[F](tickerController)
      ).orNotFound

      // With Middlewares in place
      finalHttpApp = Logger.httpApp(true, true)(httpApp)

      _ <- 
        EmberServerBuilder.default[F]
          .withHost(ipv4"0.0.0.0")
          .withPort(port"4000")
          .withHttpApp(finalHttpApp)
          .build
    } yield ()
  }.useForever
