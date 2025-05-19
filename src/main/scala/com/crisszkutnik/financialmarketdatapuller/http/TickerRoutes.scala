package com.crisszkutnik.financialmarketdatapuller.http

import io.circe.*
import org.http4s.*
import io.circe.syntax.*
import io.circe.generic.auto.*
import com.crisszkutnik.financialmarketdatapuller.priceFetcher.exceptions.TickerNotFoundException
import com.crisszkutnik.financialmarketdatapuller.priceFetcher.{AssetType, Market, PriceResponse}
import org.http4s.dsl.Http4sDsl
import cats.effect.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.prometheus.metrics.core.metrics.Counter
import org.http4s.circe.*

import scala.util.{Failure, Success, Try}

case class BasicTickerBody(market: String, ticker: String, assetType: Option[String])

object TickerRoutes:
  private lazy val counter = Counter
    .builder()
    .name("basic_ticker_requests")
    .help("Data of requests made to basic ticker endpoints")
    .labelNames("status")
    .register()

  private lazy val OK = "ok"
  private lazy val BAD_REQUEST = "bad_request"
  private lazy val INTERNAL_ERROR = "internal_server_error"

  def tickerRoutes[F[_]: Concurrent](controller: TickerController[F]): HttpRoutes[F] =
    val dsl = new Http4sDsl[F]{}
    import dsl.*

    implicit val basicTickerBodyReader: EntityDecoder[F, BasicTickerBody] = jsonOf[F, BasicTickerBody]

    HttpRoutes.of[F] {
      case GET -> Root / "ticker" / "basic" / market / ticker / assetType =>
        getBasicTickerValue(dsl, controller, market, ticker, Option(assetType))

      case req @ POST -> Root / "ticker" / "basic" =>
        for {
          body <- req.as[BasicTickerBody]
          resp <- getBasicTickerValue(dsl, controller, body)
        } yield resp
    }

  private def getBasicTickerValue[F[_]: Concurrent](dsl: Http4sDsl[F], controller: TickerController[F], market: String, ticker: String, assetType: Option[String]): F[Response[F]] =
    import dsl.*

    val parsedMarket = Try(Market.valueOf(market));
    val parsedAssetType = assetType.map(AssetType.valueOf)

    (parsedMarket, parsedAssetType) match
      case (Success(m), at) =>
        controller.getBasicTickerValue(m, ticker, at) match
          case Success(res) =>
            counter.labelValues(OK).inc()
            Ok(res.asJson)
          case Failure(e: TickerNotFoundException) =>
            counter.labelValues(BAD_REQUEST).inc()
            BadRequest(e.getMessage)
          case Failure(e: Throwable) =>
            counter.labelValues(INTERNAL_ERROR).inc()
            InternalServerError()
      case _ =>
        counter.labelValues(BAD_REQUEST).inc()
        BadRequest()

  private def getBasicTickerValue[F[_]: Concurrent](dsl: Http4sDsl[F], controller: TickerController[F], body: BasicTickerBody): F[Response[F]] =
    getBasicTickerValue(dsl, controller, body.market, body.ticker, body.assetType)