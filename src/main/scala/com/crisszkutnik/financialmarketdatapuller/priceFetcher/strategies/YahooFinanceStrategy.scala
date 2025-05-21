package com.crisszkutnik.financialmarketdatapuller.priceFetcher.strategies

import com.crisszkutnik.financialmarketdatapuller.priceFetcher.exceptions.TickerNotFoundException
import com.crisszkutnik.financialmarketdatapuller.priceFetcher.strategies.YahooFinanceStrategy.yfinanceRequestsFailedCounter
import com.crisszkutnik.financialmarketdatapuller.priceFetcher.{AssetType, Currency, Market, Source, TickerPriceInfo}
import com.typesafe.scalalogging.Logger
import io.prometheus.metrics.core.metrics.Counter
import sttp.client4.quick.*
import sttp.model.StatusCode

import scala.util.Try

class YahooFinanceStrategy(
  private val logger: Logger = Logger[YahooFinanceStrategy]
) extends PriceFetcher:
  private val headers: Map[String, String] = Map(
    "User-Agent" -> "PostmanRuntime/7.43.4",
    "Accept" -> "*/*",
    "Accept-Encoding" -> "gzip, deflate, br",
    "Cache-Control" -> "max-age=0"
  )

  val source: Source = Source.YAHOOFINANCE

  def canHandle(market: Market, ticker: String, assetType: AssetType): Boolean =
    assetType == AssetType.STOCK

  def canHandle(market: Market, ticker: String): Boolean = true

  def getTickerPriceInfo(market: Market, ticker: String, assetType: AssetType): Try[TickerPriceInfo] =
    getTickerPriceInfo(market, ticker)

  def getTickerPriceInfo(market: Market, ticker: String): Try[TickerPriceInfo] =
    Try {
      val actualTicker = transformTicker(market, ticker)
      val (price, currency, change, changePct) = retrieveData(actualTicker)

      TickerPriceInfo(price, change, changePct, 1, currency)
    }

  private def retrieveDocument(ticker: String) =
    try {
      val response = quickRequest
        .get(uri"https://query1.finance.yahoo.com/v8/finance/chart/${ticker}")
        .headers(headers)
        .send()

      if response.code != StatusCode.Ok then {
        yfinanceRequestsFailedCounter.labelValues(ticker, response.code.toString).inc()
        throw Exception("Ticker not found")
      }

      ujson.read(response.body)
    } catch
      case e: Throwable =>
        logger.error(s"Failure retrieving document for ticker ${ticker}")
        logger.error(e.toString)
        throw TickerNotFoundException(source, ticker)

  private def retrieveData(ticker: String) =
    val json = retrieveDocument(ticker)

    // TODO: Maybe try to use Circe for this?
    // chart.result[0].meta.regularMarketPrice
    val value = (((json("chart").obj)("result").arr.head)("meta").obj)("regularMarketPrice").num

    // chart.result[0].meta.currency
    val currency = (((json("chart").obj)("result").arr.head)("meta").obj)("currency").str
    // Some tickers have a blank currency field. Assume it is USD
    // TODO: Make currency an optional field
    val enumVal = Try(Currency.valueOf(currency)).getOrElse(Currency.USD)

    val previousClose = (((json("chart").obj)("result").arr.head)("meta").obj)("previousClose").num

    val change = value - previousClose;
    val changePct = (value - previousClose) / previousClose

    (value, enumVal, change, changePct.toFloat)


  private def transformTicker(market: Market, ticker: String) =
    market match
      case Market.BCBA => s"${ticker}.BA"
      case _ => ticker

object YahooFinanceStrategy {
  private val yfinanceRequestsFailedCounter = Counter
    .builder()
    .name("yahoo_finance_requests_failed")
    .help("Amount of Yahoo Finance requests that fail")
    .labelNames("ticker", "statusCode")
    .register()
}