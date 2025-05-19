package com.crisszkutnik.financialmarketdatapuller.priceFetcher.strategies

import com.crisszkutnik.financialmarketdatapuller.priceFetcher.exceptions.TickerNotFoundException
import com.crisszkutnik.financialmarketdatapuller.priceFetcher.{AssetType, Currency, Market, Source, TickerPriceInfo}
import com.typesafe.scalalogging.Logger
import sttp.client4.quick.*
import sttp.model.StatusCode

import scala.util.Try

class YahooFinanceStrategy(
  private val logger: Logger = Logger[YahooFinanceStrategy]
) extends PriceFetcher:
  val source: Source = Source.YAHOOFINANCE

  def canHandle(market: Market, ticker: String, assetType: AssetType): Boolean =
    assetType == AssetType.STOCK

  def canHandle(market: Market, ticker: String): Boolean = true

  def getTickerPriceInfo(market: Market, ticker: String, assetType: AssetType): Try[TickerPriceInfo] =
    getTickerPriceInfo(market, ticker)

  def getTickerPriceInfo(market: Market, ticker: String): Try[TickerPriceInfo] =
    Try {
      val actualTicker = transformTicker(market, ticker)
      val (price, currency) = retrieveData(actualTicker)

      TickerPriceInfo(price, 1, currency)
    }

  private def retrieveDocument(ticker: String) =
    try {
      val response = quickRequest
        .get(uri"https://query1.finance.yahoo.com/v8/finance/chart/${ticker}")
        .send()

      if response.code != StatusCode.Ok then
        throw Exception("Ticker not found")

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
    val enumVal = Currency.valueOf(currency)

    (value, enumVal)


  private def transformTicker(market: Market, ticker: String) =
    market match
      case Market.BCBA => s"${ticker}.BA"
      case _ => ticker