package com.crisszkutnik.financialmarketdatapuller.priceFetcher.strategies

import com.crisszkutnik.financialmarketdatapuller.priceFetcher.exceptions.TickerNotFoundException
import com.crisszkutnik.financialmarketdatapuller.priceFetcher.{AssetType, Currency, Market, Source, TickerPriceInfo}
import com.typesafe.scalalogging.Logger
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import sttp.client4.quick.*
import sttp.client4.Response
import sttp.model.Uri

import scala.util.Try

class IolStrategy(private val logger: Logger = Logger[IolStrategy]) extends PriceFetcher:
  val source: Source = Source.IOL

  def canHandle(market: Market, ticker: String, assetType: AssetType): Boolean =
    assetType match
      case AssetType.STOCK | AssetType.BOND => true
      case _ => false

  def canHandle(market: Market, ticker: String): Boolean = true

  // We will assume it is a stock
  def getTickerPriceInfo(market: Market, ticker: String): Try[TickerPriceInfo] = {
    Try {
      val doc = getDocument(transformMarket(market), ticker)

      if !foundTicker(doc) then
        throw TickerNotFoundException(source, market, ticker)

      val assetType = if (isArgentinaBond(doc)) AssetType.BOND else AssetType.STOCK

      getTickerPriceInfo(market, ticker, assetType, Some(doc)).get
    }
  }

  def getTickerPriceInfo(market: Market, ticker: String, assetType: AssetType): Try[TickerPriceInfo] =
    getTickerPriceInfo(market, ticker, assetType, None)

  private def getTickerPriceInfo(market: Market, ticker: String, assetType: AssetType, receivedDoc: Option[Document]): Try[TickerPriceInfo] =
    Try {
      val doc = receivedDoc.getOrElse(getDocument(transformMarket(market), ticker))

      if foundTicker(doc) then
        TickerPriceInfo(
          getPrice(doc),
          getUnitsForGivenPrice(assetType),
          getCurrency(doc)
        )
      else {
        logger.error(s"Could not find info for $ticker, $market, $assetType")
        throw TickerNotFoundException(source, market, ticker)
      }
    }

  private def foundTicker(doc: Document): Boolean =
    Option(doc.selectFirst("#error")).isEmpty

  private def getDocument(market: String, ticker: String): Document =
    try {
      val url: Uri = uri"https://iol.invertironline.com/titulo/cotizacion/$market/$ticker"
      val response: Response[String] = quickRequest.get(url).send()
      Jsoup.parse(response.body)
    } catch
      case e: Exception =>
        throw TickerNotFoundException(source, market, ticker)

  private def isArgentinaBond(doc: Document): Boolean = {
    val txt = doc
      .selectFirst("h1.header-title")
      .ownText()
      .toLowerCase

    txt.contains("bono") && txt.contains("argentina")
  }

  private def getPrice(doc: Document): Double =
    doc
      .selectFirst("#IdTitulo [data-field=\"UltimoPrecio\"]")
      .ownText()
      .replace(".", "")
      .replace(",", ".")
      .toDouble
  
  private def getCurrency(doc: Document): Currency =
    doc.selectFirst("#IdTitulo span").ownText() match
      case "US$" => Currency.USD
      case _ => Currency.ARS

  private def getUnitsForGivenPrice(assetType: AssetType) =
    assetType match
      case AssetType.BOND => 100
      case _ => 1

  private def transformMarket(market: Market): String =
    market match
      case Market.NYSEARCA => Market.NYSE.toString
      case other => other.toString