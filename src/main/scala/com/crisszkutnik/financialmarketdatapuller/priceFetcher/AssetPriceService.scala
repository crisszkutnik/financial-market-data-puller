package com.crisszkutnik.financialmarketdatapuller.priceFetcher

import com.crisszkutnik.financialmarketdatapuller.priceFetcher.exceptions.TickerNotFoundException
import com.crisszkutnik.financialmarketdatapuller.priceFetcher.strategies.{FciStrategy, IolStrategy, PriceFetcher, YahooFinanceStrategy}
import com.typesafe.scalalogging.Logger

import scala.util.{Failure, Success, Try}

case class FetcherEvaluationStatus(
  tickerInfo: Option[TickerPriceInfo] = None,
  source: Option[Source] = None,
  isNotFound: Boolean = false
)

class AssetPriceService(
  private val fetchers: List[PriceFetcher] = List(
    YahooFinanceStrategy(),
    IolStrategy(),
    FciStrategy()
  ),
  private val logger: Logger = Logger[AssetPriceService]
):
  def getValue(market: Market, ticker: String, assetType: AssetType): Try[PriceResponse] =
    val validFetchers = fetchers.filter(_.canHandle(market, ticker, assetType))

    val foldFn = (acc: FetcherEvaluationStatus, fetcher: PriceFetcher) => {
      acc match
        case FetcherEvaluationStatus(Some(_), Some(_), _) => acc
        case _ =>
          fetcher.getTickerPriceInfo(market, ticker, assetType) match
            case Success(info) => FetcherEvaluationStatus(Some(info), Some(fetcher.source), acc.isNotFound)
            case Failure(e: TickerNotFoundException) => FetcherEvaluationStatus(None, None, true)
            case Failure(e: Throwable) =>
              logger.error(e.toString)
              FetcherEvaluationStatus(None, None, false)
    }

    val fetcherStatus = validFetchers.foldLeft(FetcherEvaluationStatus())(foldFn)

    fetcherStatus match
      case FetcherEvaluationStatus(Some(info), Some(source), _) =>
        Success(
          PriceResponse(
            info.value,
            ticker,
            market,
            assetType,
            info.unitsForTickerPrice,
            info.currency,
            source
          )
        )
      case FetcherEvaluationStatus(_, _, true) =>
        Failure(TickerNotFoundException(market, ticker))
      case FetcherEvaluationStatus(_, _, false) =>
        Failure(Exception(s"Critical error when trying to retrieve ticker $market:$ticker"))