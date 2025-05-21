package com.crisszkutnik.financialmarketdatapuller.priceFetcher

case class PriceResponse(
  value: Double,
  change: Double,
  changePct: Float,
  ticker: String,
  market: Market,
  assetType: Option[AssetType],
  unitsForTickerPrice: Int,
  currency: Currency,
  source: Source
)

case class TickerPriceInfo(
  value: Double,
  change: Double,
  changePct: Float,
  unitsForTickerPrice: Int,
  currency: Currency
)