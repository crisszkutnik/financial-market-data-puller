package com.crisszkutnik.financialmarketdatapuller.priceFetcher

case class PriceResponse(
  value: Double,
  ticker: String,
  market: Market,
  assetType: Option[AssetType],
  unitsForTickerPrice: Int,
  currency: Currency,
  source: Source
)

case class TickerPriceInfo(
  value: Double,
  unitsForTickerPrice: Int,
  currency: Currency
)