package com.crisszkutnik.financialmarketdatapuller.priceFetcher.strategies

import com.crisszkutnik.financialmarketdatapuller.priceFetcher.{AssetType, Market, Source, TickerPriceInfo}

import scala.util.Try

trait PriceFetcher:
  val source: Source;
  def canHandle(market: Market, ticker: String, assetType: AssetType): Boolean
  def canHandle(market: Market, ticker: String): Boolean
  def getTickerPriceInfo(market: Market, ticker: String, assetType: AssetType): Try[TickerPriceInfo]
  def getTickerPriceInfo(market: Market, ticker: String): Try[TickerPriceInfo]