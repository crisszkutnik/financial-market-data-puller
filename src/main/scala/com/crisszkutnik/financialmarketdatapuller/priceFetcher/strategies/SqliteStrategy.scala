package com.crisszkutnik.financialmarketdatapuller.priceFetcher.strategies
import com.crisszkutnik.financialmarketdatapuller.Config
import com.crisszkutnik.financialmarketdatapuller.priceFetcher.exceptions.TickerNotFoundException
import com.crisszkutnik.financialmarketdatapuller.priceFetcher.strategies.SqliteStrategy.getInfo
import com.crisszkutnik.financialmarketdatapuller.priceFetcher.{AssetType, Currency, Market, Source, TickerPriceInfo}

import java.sql.{Connection, DriverManager, ResultSet, Statement}
import scala.util.{Failure, Success, Try, Using}

/*
  Really simple and dumb implementation

  If more than 1 pod is used, this cache will be much less effective as it is
  one SQLite database per pod
*/

class SqliteStrategy extends PriceFetcher:
  val source: Source = Source.SQLITE

  def canHandle(market: Market, ticker: String): Boolean = true
  def canHandle(market: Market, ticker: String, assetType: AssetType): Boolean = true

  def getTickerPriceInfo(market: Market, ticker: String, assetType: AssetType): Try[TickerPriceInfo] =
    customGetTickerPrice(market, ticker, Some(assetType))

  def getTickerPriceInfo(market: Market, ticker: String): Try[TickerPriceInfo] =
    customGetTickerPrice(market, ticker, None)

  private def customGetTickerPrice(market: Market, ticker: String, assetType: Option[AssetType]) =
    val res = getInfo(market, ticker, assetType)

    res match
      case Success(None) => Failure(TickerNotFoundException(source, market, ticker))
      case Success(Some(v)) => Success(v)
      case Failure(err) => Failure(err)

object SqliteStrategy:
  private val connection: Connection = DriverManager.getConnection("jdbc:sqlite:data.db")
  init()

  private def init() =
    val stmt = connection.createStatement()
    stmt.executeUpdate(
      """
        |CREATE TABLE IF NOT EXISTS prices(
        |ticker TEXT NOT NULL,
        |market TEXT NOT NULL,
        |assetType TEXT,
        |value REAL NOT NULL,
        |unitsForTickerPrice INTEGER NOT NULL,
        |currency TEXT NOT NULL,
        |updatedAt INTEGER NOT NULL,
        |PRIMARY KEY(ticker, market, assetType)
        |)
        |""".stripMargin)

    stmt.close()


  def insertInfo(market: Market, ticker: String, assetType: Option[AssetType], tpi: TickerPriceInfo): Try[Unit] =
    Using(connection.createStatement()) { stmt =>
      val mkt = market.toString
      val parsedAssetType = assetType match {
        case Some(at) => s"'$at'"
        case None => s"NULL"
      }
      val currency = tpi.currency.toString
      val timestamp = System.currentTimeMillis()
      val value = tpi.value
      val uftp = tpi.unitsForTickerPrice

      val query = s"INSERT INTO prices VALUES ('$ticker', '$mkt', $parsedAssetType, $value, $uftp, '$currency', $timestamp)"

      stmt.executeUpdate(query)
      ()
    }

  def getInfo(market: Market, ticker: String, assetType: Option[AssetType]): Try[Option[TickerPriceInfo]] =
    Using(connection.createStatement()) { stmt =>
      val mkt = market.toString
      val fullAssetTypeStatement = preparseAssetTypeStatement(assetType)

      val query = s"SELECT * FROM prices WHERE market='$mkt' AND ticker='$ticker' AND $fullAssetTypeStatement LIMIT 1"

      val rs = stmt.executeQuery(query)

      /*
      Cursor is by default before the first row, so move it to the first
      (and only) row. Will throw if no row
      */
      rs.next()

      // stmt.close()

      if entryExpired(rs) then
        deleteEntry(stmt, rs)
        None
      else
        Some(
          TickerPriceInfo(
            rs.getDouble("value"),
            rs.getInt("unitsForTickerPrice"),
            Currency.valueOf(rs.getString("currency"))
          )
        )
    }

  private def deleteEntry(stmt: Statement, entry: ResultSet): Unit =
    val ticker = entry.getString("ticker")
    val mkt = entry.getString("market")
    val assetType = entry.getString("assetType")

    val fullAssetTypeStatement = preparseAssetTypeStatement(Option(AssetType.valueOf(assetType)))

    stmt.executeUpdate(
      s"DELETE FROM prices WHERE market='$mkt' AND ticker='$ticker' AND $fullAssetTypeStatement"
    )
    ()

  private def entryExpired(entry: ResultSet) =
    val timestamp = entry.getLong("updatedAt")
    val currentTime = System.currentTimeMillis()

    currentTime >= timestamp + Config.SQLITE_CACHE_EXPIRE_TIME_MS

  // Will include ''
  private def preparseAssetTypeStatement(assetType: Option[AssetType]) =
    assetType match
      case Some(v) =>
        val str = v.toString
        s"assetType='$str'"
      case None => s"assetType IS NULL"