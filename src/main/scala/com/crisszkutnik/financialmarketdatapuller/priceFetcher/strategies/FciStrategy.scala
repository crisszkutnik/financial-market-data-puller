package com.crisszkutnik.financialmarketdatapuller.priceFetcher.strategies

import FciStrategy.BASE_PATH
import com.crisszkutnik.financialmarketdatapuller.priceFetcher.exceptions.TickerNotFoundException
import com.crisszkutnik.financialmarketdatapuller.priceFetcher.{AssetType, Currency, Market, Source, TickerPriceInfo}
import com.typesafe.scalalogging.Logger
import org.apache.commons.io.FileUtils
import org.apache.poi.ss.usermodel.WorkbookFactory
import sttp.client4.UriContext

import java.io.{File, FileOutputStream}
import java.nio.channels.Channels
import java.nio.file.{FileAlreadyExistsException, Files, Path}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Date
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}


class FciStrategy(
 private val logger: Logger = Logger[FciStrategy]
) extends PriceFetcher:
  val source: Source = Source.CAFCI
  
  def canHandle(market: Market, ticker: String, assetType: AssetType): Boolean =
    (market, ticker, assetType) match
      case (Market.BCBA, _, AssetType.MUTUAL_FUND) => true
      case _ => false

  def canHandle(market: Market, ticker: String): Boolean =
    market match
      case Market.BCBA => true
      case _ => false

  private def getFilePath: Path =
    val currentTime = LocalDateTime.now()
    val yesterdayTime = LocalDateTime.now().minusDays(1)
    val fileNameFormatter = DateTimeFormatter.ofPattern("YYYY-MM-dd")
    val hourFormatter = DateTimeFormatter.ofPattern("HH")

    val formattedYesterdayFileName = yesterdayTime.format(fileNameFormatter)
    val formattedFileName = currentTime.format(fileNameFormatter)
    val formattedHour = currentTime.format(hourFormatter)

    val hour = Integer.parseInt(formattedHour)

    val fileName =
      hour match
        case _ if hour >= 19 => formattedFileName
        case _ => formattedYesterdayFileName

    Path.of(BASE_PATH + s"/${fileName}.xlsx")

  private def downloadFile(path: Path): File =
    logger.info("Downloading updated FCI file")
    val timestamp = Date().getTime
    val uri = uri"https://api.cafci.org.ar/pb_get?d=${timestamp}"
    val readableByteChannel = Channels.newChannel(uri.toJavaUri.toURL.openStream())

    val file = path.toFile

    val fileOutputStream = FileOutputStream(file)
    val fileChannel = fileOutputStream.getChannel

    fileChannel.transferFrom(readableByteChannel, 0, Long.MaxValue)
    fileChannel.close()
    readableByteChannel.close()
    fileOutputStream.close()

    file

  private def createDirectory(): Unit =
    try {
      val p = Path.of(BASE_PATH)
      val _ = Files.createDirectory(p)
      logger.error("Directory created")
    } catch
      case e: FileAlreadyExistsException =>
        logger.error("Directory already exists. Skipping.")

  private def retrieveFile(): Try[File] =
    Try {
      val filePath = getFilePath
      val exists = Files.exists(filePath)

      if exists then
        filePath.toFile
      else
        FileUtils.cleanDirectory(Path.of(BASE_PATH).toFile)
        downloadFile(filePath)
    }

  private def readFromSpreadsheet(fciName: String, file: File): Option[TickerPriceInfo] =
    val wb =  WorkbookFactory.create(file)
    val sheet = wb.getSheetAt(0)
    val it = sheet.iterator().asScala

    val row = it.find(r => {
      val cell = r.getCell(0)
      cell.getStringCellValue.strip() == fciName
    })

    if row.isEmpty then
      None
    else
      val price = row.get.getCell(5).getNumericCellValue
      val currency = row.get.getCell(1).getStringCellValue

      Some(
        TickerPriceInfo(
          price,
          1000,
          Currency.valueOf(currency)
        )
      )


  private def readValue(fciName: String): Try[TickerPriceInfo] =
    retrieveFile() match
      case Success(f) =>
        val info = readFromSpreadsheet(fciName, f)

        if info.isEmpty then
          logger.error(s"Could not find info for $fciName")
          Failure(TickerNotFoundException(source, fciName))
        else
          Success(info.get)
      case Failure(e: Throwable) =>
        logger.error("Failed to retrieve file")
        logger.error(e.toString)
        Failure(e)

  def getTickerPriceInfo(market: Market, ticker: String, assetType: AssetType): Try[TickerPriceInfo] =
    getTickerPriceInfo(market, ticker)

  def getTickerPriceInfo(market: Market, ticker: String): Try[TickerPriceInfo] =
    Try {
      createDirectory()
      readValue(ticker).get
    }

object FciStrategy:
  private val BASE_PATH = "./fci_files"