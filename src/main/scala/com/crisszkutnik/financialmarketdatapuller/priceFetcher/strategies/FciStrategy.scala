package com.crisszkutnik.financialmarketdatapuller.priceFetcher.strategies

import com.crisszkutnik.financialmarketdatapuller.priceFetcher.exceptions.TickerNotFoundException
import com.crisszkutnik.financialmarketdatapuller.priceFetcher.{AssetType, Currency, Market, Source, TickerPriceInfo}
import com.typesafe.scalalogging.Logger
import org.apache.poi.ss.usermodel.{Row, Sheet, Workbook, WorkbookFactory}
import cats.effect.IO
import sttp.client4.UriContext

import java.io.{File, FileOutputStream}
import java.nio.channels.Channels
import java.nio.file.{FileAlreadyExistsException, Files, Path}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Date
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/*
* This is probably far from the most functional code ever made in Scala
* and most likely could use a refactor
* */

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

  def getTickerPriceInfo(market: Market, ticker: String, assetType: AssetType): Try[TickerPriceInfo] =
    getTickerPriceInfo(market, ticker)

  def getTickerPriceInfo(market: Market, ticker: String): Try[TickerPriceInfo] =
    Try {
      readValue(ticker).get
    }

  private def readFromSpreadsheet(fciName: String, it: Iterator[Row]): Option[TickerPriceInfo] =
    val row = it.find(r => {
      val cell = r.getCell(0)
      cell.getStringCellValue.strip() == fciName
    })

    if row.isEmpty then
      None
    else
      val price = row.get.getCell(5).getNumericCellValue
      val currency = row.get.getCell(1).getStringCellValue
      val lastPrice = row.get.getCell(6).getNumericCellValue

      val change = price - lastPrice
      val changePct = (price - lastPrice) / lastPrice

      Some(
        TickerPriceInfo(
          price,
          change,
          changePct.toFloat,
          1000,
          Currency.valueOf(currency)
        )
      )


  private def readValue(fciName: String): Try[TickerPriceInfo] =
    FciFileContainer.getIterator match
      case Success(it) =>
        val info = readFromSpreadsheet(fciName, it)

        if info.isEmpty then
          logger.error(s"Could not find info for $fciName")
          Failure(TickerNotFoundException(source, fciName))
        else
          Success(info.get)
      case Failure(e: Throwable) =>
        logger.error("Failed to retrieve file")
        logger.error(e.toString)
        Failure(e)

object FciFileContainer:
  private val logger: Logger = Logger[FciStrategy]
  private val BASE_PATH = "./fci_files"
  private var fileObject: Option[File] = None
  private var lastDownloadedFileDate: Option[String] = None
  private var wb: Option[Workbook] = None
  private var sheet: Option[Sheet] = None

  private def getFileName: String =
    val currentTime = LocalDateTime.now()
    val yesterdayTime = LocalDateTime.now().minusDays(1)
    val fileNameFormatter = DateTimeFormatter.ofPattern("YYYY-MM-dd")
    val hourFormatter = DateTimeFormatter.ofPattern("HH")

    val formattedYesterdayFileName = yesterdayTime.format(fileNameFormatter)
    val formattedFileName = currentTime.format(fileNameFormatter)
    val formattedHour = currentTime.format(hourFormatter)

    val hour = Integer.parseInt(formattedHour)

    hour match
      case _ if hour >= 19 => formattedFileName
      case _ => formattedYesterdayFileName

  private def getFullFilePath(fileName: String): Path =
    Path.of(BASE_PATH + s"/${fileName}.xlsx")

  private def downloadFile(filePath: Path): File =
    logger.info("Downloading updated FCI file")
    val timestamp = Date().getTime
    val uri = uri"https://api.cafci.org.ar/pb_get?d=${timestamp}"
    val readableByteChannel = Channels.newChannel(uri.toJavaUri.toURL.openStream())

    val file = filePath.toFile

    val fileOutputStream = FileOutputStream(file)
    val fileChannel = fileOutputStream.getChannel

    fileChannel.transferFrom(readableByteChannel, 0, Long.MaxValue)
    fileChannel.close()
    readableByteChannel.close()
    fileOutputStream.close()

    file

  private def shouldDownloadNewFile(fileName: String): Boolean = {
    lastDownloadedFileDate.exists(fileName.eq)
  }

  private def createDirectoryIfNeeded(): Unit =
    try {
      val p = Path.of(BASE_PATH)
      val _ = Files.createDirectory(p)
      logger.info("Directory created")
    } catch
      case e: FileAlreadyExistsException =>
        logger.info("Directory already exists. Skipping.")

  private def prepareNewFile(): Unit =
    val newFileName = getFileName
    val newFilePath = getFullFilePath(newFileName)

    createDirectoryIfNeeded()

    val newFile = Files.exists(newFilePath) match {
      case true => newFilePath.toFile
      case false => downloadFile(newFilePath)
    }

    val newWb = WorkbookFactory.create(newFile)
    val newSheet = newWb.getSheetAt(0)

    val oldFile = fileObject
    val oldWb = wb

    // Set new state accordingly
    lastDownloadedFileDate = Some(newFileName)
    fileObject = Some(newFile)
    wb = Some(newWb)
    sheet = Some(newSheet)

    if oldWb.isDefined then {
      val _ = Try {
        wb.get.close()
      }
    }

    /*
    * In theory this could cause a race condition and a request to fail because of the
    * file missing
    *
    * Fixing this will require some kind of sync or mutex lock
    *
    * Does http4s use an event loop like Node? Is it light multi-threading similar to Golang or
    * Java promises? Investigate this in the future. Big TODO here
    * */
    if oldFile.isDefined then {
      val _ = Files.deleteIfExists(oldFile.get.toPath)
    }


  private def prepareNewFileJob(): IO[Unit] = IO {
    prepareNewFile()
  }

  def getIterator: Try[Iterator[Row]] = {
    Try {
      if wb.isEmpty then {
        prepareNewFile()
      }

      if shouldDownloadNewFile(getFileName) then {
        val _ = prepareNewFileJob().start
      }

      sheet.get.iterator().asScala
    }
  }