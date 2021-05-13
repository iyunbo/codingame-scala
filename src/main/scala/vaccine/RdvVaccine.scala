package org.iyunbo.coding
package vaccine

import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.support.ui.{ExpectedConditions, WebDriverWait}
import org.openqa.selenium.{By, _}
import org.scalatest.time.{Seconds, Span}
import org.scalatestplus.selenium.WebBrowser

import scala.collection.mutable
import scala.io.StdIn.readLine
import scala.jdk.CollectionConverters._
import scala.util.Random

object RdvVaccine extends WebBrowser {

  // initialization
  implicit val driver: WebDriver = new ChromeDriver()
  val js: JavascriptExecutor = driver.asInstanceOf[JavascriptExecutor]
  val host = "https://www.doctolib.fr/"

  // behavioural parameters
  val waitForRefresh: Int = 0
  val maxPage: Int = 8
  val verticalStep = 700
  val topPosition = 150
  val bottomPosition = 1600
  val maxCalendarsPerPage = 10
  val roundCount = 24

  // functional parameters
  val region = "paris"
  val maxAvailableDays = 2

  val FIRST_MODERNA = 7005
  val SECOND_MODERNA = 7004

  val FIRST_PFIZER_BIONTECH = 6970
  val SECOND_PFIZER_BIONTECH = 6971
  val THIRD_PFIZER_BIONTECH = 8192

  val FIRST_ASTRA_ZENECA = 7107
  val SECOND_ASTRA_ZENECA = 7108
  val THIRD_ASTRA_ZENECA = 8193

  val JANSSEN = 7945

  val OPTION_TO_GO = "1re injection vaccin COVID-19 (Pfizer-BioNTech)"

  def main(args: Array[String]): Unit = {

    driver.manage().window().maximize()
    implicitlyWait(Span(1.5, Seconds))


    for (round <- 1 to roundCount) findRdvVaccine(round)


    //    driver.close()
  }


  private def findRdvVaccine(round: Int): Unit = {
    log(s"start round $round")
    for (page <- 1 to maxPage) scanPage(page)
  }

  def onPageLoad(page: Int): Unit = {
    log(s"loading page $page")
    log(s"url: ${driver.getCurrentUrl}")
  }

  private def scanPage(page: Int): Unit = {
    onPageLoad(page)
    go to s"$host/vaccination-covid-19/$region?force_max_limit=$maxAvailableDays&page=$page&ref_visit_motive_ids%5B%5D=$FIRST_MODERNA&ref_visit_motive_ids%5B%5D=$FIRST_PFIZER_BIONTECH"
    for (position <- topPosition to bottomPosition by verticalStep) {
      showRdvs(position)
    }
  }

  private def showRdvs(y: Int): Unit = {
    js.executeScript(s"window.scrollBy(0,$y)")
    Thread.sleep(waitForRefresh)

    val rdvs = searchRdvs

    chooseOneRdv(rdvs)
      .fold {
        log("No RDV available")
      } {
        onRdvFound
      }

  }

  private def searchRdvs = {
    driver.findElements(By.xpath("//div[@class='Tappable-inactive availabilities-slot']")).asScala
  }

  def falsePositive(address: String): Boolean = {
    address contains "91200 Athis-Mons"
  }

  private def onRdvFound(rdv: WebElement): Unit = {
    try {
      log(s"available: ${rdv.getText}")
      log(s"found potential RDV on: ${driver.getCurrentUrl}")

      log(s"chose the RDV at location ${rdv.getLocation}")
      val wait = new WebDriverWait(driver, 6)
      wait.until(ExpectedConditions.elementToBeClickable(rdv))
      rdv.click()

      log(s"this RDV looks promising: ${driver.getCurrentUrl}")
      val location = driver.findElement(By.xpath(s"//div[@class='dl-text dl-text-body dl-text-regular dl-text-s']"))
      log(s"the adress:\n${location.getText}")
      if (falsePositive(location.getText)) {
        log(s"this vacine center is known to be unavailable, skipping")
        // we don't come back to last page, otherwise we repeat the same RDV
        return
      }
      Beep.run()

      val opt = driver.findElement(By.xpath(s"//select[@class='dl-select form-control dl-select-block booking-compact-select']/option[text()='$OPTION_TO_GO']"))
      log(s"select option ${opt.getLocation}")
      opt.click()

      log(s"looking for available RDV of first injection")
      var rdvs = searchRdvs
      selectRdvs(rdvs)

      if (rdvs.nonEmpty) {
        log(s"looking for available RDV of second injection")
        rdvs = searchRdvs
        selectRdvs(rdvs)
      }

      log(s"Done")

      pause
    } catch {
      case e: ElementClickInterceptedException => e.printStackTrace()
      case e: StaleElementReferenceException => e.printStackTrace()
      case e: Exception => e.printStackTrace()
    }
    goBack()
  }

  private def pause = {
    readLine("Enter to continue")
  }

  private def selectRdvs(rdvs: mutable.Buffer[WebElement]): Unit = {
    if (rdvs.nonEmpty) {
      log(s"found ${rdvs.size} RDVs")

      val rdv = chooseOneRdv(rdvs)

      rdv.fold({
        log(s"Impossible!")
      })(r => {
        log(s"pick ${r.getText}")
        r.click()
      })

    } else {
      log(s"no available RDV")
    }
  }

  private def chooseOneRdv(rdvs: mutable.Buffer[WebElement]): Option[WebElement] = {
    if (rdvs.nonEmpty) {
      Some(Random.shuffle(rdvs).head)
    } else {
      None
    }
  }

  private def log(msg: String): Unit = println(msg)
}