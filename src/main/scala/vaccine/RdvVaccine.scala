package org.iyunbo.coding
package vaccine

import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.support.ui.{ExpectedConditions, WebDriverWait}
import org.openqa.selenium.{By, _}
import org.scalatest.time.{Seconds, Span}
import org.scalatestplus.selenium.WebBrowser

import java.time.LocalDateTime
import scala.annotation.tailrec
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
  implicit val DEFAULT_START_PAGE: Int = 1
  implicit val DEFAULT_END_PAGE: Int = 8
  val verticalStep = 700
  val topPosition = 150
  val bottomPosition = 1600
  val maxCalendarsPerPage = 10
  val MAX_ROUND = 200

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

  val OPTION_MOTIF = "Patients de moins de 50 ans éligibles"
  val OPTION_INJECTION = "1re injection vaccin COVID-19 (Pfizer-BioNTech)"

  def main(args: Array[String]): Unit = {

    driver.manage().window().maximize()
    implicitlyWait(Span(1.5, Seconds))

    implicit var startPage: Int = RdvVaccine.DEFAULT_START_PAGE
    implicit var endPage: Int = RdvVaccine.DEFAULT_END_PAGE

    if (args.length >= 2) {
      startPage = Integer.valueOf(args(0))
      endPage = Integer.valueOf(args(1))
    }


    for (round <- 1 to MAX_ROUND) findRdvVaccine(round)(startPage, endPage)

    Beep.long()
    driver.close()
  }


  private def findRdvVaccine(round: Int)(implicit startPage: Int, endPage: Int): Unit = {
    log(s"start round $round")
    for (page <- startPage to endPage) scanPage(page)
  }

  def onPageLoad(page: Int): Unit = {
    log(s"loading page $page")
    log(s"url: ${driver.getCurrentUrl}")
  }

  private def scanPage(page: Int): Unit = {
    onPageLoad(page)
    go to s"$host/vaccination-covid-19/$region?force_max_limit=$maxAvailableDays&page=$page&ref_visit_motive_ids%5B%5D=$FIRST_MODERNA&ref_visit_motive_ids%5B%5D=$FIRST_PFIZER_BIONTECH"
    for (position <- topPosition to bottomPosition by verticalStep) {
      analyseRdvs(position)
    }
  }

  private def analyseRdvs(y: Int): Unit = {
    js.executeScript(s"window.scrollBy(0,$y)")
    Thread.sleep(waitForRefresh)

    val rdvs = searchRdvs

    chooseOne(rdvs)
      .fold {
        log("No RDV available")
      } {
        prendreRdv
      }

  }

  private def searchRdvs = {
    driver.findElements(By.xpath("//div[@class='Tappable-inactive availabilities-slot']")).asScala
  }

  def falsePositive(address: String): Boolean = {
    (address equals "Salle des fêtes René L'HELG\n12 Rue Edouard Vaillant, 91200 Athis-Mons") ||
      (address equals "Centre de Vaccination Covid Paris 15\n31 Rue Peclet, 75015 Paris")
  }

  private def prendreRdv(rdv: WebElement): Unit = {
    try {
      log(s"available: ${rdv.getText}")
      log(s"found potential RDV on: ${driver.getCurrentUrl}")

      log(s"about to click on the RDV [${rdv.getAttribute("title")}] at location ${rdv.getLocation}")
      val wait = new WebDriverWait(driver, 6)
      wait.until(ExpectedConditions.elementToBeClickable(rdv))
      rdv.click()

      log(s"checking this RDV at: ${driver.getCurrentUrl}")
      val alreadyConsulted = driver.findElements(By.xpath(s"//span[@class='dl-text dl-text-body dl-text-regular dl-text-s dl-selectable-card-title' and text()='Non']"))
      if (!alreadyConsulted.isEmpty) {
        log(s"they ask us if we had already consulted the center, choose ${alreadyConsulted.get(0).getText}")
        alreadyConsulted.get(0).click()
      }

      val address = driver.findElement(By.xpath(s"//div[@class='dl-text dl-text-body dl-text-regular dl-text-s']"))
      log(s"the address:\n${address.getText}")
      if (falsePositive(address.getText)) {
        log(s"this vacine center is known to be unavailable, skipping")
        // we don't come back to last page, otherwise we repeat the same RDV
        return
      }

      log("Congz! this center is available")

      val cat = driver.findElements(By.xpath(s"//select[@class='dl-select form-control dl-select-block booking-compact-select']/option[text()='$OPTION_MOTIF']"))
      if (!cat.isEmpty) {
        log(s"the motif is required, choose ${cat.get(0).getText}")
        cat.get(0).click()
      }

      val opt = driver.findElement(By.xpath(s"//select[@class='dl-select form-control dl-select-block booking-compact-select']/option[text()='$OPTION_INJECTION']"))
      log(s"select option ${opt.getLocation}")
      opt.click()

      log(s"looking for available RDV of first injection")
      var rdvs = searchRdvs
      pickAvailableSlot(rdvs)

      if (rdvs.nonEmpty) {
        log(s"looking for available RDV of second injection")
        rdvs = searchRdvs
        pickAvailableSlot(rdvs)
        log(s"You got a RDV!")
        Beep.short()
        pause
      }

      acceptIfAsked()

      val acceptTerm = "\"J'ai lu et j'accepte les consignes\""
      val submit = driver.findElements(By.xpath(s"//button[@class='dl-button-check-inner']/option[text()=$acceptTerm]"))
      if (!submit.isEmpty) {
        log(s"Submit, at ${submit.get(0).getLocation}")
        submit.get(0).click()
        acceptIfAsked()
      }

    } catch {
      case e: ElementClickInterceptedException => {
        log("Cannot click, we guess it's a false slot")
        e.printStackTrace()
      }
      case e: StaleElementReferenceException => e.printStackTrace()
      case e: Exception => e.printStackTrace()
    }
  }

  @tailrec
  private def acceptIfAsked(): Unit = {
    val acceptTerm = "\"J'accepte\""
    val accept = driver.findElements(By.xpath(s"//button[@class='dl-button-check-inner']/option[text()=$acceptTerm]"))
    if (!accept.isEmpty) {
      log(s"accept the terms, at ${accept.get(0).getLocation}")
      accept.get(0).click()
      acceptIfAsked()
    }
  }

  private def pause = {
    readLine("Enter to continue")
  }

  private def pickAvailableSlot(rdvs: mutable.Buffer[WebElement]): Unit = {
    if (rdvs.nonEmpty) {
      log(s"found ${rdvs.size} RDVs")

      val rdv = chooseOne(rdvs)

      rdv.fold({
        log(s"Impossible!")
      })(r => {
        log(s"pick [${r.getAttribute("title")}]")
        r.click()
      })

    } else {
      log(s"no available RDV")
    }
  }

  private def chooseOne[A](rdvs: Iterable[A]): Option[A] = {
    if (rdvs.nonEmpty) {
      Some(Random.shuffle(rdvs).head)
    } else {
      None
    }
  }

  private def log(msg: String): Unit = println(s"${LocalDateTime.now()} [Covid-19] [Info] $msg")
}