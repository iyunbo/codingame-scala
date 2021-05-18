package org.iyunbo.coding
package vaccine

import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.support.ui.{ExpectedConditions, WebDriverWait}
import org.openqa.selenium.{By, _}
import org.scalatest.time.{Seconds, Span}
import org.scalatestplus.selenium.WebBrowser

import java.nio.file.Files
import java.time.LocalDateTime
import scala.collection.mutable
import scala.io.StdIn.readLine
import scala.jdk.CollectionConverters._
import scala.language.postfixOps
import scala.util.Random

object RdvVaccine extends WebBrowser {


  val HOST = "https://www.doctolib.fr/"

  // behavioural parameters
  type Product = String
  val DEFAULT_START_PAGE: Int = 1
  val DEFAULT_END_PAGE: Int = 8
  val STEP_Y = 700
  val TOP_Y = 150
  val BOTTOM_Y = 1600
  val MAX_ROUND = 2000
  val SEARCH_WAIT_SEC = 2

  // functional parameters
  val REGION = "paris"
  val MAX_AVAILABLE_DAYS = 2

  val FIRST_MODERNA = 7005
  val SECOND_MODERNA = 7004

  val FIRST_PFIZER_BIONTECH = 6970
  val SECOND_PFIZER_BIONTECH = 6971
  val THIRD_PFIZER_BIONTECH = 8192

  val FIRST_ASTRA_ZENECA = 7107
  val SECOND_ASTRA_ZENECA = 7108
  val THIRD_ASTRA_ZENECA = 8193

  val JANSSEN = 7945

  val MOTIF_CATEGORY = "Vaccination Pfizer"
  val PRIMARY_OPTION_INJECTION = "1re injection vaccin COVID-19 (Pfizer-BioNTech)"
  val SECONDARY_OPTION_INJECTION = "1re injection vaccin COVID-19 (Moderna)"
  val DEFAULT_PRODUCT: Product = s"ref_visit_motive_ids%5B%5D=$FIRST_MODERNA&ref_visit_motive_ids%5B%5D=$FIRST_PFIZER_BIONTECH"

  val VACCINE_CENTER_FILTER = "ancestor::div[@id!='search-result-5021419']"

  val product: String => Product = {
    case "pfizer1" => productParam(FIRST_PFIZER_BIONTECH)
    case "pfizer2" => productParam(SECOND_PFIZER_BIONTECH)
    case "pfizer3" => productParam(THIRD_PFIZER_BIONTECH)
    case "moderna1" => productParam(FIRST_MODERNA)
    case "moderna2" => productParam(SECOND_MODERNA)
    case "janssen" => productParam(JANSSEN)
    case "astra1" => productParam(FIRST_ASTRA_ZENECA)
    case "astra2" => productParam(SECOND_ASTRA_ZENECA)
    case "astra3" => productParam(THIRD_ASTRA_ZENECA)
    case s => throw new IllegalArgumentException(s"unknown product: $s")
  }

  // initialization
  setup()
  implicit val driver: WebDriver = new ChromeDriver()
  val js: JavascriptExecutor = driver.asInstanceOf[JavascriptExecutor]

  def productParam(id: Int): Product = s"ref_visit_motive_ids%5B%5D=$id"

  def main(args: Array[String]): Unit = {

    log(s"set DOM search waiting time to $SEARCH_WAIT_SEC")
    implicitlyWait(Span(SEARCH_WAIT_SEC, Seconds))

    var startPage: Int = DEFAULT_START_PAGE
    var endPage: Int = DEFAULT_END_PAGE
    var produit: Product = DEFAULT_PRODUCT

    if (args.length >= 2) {
      startPage = Integer.valueOf(args(0))
      endPage = Integer.valueOf(args(1))
    }

    if (args.length >= 3) {
      log(s"Search for product(s): ${args(2)}")
      produit = args(2).split(",").map(product).mkString("&")
    }

    agreeCookies

    for (round <- 1 to MAX_ROUND) findRdvVaccine(round)(startPage, endPage, produit)

    Beep.long()
    driver.close()
  }


  def getOsSuffix: String = {
    val os = System.getProperty("os.name")
    log(s"OS: $os")
    val osString = os.toLowerCase
    if (osString.contains("win")) {
      "win32"
    } else if (osString.contains("nux")) {
      "linux64"
    } else if (osString.contains("darwin") || osString.contains("mac")) {
      "mac64"
    } else {
      "generic"
    }
  }

  private def setup(): Unit = {
    import java.io.File
    import java.net.URL
    import java.nio.file.Paths
    import sys.process._

    log("Initialization started")

    val driverName = "chromedriver"
    val driverDownloadUrl = s"https://chromedriver.storage.googleapis.com/90.0.4430.24/chromedriver_$getOsSuffix.zip"
    val dir = Paths.get(".")
    val driverFile = Paths.get(driverName)

    if (!Files.exists(driverFile)) {
      log(s"chromedriver does not exist, downloading from $driverDownloadUrl")
      val zipFile = s"$driverName.zip"
      new URL(driverDownloadUrl) #> new File(zipFile) !!

      log(s"unzipping $zipFile")
      FileHelper.uncompress(zipFile)
      Files.delete(Paths.get(zipFile))

      log(s"ensuring executable permission on $driverName")
      FileHelper.setExecutePermission(driverName)

      log(s"driver $driverName is now ready")
    }

    val driverPath = Paths.get(driverName).toAbsolutePath.toString
    log(s"setting chormedriver path to $driverPath")
    System.setProperty("webdriver.chrome.driver", driverPath)

    log("Initialization done")
  }

  private def agreeCookies: Any = {
    go to s"$HOST/vaccination-covid-19/$REGION?force_max_limit=$MAX_AVAILABLE_DAYS&$DEFAULT_PRODUCT"
    val agreeCookies = driver.findElements(By.xpath(s"//button[@id='didomi-notice-agree-button']"))
    if (!agreeCookies.isEmpty) {
      log(s"We accepte cookies")
      agreeCookies.get(0).click()
    }
  }

  private def findRdvVaccine(round: Int)(startPage: Int, endPage: Int, produit: Product): Unit = {
    log(s"start round $round with parameters: (startPage=$startPage, endPage=$endPage, product=$produit)")
    for (page <- startPage to endPage) scanPage(page, produit)
  }

  def onPageLoad(page: Int): Unit = {
    log(s"loading page $page")
    log(s"url: ${driver.getCurrentUrl}")
  }

  private def scanPage(page: Int, produit: Product): Unit = {
    onPageLoad(page)
    go to s"$HOST/vaccination-covid-19/$REGION?force_max_limit=$MAX_AVAILABLE_DAYS&page=$page&$produit"
    for (position <- TOP_Y to BOTTOM_Y by STEP_Y) {
      analyseRdvs(position)
    }
  }

  private def analyseRdvs(y: Int): Unit = {
    js.executeScript(s"window.scrollBy(0,$y)")

    log(s"searching for available RDV ... ")
    val rdvs = searchRdvs

    chooseOne(rdvs)
      .fold {
        log(s"No RDV available near y=$y")
      } { rdv =>
        prendreRdv(rdv)
      }

  }

  private def searchRdvs = {
    driver.findElements(
      //By.xpath(s"//div[@class='Tappable-inactive availabilities-slot' and $VACCINE_CENTER_FILTER]")
      By.xpath(s"//div[@class='Tappable-inactive availabilities-slot']")
    ).asScala
  }

  def falsePositive(address: String): Boolean = {
    // we keep here the list of false positive centers
    address equals "Salle des fêtes René L'HELG\n12 Rue Edouard Vaillant, 91200 Athis-Mons"
  }

  def findSecendInjection: Any = {
    log(s"Good! the first RDV is successfully reserved")
    log(s"looking for available RDV of second injection")
    val rdvs = searchRdvs
    pickAvailableRdv(rdvs)
  }

  private def prendreRdv(rdv: WebElement): Unit = {
    try {
      process(rdv)
    } catch {
      case e: ElementClickInterceptedException =>
        log("Cannot click, we guess it's either a false slot or already visited, ignoring")
      case e: StaleElementReferenceException =>
        log("DOM has changed, verify if you changed the url")
        e.printStackTrace()
      case e: Exception =>
        log("Unexpected error")
        e.printStackTrace()
    }
  }

  private def process(rdv: WebElement): Unit = {
    enterRdvPage(rdv)

    answerArleadyConsulted()

    identifyAddress()

    answerMotifCategory()

    answerMotif()

    findBothRdv()

    saitisfayOrRetry()
  }

  private def saitisfayOrRetry(): Unit = {
    if (successful()) {
      log(s"You got a RDV !")
      Beep.short()
      pause
    } else {
      retry()
    }
  }

  private def answerMotif(): Unit = {
    val motif = driver.findElements(By.xpath(s"//select[@class='dl-select form-control dl-select-block booking-compact-select']/option[text()='$PRIMARY_OPTION_INJECTION']"))
    if (!motif.isEmpty) {
      log(s"select motif ${motif.get(0).getText}")
      motif.get(0).click()
    } else {
      trySecondOption()
    }
  }

  private def answerMotifCategory(): Unit = {
    val category = driver.findElements(By.xpath(s"//select[@class='dl-select form-control dl-select-block booking-compact-select']/option[text()='$MOTIF_CATEGORY']"))
    if (!category.isEmpty) {
      log(s"the motif category is required, choose ${category.get(0).getText}")
      category.get(0).click()
    }
  }

  private def identifyAddress(): Unit = {
    val address = driver.findElements(By.xpath(s"//div[@class='dl-text dl-text-body dl-text-regular dl-text-s']"))
    if (!address.isEmpty) {
      log(s"the address:\n${address.get(0).getText}")
    } else {
      log(s"could not identify the address, going forward anyway")
    }
  }

  private def answerArleadyConsulted(): Unit = {
    log(s"checking this RDV at: ${driver.getCurrentUrl}")
    val alreadyConsulted = driver.findElements(By.xpath(s"//span[@class='dl-text dl-text-body dl-text-regular dl-text-s dl-selectable-card-title' and text()='Non']"))
    if (!alreadyConsulted.isEmpty) {
      log(s"they ask us if we had already consulted the center, choose ${alreadyConsulted.get(0).getText}")
      alreadyConsulted.get(0).click()
    }
  }

  private def enterRdvPage(rdv: WebElement): Unit = {
    log(s"RDV available: ${rdv.getAttribute("title")}")
    log(s"found potential RDV on: ${driver.getCurrentUrl}")

    log(s"about to click on the RDV [${rdv.getText}] at location ${rdv.getLocation}")
    val wait = new WebDriverWait(driver, 3)
    wait.until(ExpectedConditions.elementToBeClickable(rdv))
    rdv.click()
  }

  private def retry(): Unit = {
    log(s"We could not complete the RDV")
    log(s"Try one more time")

    trySecondOption()

    findBothRdv()

    if (successful()) {
      log(s"You got a RDV !")
      Beep.short()
      pause
    } else {
      log(s"We could not make the RDV, going back to last url")
      goBack()
    }
  }

  private def findBothRdv(): Unit = {
    val rdvs = findFirstInjection

    if (rdvs.nonEmpty) {
      findSecendInjection
    }
  }

  private def findFirstInjection = {
    log(s"looking for available RDV of first injection")
    val rdvs = searchRdvs
    pickAvailableRdv(rdvs)
    rdvs
  }

  private def trySecondOption(): Unit = {
    log(s"cannot find first option, trying second option")
    val opt = driver.findElements(By.xpath(s"//select[@class='dl-select form-control dl-select-block booking-compact-select']/option[text()='$SECONDARY_OPTION_INJECTION']"))
    if (!opt.isEmpty) {
      log(s"select option ${opt.get(0).getText}")
      opt.get(0).click()
    }
  }

  private def successful(): Boolean = {
    log(s"checking if the RDV is valid")
    val accept = driver.findElements(By.xpath("//button[@class='dl-button-check-inner' and contains(text(), 'accepte')]"))
    !accept.isEmpty
  }

  private def pause = {
    readLine("Enter to continue")
  }

  private def pickAvailableRdv(rdvs: mutable.Buffer[WebElement]): Unit = {
    if (rdvs.nonEmpty) {
      log(s"found ${rdvs.size} RDVs")

      val rdv = chooseOne(rdvs)

      rdv.fold({
        log(s"Impossible! Check your code")
      })(r => {
        log(s"about to pick RDV[${r.getAttribute("title")}]")
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

  private def log(msg: String): Unit = println(s"${LocalDateTime.now()} [COVID-19] [INFO] $msg")
}