package co.uproot.abandon

import org.apache.fop.apps.FopFactory
import FOP._
import Util._
import scala.xml.Node
import scala.xml.Elem
import java.time.format.DateTimeFormatter
import java.time.format.ResolverStyle
import scala.util.matching.Regex

trait WithDelta {
  val delta: BigDecimal
}

object Util {
  val monthMap = Array("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  val Zero = BigDecimal(0)
  def sum(s: Iterable[BigDecimal]) = s.foldLeft(Zero)(_ + _)
  // def sumDeltas(s: Seq[{ val delta: BigDecimal }]) = s.foldLeft(Zero)(_ + _.delta)
  def sumDeltas(s: Seq[WithDelta]) = s.foldLeft(Zero)(_ + _.delta)
  def except[T](a: Seq[T], i: Int) = a.slice(0, i) ++ a.slice(i + 1, a.length)

  private def parseDateWith(s: String, dtf: DateTimeFormatter): Either[String, Date] = {
    return try {
      val ld = java.time.LocalDate.parse(s, dtf)
      val d = Date(ld.getYear, ld.getMonthValue, ld.getDayOfMonth)
      println(d)
      Right(d)
    } catch {
      case e:java.time.DateTimeException =>
        Left(e.getMessage)
    }
  }

  def parseDate(s:String):Date = {
    parseDateWith(s, DateTimeFormatter.ofPattern("d-MMM-uuuu").withResolverStyle(ResolverStyle.STRICT)).orElse {
      parseDateWith(s, DateTimeFormatter.ofPattern("uuuu/MMM/d").withResolverStyle(ResolverStyle.STRICT))
    } match {
      case Right(date) => date
      case Left (message) =>
        println("Couldn't parse date because, " + message)
        println("Supported formats are year/month/day or day-month-year")
        System.exit(1)
        null
    }
  }

  val nameSplitRegEx = new Regex(":")
}

class Main(args: Array[String]) {

  def parsePost(postNode: Node): Post = {
    val name = postNode.attrib("name")
    val delta = BigDecimal(postNode.attrib("delta"))
    val commentOpt = (postNode \ "comment").headOption.map(_.text)
    Post(name, delta, commentOpt)
  }

  def parseTransaction(txnNode: Node): Transaction = {
    val dateFields = txnNode.attrib("date").split("-").map(_.toInt)
    val date = Date(dateFields(0), dateFields(1), dateFields(2))
    val txnComments = (txnNode \ "comment").map(_.text)
    val annotationOpt = (txnNode \ "annotation").headOption.map(_.text)
    val payeeOpt = (txnNode \ "payee").headOption.map(_.text)
    val postNodes = txnNode \ "post"
    val posts = postNodes.map(parsePost)
    assert(sumDeltas(posts) == Zero, "Posts don't add up to zero:\n" + posts.mkString("\n"))
    Transaction(date, annotationOpt, payeeOpt, txnComments, posts)
  }

  def parseFromFile(fName: String) = {
    val txnNodes = xml.XML.load(fName) \\ "abandon" \\ "journal" \\ "transactions" \\ "txn"
    val rawTxns = txnNodes.map(parseTransaction)
    val demuxedTxns = rawTxns // .map(_.demuxTxns)
    demuxedTxns.foreach { _.updateTxnRefs() }
    demuxedTxns
  }

  val fName = args(0) // TODO: Config
  val reportStartDate = Util.parseDate(args(1)) // TODO: Config
  val muted = args(2) // ".*Personal.*"
  val txns = parseFromFile(fName)

  // val txns = txns.flatMap(_.txns)
  // val cookedTxns = txns.map { _.simpleCook }
  val cookedTxns = txns.flatMap(_.cookPosts)

  val ledgerReport = new Report(reportStartDate, cookedTxns, muted).mkLedgerReport

  val dumpFOP = false
  if (dumpFOP) {
    val pp = new xml.PrettyPrinter(120, 4)
    val writer = new java.io.FileWriter("report.fop")
    writer.write(pp.format(ledgerReport))
    writer.close
  }
  mkPDF("report.pdf", ledgerReport)

  val detailedBalanceReport = new Report(reportStartDate, cookedTxns, muted).mkDetailedBalanceReport
  mkPDF("detailed_balance_report.pdf", detailedBalanceReport)

  val flowReport = new Report(reportStartDate, cookedTxns, muted).mkFlowReport
  mkPDF("flow_report.pdf", flowReport)
}

object Main {
  def main(args: Array[String]): Unit = {
    new Main(args)
  }
}
