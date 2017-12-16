package co.uproot.abandon

import org.apache.fop.apps.FopFactory
import java.io.FileOutputStream
import org.xml.sax.helpers.XMLReaderFactory
import org.xml.sax.helpers.XMLReaderAdapter
import java.io.StringReader
import org.xml.sax.InputSource
import FOP._
import scala.xml.Node
import Util._
import scala.xml.transform.RewriteRule
import scala.xml.transform.RuleTransformer
import scala.xml.Elem
import java.time.format.DateTimeFormatter
import java.time.format.ResolverStyle

object Util {
  val monthMap = Array("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  val Zero = BigDecimal(0)
  def sum(s: Iterable[BigDecimal]) = s.foldLeft(Zero)(_ + _)
  def sumDeltas(s: Seq[{ val delta: BigDecimal }]) = s.foldLeft(Zero)(_ + _.delta)
  def except[T](a: Seq[T], i: Int) = a.slice(0, i) ++ a.slice(i + 1, a.length)

  def parseDate(s:String):Date = {
    try {
    val dtf = DateTimeFormatter.ofPattern("d-MMM-uuuu").withResolverStyle(ResolverStyle.STRICT)
    val ld = java.time.LocalDate.parse(s, dtf)
    val d = Date(ld.getYear, ld.getMonthValue, ld.getDayOfMonth)
    println(d)
    d
    } catch {
      case e:java.time.DateTimeException =>
        println("Couldn't parse date because, " + e.getMessage)
        System.exit(1)
        null
    }
  }
}

case class Date(year: Int, month: Int, day: Int) {
  override def toString = f"$day%2d ${monthMap(month - 1)} $year%4d"
  def toInt = {
    year * 400 + month * 40 + day
  }
}

/** A transaction as parsed from input */
case class Transaction(
  date: Date,
  annotationOpt: Option[String],
  payeeOpt: Option[String],
  comments: Seq[String],
  posts: Seq[Post]) {

  def splitPostsByCount = {
    val (posPosts, negPosts) = posts.partition(_.delta > Zero)
    if (posPosts.length > negPosts.length) {
      (posPosts, negPosts)
    } else {
      (negPosts, posPosts)
    }
  }

  def demuxPosts = {
    val (majorityPosts, minorityPosts) = splitPostsByCount
    val minorityAccountNames = minorityPosts.map(_.name).toSet
    // assert(minorityAccountNames.size == 1, "Currently, minority Post can only be from one account. MinorityAccountNames: " + minorityAccountNames.mkString(", "))
    if (majorityPosts.length == minorityPosts.length || minorityAccountNames.size != 1) {
      // Demux not required
      this
    } else {
      // Demux possible
      assert(minorityPosts.length == 1, "Currently, only one minority Post supported")
      val minorTxn = minorityPosts.head
      val demuxedPosts = majorityPosts.map(major => Post(minorTxn.name, -major.delta, minorTxn.commentOpt))
      copy(posts = majorityPosts ++ demuxedPosts)
    }
  }

  def cookPosts: Seq[CookedPost] = {
    val (majorityPosts, minorityPosts) = splitPostsByCount
    val (majorityCount, minorityCount) = (majorityPosts.length, minorityPosts.length)
    if (majorityCount == 1 && minorityCount == 1) {
      // Simple case
      posts.map { _.simpleCook }
    } else {
      val majorityAccountNames = majorityPosts.map(_.name).toSet
      if (minorityCount == 1 && majorityAccountNames.size == 1) {
        // Needs simple demux
        val minorityCookedTxn = minorityPosts.head.simpleCook
        val majorityAccountName = majorityAccountNames.head
        val oppOtherPosts = majorityPosts.map { mt =>
          CookedOtherPost(minorityCookedTxn.name, -mt.delta, mt.splitComments)
        }
        val majorityCookedTxn = CookedPost(date, majorityAccountName, sumDeltas(majorityPosts), comments.toSeq, oppOtherPosts, Nil)
        Seq(majorityCookedTxn, minorityCookedTxn)
      } else {
        posts.map { _.simpleCook }
      }
    }
  }

  def updateTxnRefs() {
    posts.zipWithIndex.foreach {
      case (post, idx) =>
        post.other = except(posts, idx)
        post.txn = this
    }
  }
}

/** A post as parsed from input */
case class Post(
  name: String,
  delta: BigDecimal,
  commentOpt: Option[String]) {

  var other: Seq[Post] = Nil
  var txn: Transaction = null

  val isPositive = delta > Zero

  def getBalancedOther = {
    other.partition(isPositive == _.isPositive)
  }

  val splitComments:Seq[String] = commentOpt.map(_.split(";").toList).getOrElse(Nil)

  // TODO: All annotations should not be prefixed by "Chq #". Should be a config setting either here or in abandon
  def comments = txn.annotationOpt.map("Chq #" + _) ++ txn.payeeOpt ++ splitComments ++ txn.comments

  def date = txn.date

  def simpleCook = {
    val (similarOther, oppOther) = getBalancedOther
    val oppOtherCooked = oppOther.map(o => CookedOtherPost(o.name, o.delta, o.splitComments))
    val similarOtherCooked = similarOther.map(o => CookedOtherPost(o.name, o.delta, o.splitComments))
    CookedPost(date, name, delta, comments.toSeq, oppOtherCooked, similarOtherCooked)
  }
}

/** A post that is ready to be reported. All the analysis has to be done previously */
case class CookedPost(date: Date, name: String, delta: BigDecimal, comments: Seq[String], oppositeOthers: Seq[CookedOtherPost], similarOthers: Seq[CookedOtherPost]) {
  val isComplex = math.min(oppositeOthers.length, similarOthers.length + 1) > 1
}

/** A transaction that is ready to be reported as the other entry of a transaction. All the analysis has to be done previously */
case class CookedOtherPost(name: String, delta: BigDecimal, comments: Seq[String])

object Main extends App {

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

class Report(startDate: Date, posts: Seq[CookedPost], muted: String) {

  private val allAccountNames = posts.map(_.name).toSet
  private val sortedAllAccountNames = allAccountNames.toSeq.sorted
  private val (currPosts, prevPosts) = posts.partition(_.date.toInt >= startDate.toInt)
  private val openingBalances = prevPosts.groupBy(_.name).map { case (name, gposts) => (name -> sumDeltas(gposts)) }

  private val emptyCell = Cell("")
  private val thinLine = "grey .5pt solid"
  private val veryThinLine = "#ddd .5pt dotted"
  private val bottomThinLineAttr = Map("border-bottom" -> thinLine)
  private val topThinLineAttr = Map("border-top" -> thinLine)
  private val topVeryThinLineAttr = Map("border-top" -> veryThinLine)
  private val bottomVeryThinLineAttr = Map("border-bottom" -> veryThinLine)
  private val lightGreyBackground = Map("background-color" -> "#ddd")
  private val leftThinBorder = Map("border-left" -> "#bbb .5pt solid")
  private val boldAttr = Map("font-weight" -> "bold")
  private val rowPaddingTopAttr = Map("padding-top" -> "4pt")
  private val rowPaddingBottomAttr = Map("padding-bottom" -> "4pt")
  private val rowMarginLeftAttr = Map("margin-left" -> "4pt")
  private val rowMarginBottomAttr = Map("margin-bottom" -> "4pt")
  private val italicAttr = "font-style" -> "italic"
  private val openCloseAttr = boldAttr ++ Map(italicAttr, "color" -> "grey")
  private val alignCenterAttr = Map("text-align" -> "center")
  private val alignRightAttr = Map("text-align" -> "right")
  private val keepWithNextAttr = Map("keep-with-next.within-page" -> "always")
  private val keepWithPrevAttr = Map("keep-with-previous.within-page" -> "always")
  private val amountPadRightAttr = Map("padding-right" -> "2pt")

  private def formatDate(d: Date) = {
    xml.Group(Seq(mkBlock(f"${d.day}%2d ${monthMap(d.month - 1)}"), mkBlock(f"${d.year}%4d") % (rowPaddingBottomAttr + ("font-size" -> "80%"))))
  }

  private def mkAmountCell(a: BigDecimal, suffix: String = "") = {
    val str = f"${a}%,.2f"
    val color = if (a.isWhole) { "#bbb" } else { "#888" }
    val formattedStr = str replaceAll ("(\\.[0-9]*$)", "<fo:inline color='" + color + "'>$1</fo:inline>")
    // val formattedStr = str replaceAll ("([^.]*)(\\..*$)", "<fo:inline font-weight='bold'>$1</fo:inline><fo:inline color='grey'>$2</fo:inline>")
    Cell(xml.Unparsed(formattedStr + suffix), alignRightAttr, cellAttribs = amountPadRightAttr)
  }

  private def mkBalanceCell(a: BigDecimal) = {
    if (a == Zero) {
      mkAmountCell(a, "  Zr")
    } else if (a < Zero) {
      mkAmountCell(-a, " Dr")
    } else {
      mkAmountCell(a, " Cr")
    }
  }

  private val ledgerHeader = Row(
    Seq(
      Cell("Date"),
      emptyCell,
      Cell("Particulars"),
      Cell("Debit", alignRightAttr, cellAttribs = amountPadRightAttr),
      Cell("Credit", alignRightAttr, cellAttribs = amountPadRightAttr),
      Cell("Balance", alignRightAttr, cellAttribs = amountPadRightAttr)),
    topThinLineAttr ++ bottomThinLineAttr)

  private val trialBalanceHeader = Row(
    Seq(
      Cell("Account"),
      Cell("Opening Balance", alignRightAttr, cellAttribs = amountPadRightAttr),
      Cell("Debit", alignRightAttr, cellAttribs = amountPadRightAttr),
      Cell("Credit", alignRightAttr, cellAttribs = amountPadRightAttr),
      Cell("Closing Balance", alignRightAttr, cellAttribs = amountPadRightAttr)),
    topThinLineAttr ++ bottomThinLineAttr)

  private def mkTxn(txn: CookedPost, resultBalance: BigDecimal) = {
    val comments = txn.comments
    val sumSimilarOther = sumDeltas(txn.similarOthers)

    val attrs = if (txn.isComplex) Map("background-color" -> "#f99") else emptyAttribs
    val commentAttrs = attrs + ("color" -> "#222") + ("font-size" -> "92%")
    val relatedAttrs = attrs + ("color" -> "#222") + ("font-size" -> "92%")

    def mkOtherComment(c: String) = {
      val pointer = mkInline("‣", Map("font-family" -> "DejaVu Sans"))
      mkBlock(xml.Group(Seq(pointer, c))) % (commentAttrs + ("margin-left" -> "8pt"))
    }

    val narrationBlocks = comments.map { comment =>
      mkBlock(comment) % commentAttrs
    }

    val relatedBlocks =
      if (txn.similarOthers.length > 0) {
        val headerRow = Row(Seq(Cell("Related transactions:")), relatedAttrs + ("font-size" -> "80%"))
        val rows =
          txn.similarOthers.map {
            case otherTxn =>
              Row(Seq(Cell(otherTxn.name), mkBalanceCell(otherTxn.delta)), relatedAttrs)
          }
        mkTable(
          rows, Seq(headerRow),
          Map(),
          Map(5 -> leftThinBorder, 4 -> leftThinBorder)) % ("keep-together.within-page" -> "1") % ("background-color" -> "#eee")
      } else {
        Nil
      }

    val muteNarration = (txn.oppositeOthers.length == 1) && (txn.oppositeOthers.head.name matches muted)
    val numOppOthers = txn.oppositeOthers.length
    val rows = txn.oppositeOthers.zipWithIndex map {
      case (otherTxn, indx) =>
        val firstTxn = (indx == 0)
        val lastTxn = (indx == numOppOthers - 1)

        val dateCellOpt = if (firstTxn) Some(Cell(formatDate(txn.date), cellAttribs = Map("number-rows-spanned" -> s"$numOppOthers"))) else None
        val padAttrs: Attributes = if (lastTxn) rowPaddingBottomAttr else Map.empty
        val delta = (sumSimilarOther + otherTxn.delta)
        val nameBlock = mkBlock(<fo:basic-link internal-destination={ otherTxn.name }>{ otherTxn.name }</fo:basic-link>) % boldAttr

        val otherCommentOpt = if (otherTxn.name matches muted) Nil else otherTxn.comments
        val commentBlock = otherCommentOpt.map(c => mkOtherComment(c))
        val otherBlock = Seq(nameBlock) ++ commentBlock
        val contents =
          xml.Group(if (lastTxn) {
            if (muteNarration) {
              otherBlock ++ relatedBlocks
            } else {
              otherBlock ++ narrationBlocks ++ relatedBlocks
            }
          } else {
            otherBlock
          })
        val cells = (dateCellOpt ++: Seq(
          Cell(if (delta > Zero) "To" else "By", alignCenterAttr),
          Cell(contents, padAttrs))) ++ mkDbCrCells(delta, if (firstTxn) Some(resultBalance) else None)

        Row(cells, attrs ++ { if (indx != 0) keepWithPrevAttr else (topVeryThinLineAttr) })
    }

    rows
  }

  private def mkAccountReport(name: String, gposts: Seq[CookedPost]) = {
    val openingBalance = openingBalances.get(name).getOrElse(Zero)
    val resultBalances = gposts.scanLeft(openingBalance)(_ + _.delta).tail
    val lastResult = resultBalances.last
    val rows = gposts.zip(resultBalances).map {
      case (txn, resultBalance) =>
        mkTxn(txn, resultBalance)
    }
    val openingRow = Row(Seq(
      emptyCell,
      emptyCell,
      // Cell("Opening Balance", openCloseAttr)) ++ mkDbCrCells(openingBalance, Some(openingBalance)),
      Cell("Opening Balance", openCloseAttr ++ rowPaddingBottomAttr),
      emptyCell, emptyCell,
      mkBalanceCell(openingBalance)),
      keepWithNextAttr)
    val closingBalance = -lastResult
    /*
    val closingRow = Row(Seq(
      emptyCell,
      Cell(if (closingBalance > Zero) "To" else "By", alignCenterAttr ++ openCloseAttr),
      Cell("Closing Balance", openCloseAttr)) ++ mkDbCrCells(closingBalance, None),
      bottomThinLineAttr)
      */

    val (debitPosts, creditPosts) = gposts.partition(_.delta < Zero)
    val debitSubTotal = -sumDeltas(debitPosts)
    val creditSubTotal = sumDeltas(creditPosts)
    val subTotalRow = Row(Seq(
      emptyCell,
      emptyCell,
      emptyCell,
      mkAmountCell(debitSubTotal),
      mkAmountCell(creditSubTotal),
      mkBalanceCell(openingBalance + creditSubTotal - debitSubTotal)),
      topThinLineAttr ++ keepWithPrevAttr ++ boldAttr)
    val debitTotal = debitSubTotal + { if (closingBalance < Zero) closingBalance else Zero }
    val creditTotal = creditSubTotal - { if (closingBalance >= Zero) closingBalance else Zero }
    /*
    val totalRow = Row(Seq(
      emptyCell,
      emptyCell,
      emptyCell,
      mkAmountCell(debitTotal),
      mkAmountCell(creditTotal)),
      topThinLineAttr) */
    mkBlock(xml.Group(Seq(
      mkHeading(1, name) % keepWithNextAttr, //  % Map("break-before" -> "page"),
      mkTable(
        openingRow +: rows.flatten :+ subTotalRow, Seq(ledgerHeader),
        Map(0 -> "36pt", 1 -> "24pt", 2 -> "proportional-column-width(2.9)"),
        Map(5 -> leftThinBorder, 4 -> leftThinBorder))))) % ("keep-together.within-page" -> "1") % ("id" -> name)
  }

  private def mkDbCrCells(amount: BigDecimal, balanceOpt: Option[BigDecimal]) = Seq(
    if (amount > Zero) mkAmountCell(amount) else emptyCell,
    if (amount <= Zero) mkAmountCell(-amount) else emptyCell) ++
    balanceOpt.map(mkBalanceCell)

  private val footer = mkTable(Seq(Row(Seq(
    Cell(mkInline("Generated on " + new java.util.Date, Map("padding-left" -> "4pt", "color" -> "#555"))),
    Cell(mkInline(xml.Unparsed("Page # <fo:page-number font-weight='bold'/>"), Map("padding-right" -> "4pt")), alignRightAttr)),
    topThinLineAttr + ("background-color" -> "#eee"))))

  def mkLedgerReport = {
    val groupedPosts = currPosts.groupBy(_.name)
    val sortedGroupPosts = groupedPosts.toList.sortBy(_._1)
    val toc = sortedGroupPosts.collect {
      case (name, _) if (!(name matches muted))=>
        mkBlock(xml.Group(Seq(
          <fo:basic-link internal-destination={ name }>{ mkInline(name) % boldAttr }</fo:basic-link>,
          <fo:leader leader-pattern="dots" color="grey"/>,
          <fo:page-number-citation ref-id={ name }/>))) % Map("text-align-last" -> "justify", "margin-bottom" -> "4pt")
    }
    val tocBlock = mkBlock(xml.Group(toc)) % Map("padding" -> "8pt", "border" -> "grey 1pt solid", "margin-right" -> "1in", "margin-left" -> "1in")

    val doc = mkDoc(
      xml.Group(
        tocBlock +:
          sortedGroupPosts.collect {
            case (name, gposts) if (!(name matches muted)) => mkAccountReport(name, gposts)
          }.toSeq),
      footer)

    /*
    val narationTransformer = new AttribTransformer("narration", Map("padding-bottom" -> "4pt"))
    val firstRowTransformer = new AttribTransformer("first-row", Map("padding-top" -> "4pt"))
    val transformer = new RuleTransformer(narationTransformer, firstRowTransformer)
    transformer.transform(doc)
    */

    doc
  }

  case class AccountDetails(name: String, openingBalance: BigDecimal, closingBalance: BigDecimal, debitSubTotal: BigDecimal, creditSubTotal: BigDecimal)
  case class AccountSemiDetails(name: String, debitSubTotal: Option[BigDecimal], creditSubTotal: Option[BigDecimal])

  private def getDetails(name: String, gposts: Seq[CookedPost]) = {
    val openingBalance = openingBalances.get(name).getOrElse(Zero)
    val closingBalance = openingBalance + sumDeltas(gposts)
    val (debitPosts, creditPosts) = gposts.partition(_.delta < Zero)
    val debitSubTotal = -sumDeltas(debitPosts)
    val creditSubTotal = sumDeltas(creditPosts)
    AccountDetails(name, openingBalance, closingBalance, debitSubTotal, creditSubTotal)
  }

  private def mkSemiDetailedBalanceRow(details: AccountSemiDetails) = {
    Row(Seq(
      Cell(details.name, rowPaddingBottomAttr ++ rowMarginLeftAttr),
      emptyCell,
      details.debitSubTotal.map(mkAmountCell(_)).getOrElse(emptyCell),
      details.creditSubTotal.map(mkAmountCell(_)).getOrElse(emptyCell),
      emptyCell))
  }

  private def mkDetailedBalanceRow(details: AccountDetails) = {
    Row(Seq(
      Cell(details.name, rowPaddingBottomAttr ++ rowMarginLeftAttr),
      mkBalanceCell(details.openingBalance),
      mkAmountCell(details.debitSubTotal),
      mkAmountCell(details.creditSubTotal),
      mkBalanceCell(details.closingBalance)))
  }

  def mkFlowReport = {
    val groupedPosts = currPosts.groupBy(_.name)

    val significantAccountNames = sortedAllAccountNames.filter{ name =>
      groupedPosts.isDefinedAt(name) || openingBalances.get(name).getOrElse(Zero) != Zero
    }

    val blankLine =
      Row(Seq.fill(5)(Cell("", rowPaddingBottomAttr)))

    val emptyLineSeparator =
      Row(Seq.fill(5)(Cell("", rowPaddingBottomAttr)), topThinLineAttr)

    val rows =
      significantAccountNames.flatMap { name =>
        val gposts = groupedPosts.get(name).getOrElse(Nil)
        val details = getDetails(name, gposts)
        val groupRow = Row(Seq(
          Cell(name, boldAttr + ("font-size" -> "105%")),
          mkBalanceCell(details.openingBalance),
          mkAmountCell(details.debitSubTotal),
          mkAmountCell(details.creditSubTotal),
          mkBalanceCell(details.closingBalance)
          ), bottomVeryThinLineAttr ++ lightGreyBackground)

        val changerNames = gposts.flatMap(p => (p.oppositeOthers ++ p.similarOthers).map(_.name)).toSet
        val changerDetails = changerNames.flatMap { cname =>
          val changerAmounts = gposts.map{p =>
            val oppChangerPosts = p.oppositeOthers.filter(_.name == cname)
            if (oppChangerPosts.length > 0) {
              sumDeltas(oppChangerPosts ++ p.similarOthers)
            } else {
              Zero
            }
          }
          val (positiveAmounts, negativeAmounts) = changerAmounts.partition(_ > Zero)
          if (changerAmounts.length == 0 || (sum(positiveAmounts) == 0 && sum(negativeAmounts) == 0)) {
            None
          } else {
            def onlyIfNonZero(amt: BigDecimal) = if (amt != Zero) Some(amt) else None
            Some(AccountSemiDetails(cname, onlyIfNonZero(sum(positiveAmounts)), onlyIfNonZero(-sum(negativeAmounts))))
          }
        }.toSeq

        groupRow +: changerDetails.map ( mkSemiDetailedBalanceRow ) :+ blankLine
      }.toSeq

    val doc = mkDoc(
      xml.Group(
        mkTable(
          rows, Seq(trialBalanceHeader),
          Map(0 -> "proportional-column-width(3.0)"),
          Map(2 -> leftThinBorder, 4 -> leftThinBorder))),
      footer)

    doc
  }

  def mkDetailedBalanceReport = {
    val groupedPosts = currPosts.groupBy(_.name)

    val closingBalances = sortedAllAccountNames.map {
      case name =>
        val openingBalance = openingBalances.get(name).getOrElse(Zero)
        val gposts = groupedPosts.get(name).getOrElse(Nil)
        openingBalance + sumDeltas(gposts)
    }

    val totalRow = {
      val (debitPosts, creditPosts) = currPosts.partition(_.delta > Zero)

      Row(Seq(
        Cell("Totals"),
        mkBalanceCell(sum(openingBalances.values)),
        mkAmountCell(sumDeltas(debitPosts)),
        mkAmountCell(-sumDeltas(creditPosts)),
        mkBalanceCell(sum(closingBalances)))
      )
    }

    val significantAccountNames = sortedAllAccountNames.filter{ name =>
      groupedPosts.isDefinedAt(name) || openingBalances.get(name).getOrElse(Zero) != Zero
    }

    val accountHeads = significantAccountNames.map(x => augmentString(x).takeWhile(_ != ':')).toSet.toSeq.sorted

    println(accountHeads)

    val emptyLineSeparator = 
      Row(Seq.fill(5)(Cell("", rowPaddingBottomAttr)), topThinLineAttr)

    val rows =
      significantAccountNames.groupBy(augmentString(_).takeWhile(_ != ':')).flatMap {
        case (groupName, accountNames) =>
          val accountDetails = accountNames.map { name =>
            val gposts = groupedPosts.get(name).getOrElse(Nil)
            getDetails(name, gposts)
          }

          val groupRow = Row(Seq(
            Cell(groupName, boldAttr + ("font-size" -> "105%")),
            mkBalanceCell(accountDetails.map(_.openingBalance).sum),
            mkAmountCell(accountDetails.map(_.debitSubTotal).sum),
            mkAmountCell(accountDetails.map(_.creditSubTotal).sum),
            mkBalanceCell(accountDetails.map(_.closingBalance).sum)
            ), bottomVeryThinLineAttr ++ lightGreyBackground)

          groupRow +: accountDetails.map ( mkDetailedBalanceRow )  :+ emptyLineSeparator
      }.toSeq

    val doc = mkDoc(
      xml.Group(
        mkTable(
          rows :+ totalRow, Seq(trialBalanceHeader),
          Map(0 -> "proportional-column-width(3.0)"),
          Map(2 -> leftThinBorder, 4 -> leftThinBorder))),
      footer)

    doc
  }
}

trait ClassTransformer extends RewriteRule {
  def t(e: Elem): Seq[Node]
  val className: String

  override def transform(n: Node) = n match {
    case e: Elem if (e.attribOpt("class").map(_ == className).getOrElse(false)) =>
      t(e.copy(attributes = e.attributes.remove("class")))
    case _ => n
  }
}

class AttribTransformer(val className: String, attribs: Attributes) extends ClassTransformer {
  def t(e: Elem) = (e % attribs)
}
