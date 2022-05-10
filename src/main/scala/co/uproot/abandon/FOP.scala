package co.uproot.abandon

import java.io.StringReader
import java.io.FileOutputStream
import org.apache.fop.apps.FopFactory
import org.apache.fop.apps.FopFactoryBuilder
import org.apache.fop.configuration.DefaultConfigurationBuilder
import org.xml.sax.helpers.XMLReaderFactory
import org.xml.sax.helpers.XMLReaderAdapter
import org.xml.sax.InputSource
import scala.xml.NodeSeq
import scala.xml.Node
import scala.xml.Elem
import scala.xml.Atom

object FOP {
  implicit class nodeHelper(val n: Node) {
    def attrib(s: String) = attribOpt(s).get
    def attribOpt(s: String) = n.attribute(s).map(_.apply(0).toString)
    def text = n.child.toString
  }
  implicit class elemHelper(val n: Elem) {
    def addAttrib(k: String, v: String) = n % new xml.UnprefixedAttribute(k, v, xml.Null)
    def %(attrs: Map[String, String]): Elem = {
      val seq = for ((n, v) <- attrs) yield new xml.UnprefixedAttribute(n, v, xml.Null)
      (seq.foldLeft(n))(_ % _)
    }
    def %(attr: (String, String)): Elem = {
      val (ns,v) = attr
      val a = new xml.UnprefixedAttribute(ns, v, xml.Null)
      n % a
    }
  }
  implicit def str2NodeSeq(s: String):Node = xml.Text(s)

  type Attributes = Map[String, String]
  val emptyAttribs = Map.empty[String, String]

  case class Cell(content: Node, attribs: Attributes = emptyAttribs, cellAttribs: Attributes = emptyAttribs) {
    def render(padding:String) = {
        <fo:table-cell padding-top={ padding } padding-bottom={ padding }>{
          mkBlock(content) % attribs
        }</fo:table-cell> % cellAttribs
    }

    def % (moreAttribs: Attributes) = copy(cellAttribs = cellAttribs ++ moreAttribs)
  }

  case class Row(cells: Seq[Cell], attribs: Attributes = emptyAttribs) {
    def render = mkRow(cells, "1pt") % attribs
  }

  def mkPDF(fName: String, fo: xml.NodeSeq) = {
    val outStream = new FileOutputStream(fName)
    // val factory = FopFactory.newInstance
    // factory.setUserConfig("fop_config.xml")

    val cfgBuilder = new DefaultConfigurationBuilder()
    // val cfg = cfgBuilder.buildFromFile(new java.io.File("fop_config.xml"));
    val cfg = cfgBuilder.build(getClass.getResourceAsStream("/fop_config.xml"))
    val fopFactoryBuilder = new FopFactoryBuilder(new java.net.URI("file://fop_config.xml")).setConfiguration(cfg)
    val factory = fopFactoryBuilder.build
    val foUA = factory.newFOUserAgent()
    Logging.setup(fName, foUA)
    val fop = factory.newFop("application/pdf", foUA, outStream)

    val inputStream = new StringReader(fo.toString)
    val reader = XMLReaderFactory.createXMLReader()
    val readerAdapter = new XMLReaderAdapter(reader)
    val handler = fop.getDefaultHandler
    reader.setContentHandler(handler)
    reader.parse(new InputSource(inputStream))
    outStream.close
    inputStream.close
  }

  def mkRow(cols: Seq[Cell], padding: String) = {
    <fo:table-row keep-together.within-page="always">{
      cols.map { _.render(padding) }
    }</fo:table-row>
  }

  def mkTable(rows: Seq[Row], headers: Seq[Row] = Nil, columnSizes: Map[Int, String] = Map.empty, columnAttrs:Map[Int, Attributes] = Map.empty) = {
    val numColumns = rows.headOption.map(_.cells.length).getOrElse(0)
    val columnDeclarations = (0 until numColumns) map { i =>
      val columnSize = columnSizes.get(i).getOrElse("proportional-column-width(1)")
      val attribs = columnAttrs.get(i).getOrElse(emptyAttribs)
      <fo:table-column column-width={ columnSize }/> % attribs
    }

    // table-omit-header-at-break="true">

    <fo:table width="100%" table-layout="fixed" border-collapse="collapse">
      { columnDeclarations }
      {
        if (headers.isEmpty) {
          xml.Null
        } else {
          <fo:table-header>{
            headers.map(_.render)
          }</fo:table-header>
        }
      }
      <fo:table-body>{
        rows.map(_.render)
      }</fo:table-body>
    </fo:table>
  }

  def mkDoc(body: Node, after: Node, mirrored: Boolean = true, fontSize: String = "80%") = {
    val pageSeq =
      <fo:page-sequence font-family="Ubuntu Mono" font-size={fontSize} >
        <fo:static-content flow-name="xsl-region-after">{
          after
        }</fo:static-content>
        <fo:flow flow-name="xsl-region-body">{
          body
        }</fo:flow>
      </fo:page-sequence>;

    val masterRefAttr = ("master-reference" -> (if (mirrored) "oddEvenMaster" else "oddPageMaster"))
    val pageSeqMastered = pageSeq % masterRefAttr

    <fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
      <fo:layout-master-set>
        <fo:simple-page-master master-name="oddPageMaster" page-height="29.7cm" page-width="21cm"
          margin-top="0cm" margin-bottom="0cm" margin-left="0.7cm" margin-right="0.5cm">
          <fo:region-body margin-top=".5cm" margin-bottom="2cm"/>
          <fo:region-after extent="1cm"/>
        </fo:simple-page-master>
        <fo:simple-page-master master-name="evenPageMaster" page-height="29.7cm" page-width="21cm"
          margin-top="0cm" margin-bottom="0cm" margin-left=".5cm" margin-right="0.7cm">
          <fo:region-body margin-top=".5cm" margin-bottom="2cm"/>
          <fo:region-after extent="1cm"/>
        </fo:simple-page-master>

        <fo:page-sequence-master master-name="oddEvenMaster">
          <fo:repeatable-page-master-alternatives>
            <fo:conditional-page-master-reference master-reference="oddPageMaster" odd-or-even="odd" />
            <fo:conditional-page-master-reference master-reference="evenPageMaster" odd-or-even="even" />
          </fo:repeatable-page-master-alternatives>
        </fo:page-sequence-master>
      </fo:layout-master-set>
      {pageSeqMastered}
    </fo:root>
  }

  def mkBlock(content: NodeSeq) = {
    <fo:block>{
      content
    }</fo:block>
  }
  def mkBlockContainer(content: NodeSeq) = {
    <fo:block-container>{
      content
    }</fo:block-container>
  }

  def mkHeading(level: Int, title: String) = {
    <fo:block space-before=".15cm" font-weight="bold" text-align="left" font-size="115%">{ title }</fo:block>
  }

  def mkInline(s:String, attrs: Attributes) = {
    <fo:inline>{s}</fo:inline> % attrs
  }
  def mkInline(n:NodeSeq, attrs: Attributes = emptyAttribs) = {
    <fo:inline>{n}</fo:inline> % attrs
  }
}
