import scala.xml._
import java.io._

object Formatter {
  
  def main(args: Array[String]) {

    val root = XML.load(new InputStreamReader(new FileInputStream(args(0)), "UTF-8"))
    val annotated = addIds(root)
    val handler = new NodeHandler(annotated)
    val output = 
      <html>
        <head>
          <meta charset="UTF-8" />
          <title>The Ninth Age</title>
          <script src="jquery.js" type="text/javascript"></script>
          <script src="color.jquery.js" type="text/javascript"></script>
          <script src="rulebook.js" type="text/javascript"></script>
          <link rel="stylesheet" type="text/css" href="rulebook.css" />
        </head>
        <body>
          <div class="toc">
            { tableOfContent(annotated) }
          </div>
          { handler.content() }
        </body>
      </html>

    print(output)
  }


  private var unique: Int = 1
  private def getUnique: Int = synchronized {
    unique += 1
    unique
  }

  private val toAnnotate = Set("chapter", "section")

  def addIds(node: Node): Node = node match {
    case e: Elem if toAnnotate.contains(e.label) && !e.attribute("id").isDefined => {
      e.copy(
        child = e.child.flatMap(addIds(_)), 
        attributes = new UnprefixedAttribute("id", "sec-" + getUnique, e.attributes))
    }
    case e: Elem => e.copy(child = e.child.flatMap(addIds(_)))
    case _ => node
  }

  def tableOfContent(root: Node): NodeSeq = {
    val buffer = new NodeBuffer
    for (chapter <- root \\ "chapter") {
      buffer += <a href={ "#" + chapter \@ "id" } class="to_chapter">{ chapter \@ "title" }</a>
      for (section <- chapter \\ "section") {
        buffer += <a href={ "#" + section \@ "id" } class="to_section">{ section \@ "title" }</a>
      }
    }
    buffer
  }

  class NodeHandler(root: Node) {

    def content(): NodeSeq = {
      val buffer = new NodeBuffer
      handle(root, buffer)
      buffer
    }

    private def handle(node: Node, buffer: NodeBuffer): NodeBuffer = {
      node.label match {
        case "definition" => {
          val name = node \@ "name"
          buffer += <span class="definition" id={ "ref-" + name }>{
            val inner = new NodeBuffer 
            node.child.foreach(handle(_, inner))
            inner
          }</span>
        }
        case "document" => {
          node.child.foreach(handle(_, buffer))
        }
        case "chapter" => {
          buffer += <div class="chapter">
          {
            val inner = new NodeBuffer 
            inner += <h1 id={ node \@ "id" }>{ node \@ "title" }</h1>
            node.child.foreach(handle(_, inner))
            inner
          }
          </div>
        }
        case "section" => {
          buffer += <div class="section">
          {
            val inner = new NodeBuffer 
            inner += <h2 id={ node \@ "id" }>{ node \@ "title" }</h2>
            node.child.foreach(handle(_, inner))
            inner
          }
          </div>
        }
        case "subsection" => {
          buffer += <div class="subsection">
          {
            val inner = new NodeBuffer 
            inner += <h3 id={ node \@ "id" }>{ node \@ "title" }</h3>
            node.child.foreach(handle(_, inner))
            inner
          }
          </div>
        }
        case "note" => {
          buffer += <div class="note">
          {
            val inner = new NodeBuffer 
            inner += <h4 id={ node \@ "id" }>{ node \@ "title" }</h4>
            node.child.foreach(handle(_, inner))
            inner
          }
          </div>
        }
        case "reference" => {
          val name = node \@ "name"
          buffer += <a href={ "#ref-" + name } class="reference">{ 
              node.child.map(handle(_, new NodeBuffer())) 
            }</a>
        }
        case "figure" => {
          val id = node \@ "id"
          buffer += <div class="figure" id={id}>
            { node.child.map(handle(_, new NodeBuffer())) }
            </div>
        }
        case "image" => {
          val source = node \@ "source"
          buffer += <img src={ "images/" + source} />
        }
        case "caption" => {
          buffer += <h4>{ node \@ "title" }</h4>
          node.child.foreach(handle(_, buffer))
        }
        case "sequence" => {
          buffer += <ol>
            { node.child.map(handle(_, new NodeBuffer)) }
          </ol>
        }
        case "item" if node.attribute("title").isDefined => {
          buffer += <li>
            <b>{node \@ "title"}</b><br />
            { node.child.map(handle(_, new NodeBuffer)) }
          </li>
        }
        case "item" => {
          buffer += <li>
            { node.child.map(handle(_, new NodeBuffer)) }
          </li>
        }
        case "important" => {
          buffer += <b>
            { node.child.map(handle(_, new NodeBuffer)) }
          </b>
        }
        case "illustration" => {
          buffer += <div class="illustration">
            { node.child.map(handle(_, new NodeBuffer)) }
          </div>
        }
        case "#PCDATA" => {
          val (pre, in, post) = splitString(node.text)
          buffer += Text(pre)
          buffer ++= intercalate(in.split("\n").map(Text(_)), <br />) 
          buffer += Text(post)
        }
        case "link" => {
          val to = node \@ "to"
          buffer += <a href={ "#" + to } class="link">{ 
            node.child.map(handle(_, new NodeBuffer))
          }</a>
        }
        case "table" => {
          if (node.attribute("title").isDefined) {
            buffer += <h5 class="table_title">{ node \@ "title" }</h5>
          }
          buffer += <table>
            { node.child.map(handle(_, new NodeBuffer)) }
          </table>
        }
        case "headers" => {
          buffer += <tr class="headers"> {
            val inner = new NodeBuffer
            if (node.attribute("title").isDefined) {
              inner += <th>{ node \@ "title" }</th>
            }
            node.child.foreach(handle(_, inner))
            inner
          } </tr>
        }
        case "header" => {
          buffer += <th>{
            node.child.map(handle(_, new NodeBuffer))
          }</th>
        }
        case "row" => {
          buffer += <tr> {
            val inner = new NodeBuffer
            val title = node.attribute("title") 
            if (title.isDefined && !title.get.isEmpty) {
              inner += <th>{ title.get.head.text }</th>
            }
            node.child.foreach(handle(_, inner))
            inner
          } </tr>
        }
        case "cell" => {
          buffer += <td>
            { node.child.map(handle(_, new NodeBuffer)) }
          </td>
        }
        case "list" => {
          buffer += <ul>
            { node.child.map(handle(_, new NodeBuffer)) }
          </ul>
        }
        case "br" => {
          buffer += <br />
          buffer += <br />
        }
        case _ => {
          node.child.foreach(handle(_, buffer))
        }
      }
      buffer
    }
  }

  def splitString(str: String): (String, String, String) = {
    val (pre, rest) = str.span(_.isWhitespace)
    val i = rest.lastIndexWhere(!_.isWhitespace)
    val (in, post) = rest.splitAt(i)
    (pre, in, post)
  }

  def intercalate[A](list: Seq[A], inter: A): Seq[A] = list match {
    case Seq() => list
    case Seq(x) => list
    case Seq(x, xs @ _*) => x +: inter +: intercalate(xs, inter)
  }
}