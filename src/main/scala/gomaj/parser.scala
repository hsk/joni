import util.parsing.combinator._

abstract class Row
case class HeaderRow(cells: List[String]) extends Row
case class DataRow(cells: List[String]) extends Row

object CsvParser3 extends RegexParsers {
  override val whiteSpace = "[ \t\n\r]+".r
  def eol = opt('\r') <~ '\n'
  def notQuotedCell = "[^\"\r\n,]*".r
  def quotedCell = "\"" ~> "[^\"]*".r <~ "\""
  def row = repsep((quotedCell | notQuotedCell), ",")

  def rows = repsep(row, eol) ^^ {
    case cellsList => {
      cellsList match {
        case Nil => Nil
        case hCells :: dCellsList => {
          val hRow = new HeaderRow(hCells)
          val dRows = dCellsList filter {
            // remove empty line
            case l => l.size != 1 || ! l(0).trim.isEmpty
          } map {
            case dr => new DataRow(dr)
          }
          hRow :: dRows
        }
      }
    }
  }

  def parse(input: String): ParseResult[List[Row]] = {
    parseAll(rows, input)
  }
}

object Main extends App {
  val csv3 = """"name",age,"memo"
  "Andy",20,"Skills
  - English,Chenese
  - Java,Scala
  "
  "Brian",22,
  "Charles",33,
  ,33,
  """

  val res_csv3 = CsvParser3.parse(csv3)
  println(res_csv3)


}
