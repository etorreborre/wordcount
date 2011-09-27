package essays

import java.awt.{Font, Color}
import javax.swing.table.AbstractTableModel
import gui.{LabeledFieldPanel, PositionedBorderPanel}
import swing._
import Orientation._

class ResultsPanel(totalWordsNumber: TotalWordsNumbers = new TotalWordsNumbers(),
                   referencesTable: ReferencesTable = ReferencesTable(),
                   errors: ErrorMessageBox = ErrorMessageBox()) extends
  PositionedBorderPanel(north = totalWordsNumber, center = new ScrollPane(referencesTable), south = errors) {

  def countDone(r: Results) = {
    totalWordsNumber.display(r)
    referencesTable.display(r.references)
    errors.display(r.message)
  }
}

case class ErrorMessageBox() extends TextField {
  foreground = Color.red
  font = new Font("Courier", Font.PLAIN, 15);
  editable = false
  display()

  def display(message: String = "") = text = message
}

class TotalWordsNumbers(wordsNumber: WordsNumber = WordsNumber(),
                             fullText: FullTextNumber = FullTextNumber(),
                             refs: RefsNumber = RefsNumber(),
                             diffs: DiffsNumber = DiffsNumber()) extends
  PositionedBorderPanel(center = wordsNumber, south = new BoxPanel(Horizontal) { contents ++= Seq(fullText, refs, diffs) }) {

  def display(r: Results) = {
    wordsNumber.display(r)
    fullText.display(r)
    refs.display(r)
    diffs.display(r)
  }
}

case class FullTextNumber() extends LabeledFieldPanel("Full text") {
  text = "0"
  def display(r: Results) = text = r.fullTextWordsNumber.toString
}

case class RefsNumber() extends LabeledFieldPanel("References") {
  text = "0"
  def display(r: Results) = text = r.refsWordsNumber.toString
}

case class DiffsNumber() extends LabeledFieldPanel("Difference") {
  text = "0"
  def display(r: Results) = {
    text = r.diffsNumber.toString
    if (r.diffsNumber != 0) textField.foreground = Color.red
    else                    textField.foreground = Color.black
  }
}

case class WordsNumber() extends TextField {
  foreground = Color.blue
  font = new Font("Verdana", Font.PLAIN, 30);
  editable = false
  display()

  def display(results: Results = Results()) = text = " "+results.wordsNumber+" word"+(if (results.wordsNumber == 1) "" else "s")
}

case class ReferencesTable() extends Table() {
  val columnNames = Seq("Reference", "Year", "Page")
  display()
  
  def display(references: Seq[Reference] = Seq()) = {
    model = new AbstractTableModel {
      override def getColumnName(column: Int) = columnNames(column)
      def getRowCount() = references.size
      def getColumnCount() = columnNames.size
      def getValueAt(row: Int, col: Int): AnyRef = {
        val r = references(row)
        Seq(r.ref, r.years, r.pages.getOrElse(""))(col).asInstanceOf[AnyRef]
      }
      override def isCellEditable(row: Int, column: Int) = false
    }
  }
}