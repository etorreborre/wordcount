package essays

import java.awt.{Font, Color}
import javax.swing.table.AbstractTableModel
import gui.{LabeledFieldPanel, PositionedBorderPanel}
import swing._
import event.SelectionEvent
import Orientation._
import reactive._

class ResultsPanel(totalWordsNumber: TotalWordsNumbers, referencesTable: ReferencesTable, errors: ErrorMessageBox) extends
  PositionedBorderPanel(north = totalWordsNumber, center = new ScrollPane(referencesTable), south = errors) with Observing

object ResultsPanel {
  def apply(results: EventStream[Results]) = {
    new ResultsPanel(TotalWordsNumbers(results),
                     ReferencesTable(results.map(_.references)),
                     ErrorMessageBox(results.map(_.message)))
  }
}

case class ErrorMessageBox(message: EventStream[String] = new EventSource[String]) extends TextField with Observing {
  foreground = Color.red
  font = new Font("Courier", Font.PLAIN, 15);
  editable = false
  message.foreach { m => text = m }
}

class TotalWordsNumbers(wordsNumber: WordsNumber,
                        fullText: FullTextNumber,
                        refs: RefsNumber        ,
                        diffs: DiffsNumber      ) extends PositionedBorderPanel(center = wordsNumber, south = new BoxPanel(Horizontal) { contents ++= Seq(fullText, refs, diffs) })

object TotalWordsNumbers {
  def apply(results: EventStream[Results]) = {
    new TotalWordsNumbers(
      WordsNumber(results.map(_.wordsNumber)),
      FullTextNumber(results.map(_.fullTextWordsNumber)),
      RefsNumber(results.map(_.refsWordsNumber)),
      DiffsNumber(results.map(_.diffsNumber))
    )
  }
}

case class FullTextNumber(wordsNumber: EventStream[Int]) extends LabeledFieldPanel("Full text", wordsNumber.map(_.toString))
case class RefsNumber(refsNumber: EventStream[Int]) extends LabeledFieldPanel("References", refsNumber.map(_.toString))
case class DiffsNumber(diffsNumber: EventStream[Int]) extends LabeledFieldPanel("Difference", diffsNumber.map(_.toString)) with Observing {
  diffsNumber.foreach { d =>
    if (d != 0) textField.foreground = Color.red
    else        textField.foreground = Color.black
  }
}

case class WordsNumber(wordsNumber: EventStream[Int]) extends TextField with Observing {
  foreground = Color.blue
  font = new Font("Verdana", Font.PLAIN, 30);
  editable = false

  wordsNumber.foreach { n => text = " "+n+" word"+(if (n == 1) "" else "s") }
}

case class ReferencesTable(references: EventStream[Seq[Reference]]) extends Table() with Observing {
  val columnNames = Seq("Reference", "Year", "Page")

  references.foreach { refs =>
    model = new AbstractTableModel {
      override def getColumnName(column: Int) = columnNames(column)
      def getRowCount() = refs.size
      def getColumnCount() = columnNames.size
      def getValueAt(row: Int, col: Int): AnyRef = {
        val r = refs(row)
        Seq(if (r.ref.isEmpty) r.quotation else r.ref, r.years, r.pages.getOrElse(""))(col).asInstanceOf[AnyRef]
      }
      override def isCellEditable(row: Int, column: Int) = false
    }
  }
}