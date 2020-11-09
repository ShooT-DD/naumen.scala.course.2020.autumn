import scala.collection.mutable

class Table(x: Int, y: Int) {
  private val table = mutable.Map[Int, Cell]().withDefault(_ => new EmptyCell)

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix >= 0 && ix < x && iy >= 0 && iy < y) {
      val i = ix + iy * x
      Option(table(i))
    }
    else None
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && ix < x && iy >= 0 && iy < y) {
      val i = ix + iy * x
      table(i) = cell
    }
    else throw new IllegalArgumentException
  }
}
