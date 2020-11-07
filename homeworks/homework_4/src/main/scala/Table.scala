
class Table(col: Int, row: Int){
  val cells = scala.collection.mutable.MutableList[Cell]()
  for (i <- 0 to col*row) cells += new EmptyCell()

  def getCell(x: Int, y: Int): Option[Cell] = {
    if (x < 0 || y < 0 || x >= col || y >= row) return None
    cells.get(x + y * col)
  }

  def setCell(x: Int, y: Int, c: Cell): Unit = {
    if (x < 0 || y < 0 || x >= col || y >= row) {
      print("NotValidIndex")
      return
    }
    cells(x + y * col) = c
  }
}