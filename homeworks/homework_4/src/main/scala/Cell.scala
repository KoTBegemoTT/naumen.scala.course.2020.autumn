trait  Cell

class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

class EmptyCell() extends Cell() {
  override def toString: String = "empty"
}

class StringCell(str: String) extends Cell(){
  override def toString: String = str
}

class ReferenceCell(x: Int, y: Int, t: Table) extends Cell(){
  override def toString: String = {
    getString(this, this)
  }

  def getNextCell(): Option[Cell] = {
    t.getCell(x, y)
  }

  //позволяет находить любые циклы, а не только вида first->ref->first
  def getString(first: ReferenceCell, curent: ReferenceCell): String ={
    val nextCell = curent.getNextCell()
    nextCell match {
      case None => "outOfRange"
      case Some(cell: ReferenceCell) =>
        if (cell == first)  "cyclic"
        else getString(first, cell)
      case Some(cell: Cell) => cell.toString
    }
  }
}