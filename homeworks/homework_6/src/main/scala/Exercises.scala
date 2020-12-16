object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = {
    seq.reverse
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = {
    if (idx > 1)
      return fibonacci4Index(idx-1) + fibonacci4Index(idx - 2)
    if (idx == 1)
      return 1
    0
  }

  def fibonacci(idx: Int): Seq[Int] = {
    if (idx == 0)
      return Seq[Int](0)

    var fibSeq = Seq[Int](0, 1)
    for (i <- 2 to idx) {
      fibSeq :+= fibSeq.apply(i-1) + fibSeq.apply(i-2)
    }
    fibSeq
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def getMorzeCod(symbol: Char): String ={
    if (symbol == ' ')
      return "  "
    if (MORSE.contains(symbol.toString))
      return " " + MORSE(symbol.toString)
    symbol.toString()
  }
  def morse(text: String): String = {
    val mText = text.toUpperCase.map(s => getMorzeCod(s))
    if (MORSE.contains(text(0).toString))
      mText.mkString("").replaceFirst(" ", "")
    else mText.mkString("")
  }

  def wordReverse(text: String): String = {
    text.split("(?=[!. ,?])|(?<=[ ])")
      .map(w =>
        if (w(0).isUpper)
          w.toLowerCase.reverse.capitalize
        else w.reverse).mkString("")
  }

}
