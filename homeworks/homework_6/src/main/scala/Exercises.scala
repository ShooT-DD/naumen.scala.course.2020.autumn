object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = seq.reverse

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = {
    if (idx == 0)
      0
    else if (idx <= 2)
      1
    else
      fibonacci4Index(idx - 1) + fibonacci4Index(idx - 2)
  }

  def fibonacci(idx: Int): Seq[Int] = {
    for (i <- 0 to idx)
      yield fibonacci4Index(i)
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = {
    val res = text.toUpperCase.split("").toSeq.map(s =>
      if(MORSE.contains(s))
        " " + MORSE(s)
      else if(s == " ") "  "
      else s ).mkString("")
    if(res.charAt(0) == ' ')
      res.substring(1)
    else res
  }


  def wordReverse(text: String): String = {
    text.split("(?=[!. ,?])|(?<=[!. ,?])")
      .map(word =>
        if (word.charAt(0).isUpper) {
          word.toLowerCase.reverse.capitalize
      }
        else word.reverse).mkString("")
  }

}
