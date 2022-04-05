import scala.:+

object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = seq.foldLeft(List.empty[T])((result, current) => current +: result)

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = idx match {
    case 0 | 1 => idx
    case _ => fibonacci4Index(idx - 1) + fibonacci4Index(idx - 2)
  }

  def fibonacci(idx: Int): Seq[Int] = idx match {
    case 0 => Seq(0)
    case _ => fibonacci(idx - 1) :+ fibonacci4Index(idx)
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = text
    .map(x => MORSE.getOrElse(x.toUpper.toString, x))
    .mkString(" ")

  private val wordsRegexp = "(?U)\\w+".r

  def wordReverse(text: String): String = wordsRegexp
    .replaceAllIn(text, e => e.group(0) match {
      case x if x.charAt(0).isUpper => x.toLowerCase.reverse.capitalize
      case x => x.reverse
    })
}
