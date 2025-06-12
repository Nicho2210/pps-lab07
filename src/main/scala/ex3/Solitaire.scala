package ex3

object Solitaire extends App:
  type Mark = (Int, Int)
  type Solution = Iterable[Mark]
  type IterableFactory = Solution => Iterable[Solution]
  given IterableFactory = LazyList(_)
  private val width = 5
  private val height = 5
  private val initialMark: Mark = (width / 2, height / 2)

  @main def run(): Unit = placeMarks().zipWithIndex foreach render

  def placeMarks(n: Int = width * height)(using factory: IterableFactory): Iterable[Solution] = n match
    case 1 => factory(Seq(initialMark))
    case _ =>
      for
        marks <- placeMarks(n - 1)
        x <- 0 until width
        y <- 0 until height
        mark = (x, y)
        if isFree(mark, marks)
        if isValid(mark, marks)
      yield
        marks.toSeq :+ mark

  def render(solution: (Solution, Int)): Unit =
    println(s"\nSolution ${solution._2 + 1}")
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
                    number = solution._1.toSeq.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    println(rows.mkString("\n"))

  private def isFree(mark: Mark, marks: Iterable[Mark]): Boolean =
    isInBounds(mark) && !marks.exists(_ == mark)

  private def isInBounds(mark: Mark): Boolean =
    mark._1 >= 0 && mark._1 < width && mark._2 >= 0 && mark._2 < height

  private def isValid(mark: Mark, marks: Iterable[Mark]): Boolean =
    (mark._1 == marks.last._1 && (mark._2 - marks.last._2).abs == 3) ||
      (mark._2 == marks.last._2 && (mark._1 - marks.last._1).abs == 3) ||
      ((mark._1 - marks.last._1).abs == (mark._2 - marks.last._2).abs && (mark._1 - marks.last._1).abs == 2)