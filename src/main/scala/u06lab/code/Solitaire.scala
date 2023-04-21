package u06lab.code

object Solitaire:
  private type Position = (Int, Int)
  private type Move = (Int, Int)
  private type Cell = Option[Int]
  private type Board = Seq[Seq[Cell]]

  private object Constants {
    val moves: List[Move] = List((-3, 0), (-2, 2), (0, 3), (2, 2), (3, 0), (2, -2), (0, -3), (-2, -2))
  }

  import Constants._

  extension (board: Board)
    private def placeMark(position: Position, num: Int): Board =
      board.updated(position._1, board(position._1).updated(position._2, Some(num)))

    private def isValid(position: Position, width: Int, height: Int): Boolean =
      (0 until height contains position._1) &&
        (0 until width contains position._2) &&
        board(position._1)(position._2).isEmpty


  extension (position: Position)
    def +(move: Move): Position = (position._1 + move._1, position._2 + move._2)


  def placeMarks(width: Int, height: Int): LazyList[Board] =
    val initialPosition = (height / 2, width / 2)
    val initialBoard = LazyList.fill(height)(LazyList.fill(width)(None: Option[Int])).placeMark(initialPosition, 1)
    findAllSolutions(initialBoard, initialPosition, 1, width, height)

  private def findAllSolutions(board: Board, position: Position, count: Int, width: Int, height: Int): LazyList[Board] =
    if (count == height * width) LazyList(board)
    else
      LazyList.from(moves)
        .map(move => position + move)
        .filter(nextPosition => board.isValid(nextPosition, width, height))
        .flatMap(nextPosition => findAllSolutions(board.placeMark(nextPosition, count + 1), nextPosition, count + 1, width, height))

  def displaySolutions(solutions: LazyList[Board]): Unit =
    solutions.zipWithIndex.foreach { case (solution, index) =>
      println(s"Solution ${index + 1}")
      println(render(solution))
    }
    println(s"Found ${solutions.length} solution(s)!")

  private def render(board: Board): String =
    board
      .map(row => row.map(cell => cell.fold("X")("%2d".format(_))).mkString(" "))
      .mkString("\n")


@main def testSolitaire(): Unit =
  val width = 7
  val height = 5
  val solutions = Solitaire.placeMarks(width, height)
  Solitaire.displaySolutions(solutions)
