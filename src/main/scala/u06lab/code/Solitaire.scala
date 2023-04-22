package u06lab.code
import scala.annotation.targetName

case class BoardSize(width: Int, height: Int)
case class Position(row: Int, col: Int)
case class Move(row: Int, col: Int)

case object Solitaire:
  private object Solitaire:
    type Board = Seq[Seq[Option[Int]]]
    val moves: List[Move] = List(Move(-3, 0), Move(-2, 2), Move(0, 3), Move(2, 2), Move(3, 0), Move(2, -2), Move(0, -3), Move(-2, -2))
    val emptyChar = "X"
  import Solitaire._

  extension (board: Board)
    @targetName("update")
    private def place(position: Position, num: Int): Board =
      board.updated(position.row, board(position.row).updated(position.col, Some(num)))

    private def isValidPosition(position: Position, size: BoardSize): Boolean = position match
      case Position(row, col) => (0 until size.height contains row) && (0 until size.width contains col) && board(row)(col).isEmpty


  extension (position: Position)
    @targetName("move")
    def add(move: Move): Position = Position(position.row + move.row, position.col + move.col)

  def apply(size: BoardSize): LazyList[Board] =
    val center = Position(size.height / 2, size.width / 2)
    val board = createEmptyBoard(size).place(center, 1)
    findAllSolutions(board, center, 1, size)

  def displaySolutions(solutions: LazyList[Board]): Unit =
    solutions.zipWithIndex.foreach { case (solution, index) =>
      println(s"Solution ${index + 1} \n ${render(solution)}")
    }

  private def createEmptyBoard(size: BoardSize): Board =
    Seq.fill(size.height)(Seq.fill(size.width)(None))

  private def findAllSolutions(board: Board, position: Position, count: Int = 1, size: BoardSize): LazyList[Board] =
    count match
      case marks if marks == size.width * size.height => LazyList(board)
      case _ =>
        for
          move <- moves.to(LazyList)
          next = position.add(move)
          if board.isValidPosition(next, size)
            solution <- findAllSolutions(board.place(next, count + 1), next, count + 1, size)
        yield solution

  private def render(board: Board): String =
    board.map(_.map(cell => cell.fold(emptyChar)("%2d".format(_))).mkString(" ")).mkString("\n")

@main def testSolitaire(): Unit =
  import Solitaire.*
  val size = BoardSize(5, 5)
  val solutions = Solitaire(size)
  displaySolutions(solutions)
