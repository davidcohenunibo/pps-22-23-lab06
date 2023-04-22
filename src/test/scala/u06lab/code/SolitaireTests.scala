package u06lab.code

import org.junit.Assert.*
import org.junit.Test

class SolitaireTests:

  import org.junit.Test
  import org.junit.Assert._
  object HelperSolitaireTest:
    def getBoard(boardString: String): List[List[Option[Int]]] =
      val boardList = boardString.trim.split("\n").map(_.trim.split("\\s+").toList).toList
      boardList.map(_.map(cell => if cell == "X" then None else Some(cell.toInt)))

  @Test def testEmptySolutions(): Unit =
    val size = BoardSize(3, 3)
    val solutions = Solitaire(size)
    assertEquals(List.empty, solutions)

  @Test def testSolutions(): Unit =
    import HelperSolitaireTest.getBoard
    val correctSolution = getBoard(
      "16  5  8 15  2\n" +
      "24 13 18 23 10\n " +
      "7 21  1  6 20\n" +
      "17  4  9 14  3\n" +
      "25 12 19 22 11")

    val failSolution = getBoard(
      " X  5 X X  2\n" +
        "X  8 X X  9\n" +
        "X X  1  6 X\n" +
        "X  4 X X  3\n" +
        "X  7 X X 10")

    val badSolution = getBoard(
      " X  5 4 X  2\n" +
        "X  8 1 1  9\n" +
        "X X  1  6 X\n" +
        "X  4 X X  3\n" +
        "2  3 X 1 10")

    val solutions = Solitaire(BoardSize(5, 5))
    assertTrue(solutions.contains(correctSolution))
    assertTrue(solutions.contains(failSolution))
    assertFalse(solutions.contains(badSolution))

