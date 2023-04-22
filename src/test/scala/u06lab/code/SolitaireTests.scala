package u06lab.code

import org.junit.Assert.*
import org.junit.Test

class SolitaireTests {

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
      val board = getBoard(
        "16  5  8 15  2\n" +
        "24 13 18 23 10\n " +
        "7 21  1  6 20\n" +
        "17  4  9 14  3\n" +
        "25 12 19 22 11")
      val solutions = Solitaire(BoardSize(5, 5)).take(2)
      assertTrue(solutions.contains(board))


}
