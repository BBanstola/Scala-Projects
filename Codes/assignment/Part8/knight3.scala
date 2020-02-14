import scala.annotation.tailrec
// Finding a single tour on a "mega" board
//=========================================

object CW8c {

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

  // From earlier files

  def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
    if (x._1 < dim && x._1 >= 0) {                                              // if the x coordinate of the Pos x lies between the board or not
      if (x._2 < dim && x._2 >= 0) {                                            // if the y coordinate of the Pos x lies between the board or not
        if (!(path contains x)) true else false                                // Third condition of the question
      } else false
    }
    else  false
  }


  def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    List((1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2))                // All possible moves of a knight in a board in a clockwise manner
      .map(point => (point._1+x._1, point._2+x._2))                                             // mapping the list's element into the positions in the chess board
      .filter(is_legal(dim, path, _))                                                       // filtering only the answer that is legal i.e. the only one which do not go out of the board
  }

  def getLength(dim: Int, path: Path, x: Pos) = legal_moves(dim,path,x).length


  def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val list_moves = legal_moves(dim,path,x)
    val appended_moves = for (move <- list_moves) yield{
      getLength(dim, move::path, move)}
    (list_moves zip appended_moves).toMap.toList.sortBy(each => each._2).map(each => each._1)
  }

  def time_needed[T](code: => T) : T = {
    val start = System.nanoTime()
    val result = code
    val end = System.nanoTime()
    println(f"Time needed: ${(end - start) / 1.0e9}%3.3f secs.")
    result
  }

//(9) Implement a function that searches for a single tour on a "mega" board
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 seconds.


/*
  def recursive_heuristics(dim: Int, path: Path, stack: List[Path]): Option[Path] = stack match {
    case Nil => None
    case head :: remains => if (head.size == dim * dim) Some(head)
    else recursive_heuristics(dim, path, ordered_moves(dim, head, head.head).map(_ :: head) )
  }
*/



def tour_on_mega_board(dim: Int, path: Path): Option[Path] = {
  @tailrec
  def recursive_aux(dim: Int, path: Path, stack: List[Path]): Option[Path] = stack match {
    case Nil => None
    case heads :: tails =>
      val mov = ordered_moves(dim, heads, heads.head)
      if (heads.size == math.pow(dim,2).toInt) Some(heads)
      else recursive_aux(dim, path, mov .map(_ :: heads) )
  }
  recursive_aux(dim, path, path::Nil)
}

  //tour_on_mega_board(70, List((0,0)))

  //time_needed(tour_on_mega_board(70, List((0,0))))

}
