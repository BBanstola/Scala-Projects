import scala.annotation.tailrec
// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object CW8b {


// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

  // Functions from knight1

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

  def first(xs: List[Pos], f: Pos => Option[Path]): Option[Path] = xs match{

    case Nil => None
    case position :: x =>
      val value = f(position)
      if(value.isDefined) value else first(x,f)
  }

  def first_tour(dim: Int, path: Path): Option[Path] = {

    if(path.length == dim*dim) Option(path) else
      first(legal_moves(dim,path,path.head), (position:Pos)=>first_tour(dim,position::path))}

  //(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.

  def close_aux(pos: Pos) : Path ={
    List( (pos._1+1,pos._2+2) ,
      (pos._1+2,pos._2+1),
      (pos._1+2,pos._2-1),
      (pos._1+1,pos._2-2),
      (pos._1-1,pos._2-2),
      (pos._1-2,pos._2-1),
      (pos._1-2,pos._2+1),
      (pos._1-1,pos._2+2))}

  /*

  def sort_aux(pos1: Pos, pos2: Pos, dim: Int, path: Path): Boolean =
    if (legal_moves(dim, path, pos1).size < legal_moves(dim, path, pos2).size) true else false

  def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val legalMoves = legal_moves(dim, path, x)
    legalMoves.sortWith(sort_aux(_, _, dim, path))
  }
*/


  def getLength(dim: Int, path: Path, x: Pos) = legal_moves(dim,path,x).length


  def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val list_moves = legal_moves(dim,path,x)
    val appended_moves = for (move <- list_moves) yield{
      getLength(dim, move::path, move)}
    (list_moves zip appended_moves).toMap.toList.sortBy(each => each._2).map(each => each._1)
  }


//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 


  def range_aux2(pos1: Pos, pos2: Pos, dim: Int) : Boolean = {
    if (legal_moves(dim, Nil, pos2).contains(pos1)) true else false
  }
/*
  def first_closed_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
    val expected_moves = ordered_moves(dim, path, path.head)

    first(expected_moves, (p:Pos) =>{
      val new_path = p::path
      if(new_path.length == math.pow(dim,2).toInt && ordered_moves(dim, new_path, p).isEmpty){
        if(range_aux2(p,new_path.head,dim)){
          Some(new_path)
        }
        else None
      }
      else first_tour(dim, new_path)
    })
  }*/


  def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {

    if ( close_aux(path.head).contains(path.last) && path.length == math.pow(dim,2).toInt) Some (path) else{
      val first_closed_tour_heuristic_result : Option[Path] = first(ordered_moves(dim, path, path.head),
                                      new_path => first_closed_tour_heuristics(dim, new_path :: path)  )
      first_closed_tour_heuristic_result
    }
  }


  // first_closed_tour_heuristic(6, List((3,3)))

  //(8) Same as (7) but searches for *non-closed* tours. This
//    version of the function will be called with dimensions of 
//    up to 30 * 30.


  def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = {

    @tailrec
    def aux1(dim: Int, path_list: List[Path]): Option[Path] = path_list match {
      case Nil => None
      case heads :: tails => if (dim * dim == heads.size) Some(heads)
      else aux1(dim, ordered_moves(dim, heads, heads.head).map(_ :: heads) ::: tails)
    }
    aux1(dim, List(path))
  }

  //  first_tour_heuristics(8, List((0,0)))
  // first_tour_heuristics(30, List((0,0)))


}
