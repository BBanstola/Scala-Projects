// Core Part about a "Compiler" for the Brainf*** language
//======================================================

import io.Source
import scala.util._

object CW10b {


// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


  val alphabet = List("A", "B", "C", "D", "E", "F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

  val filler1 = """[^<>+-.,\[\]]"""
  val filler2 = """\[-\]"""

  // auxillary function
  def modification(str : String, start : Int, end: Int, find : Char) : (String, Int) = {
    if(end-start == 25){
      (str.substring(0, start) + find + alphabet(end-start)+ str.substring(end+1, str.length), end-(end-start)+1)
    }
    else{
      if(end < str.length - 1){
        if(str(end + 1) == find){
          modification(str, start, end+1, find)
        }
        else{
          (str.substring(0, start) + find + alphabet(end-start)+ str.substring(end+1, str.length), end-(end-start)+1)
        }
      }
      else{
        ( str.substring(0, start) + find + alphabet(end-start)+ str.substring(end+1, str.length), end-(end-start)+1)
      }
    }
  }

  // From file bf.scala

  def load_bff(name: String) : String = {
    try (Source.fromFile(name).mkString)
    catch{
      case e: Exception => println(e)
        ""
    }
  }

  def sread(mem: Mem, mp: Int) : Int = mem.getOrElse(mp,0)

  def write(mem: Mem, mp: Int, v: Int) : Mem = mem + (mp -> v)

  def jumpRight(prog: String, pc: Int, level: Int) : Int = (prog.length > pc+1) match {
    case true =>{
      prog(pc + 1) match {
        case '[' => jumpRight(prog,pc + 1, level + 1)
        case ']' if level ==0 => pc + 2
        case ']' => jumpRight(prog, pc + 1, level - 1)
        case _ => jumpRight(prog, pc + 1 ,level)
      }
    }
    case _ => prog.length
  }

  def jumpLeft(prog: String, pc: Int, level: Int) : Int = (pc >= 0) match {
    case true =>{
      prog(pc) match {
        case '[' if level == 0 => pc + 1
        case '['  => jumpLeft(prog, pc - 1, level - 1)
        case ']' => jumpLeft(prog, pc - 1, level + 1)
        case _ => jumpLeft(prog, pc - 1 ,level)
      }
    }
    case _ => pc
  }



// DEBUGGING INFORMATION FOR COMPILERS!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. One way to debug them is to run
// example programs ``unoptimised''; and then optimised. Does
// the optimised version still produce the same result?


// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}

type Mem = Map[Int, Int]

// TASKS
//=======

// (5) Write a function jtable that precomputes the "jump
//     table" for a bf-program. This function takes a bf-program 
//     as an argument and Returns a Map[Int, Int]. The 
//     purpose of this map is to record the information about
//     pc positions where '[' or a ']' are stored. The information
//     is to which pc-position do we need to jump next?
// 
//     For example for the program
//    
//       "+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]"
//
//     we obtain the map
//
//       Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)
//  
//     This states that for the '[' on position 5, we need to
//     jump to position 20, which is just after the corresponding ']'.
//     Similarly, for the ']' on position 19, we need to jump to
//     position 6, which is just after the '[' on position 5, and so
//     on. The idea is to not calculate this information each time
//     we hit a bracket, but just look up this information in the 
//     jtable. You can use the jumpLeft and jumpRight functions
//     from Part 1 for calculating the jtable.
//
//     Then adapt the compute and run functions from Part 1 
//     in order to take advantage of the information stored in the jtable. 
//     This means whenever jumpLeft and jumpRight was called previously,
//     you should immediately look up the jump address in the jtable.


//def jtable(pg: String) : Map[Int, Int] = ...

  def jtable(pg: String) : Map[Int, Int] =
  (0 until pg.length).collect{ case pivot => pg(pivot) match {
    case '[' => Some(pivot -> jumpRight(pg, pivot + 1  , 0))
    case ']' => Some(pivot -> jumpLeft(pg, pivot - 1, 0))
    case _ => None
  }}.flatten.toMap


  // testcase
//
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)


//def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = ...

  def compute2(prog: String,table: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
    if(pc == prog.length) mem
    else if(pc == -1) mem
    else{
      prog(pc) match{
        case '>'                        => compute2(prog,table, pc+1, mp+1,mem)
        case '<'                        => compute2(prog,table, pc+1, mp-1, mem)
        case '+'                        => compute2(prog,table, pc+1, mp, write(mem, mp, sread(mem, mp)+1))
        case '-'                        => compute2(prog,table, pc+1, mp, write(mem, mp, sread(mem, mp)-1))
        case '.'                        => print(sread(mem,mp).toChar);compute2(prog,table, pc+1,mp,mem)
        case ','                        => compute2(prog,table, pc+1, mp, write(mem, mp, Console.in.read().toByte))
        case '[' if sread(mem, mp) == 0 => compute2(prog,table, table(pc), mp,mem)
        case '['                        => compute2(prog,table, pc+1, mp,mem)
        case ']' if sread(mem, mp) != 0 => compute2(prog,table,table(pc), mp, mem)
        case ']'                        => compute2(prog,table, pc+1,mp, mem)
        case _                          => compute2(prog,table, pc+1, mp, mem)
      }
    }
  }

  //def run2(pg: String, m: Mem = Map()) = ...

  def run2(prog: String, m: Mem = Map()) = compute2(prog, jtable(prog), 0, 0, m)


// testcases
// time_needed(1, run2(load_bff("benchmark.bf")))
// time_needed(1, run2(load_bff("seirpinski.bf")))


// (6) Write a function optimise which deletes "dead code" (everything
// that is not a bf-command) and also replaces substrings of the form
// [-] by a new command 0. The idea is that the loop [-] just resets the
// memory at the current location to 0. In the compute3 and run3 functions
// below you implement this command by writing the number 0 to mem(mp), 
// that is write(mem, mp, 0). 
//
// The easiest way to modify a string in this way is to use the regular
// expression """[^<>+-.,\[\]]""", which recognises everything that is 
// not a bf-command and replace it by the empty string. Similarly the
// regular expression """\[-\]""" finds all occurrences of [-] and 
// by using the Scala method .replaceAll you can replace it with the 
// string "0" standing for the new bf-command.



//def optimise(s: String) : String = ...

def optimise(s: String) : String = s.replaceAll(filler1,"").replaceAll(filler2,"0")

//def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = ...

  def compute3(prog: String,table: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
    if (pc == -1) mem
    else if (pc == prog.length) mem
    else {
      prog(pc) match{
        case '0'                        => compute3(prog, table, pc+1, mp, write(mem, mp, 0))
        case '>'                        => compute3(prog,table, pc+1, mp+1,mem)
        case '<'                        => compute3(prog,table, pc+1, mp-1, mem)
        case '+'                        => compute3(prog,table, pc+1, mp, write(mem, mp, sread(mem, mp)+1))
        case '-'                        => compute3(prog,table, pc+1, mp, write(mem, mp, sread(mem, mp)-1))
        case '.'                        => print(sread(mem,mp).toChar);compute3(prog,table, pc+1,mp,mem)
        case ','                        => compute3(prog,table, pc+1, mp, write(mem, mp, Console.in.read().toByte))
        case '[' if sread(mem, mp) == 0 => compute3(prog,table, table(pc), mp,mem)
        case '['                        => compute3(prog,table, pc+1, mp,mem)
        case ']' if sread(mem, mp) != 0 => compute3(prog,table,table(pc), mp, mem)
        case ']'                        => compute3(prog,table, pc+1,mp, mem)
        case _                          => compute3(prog,table, pc+1, mp, mem)
      }
    }

  }

//def run3(pg: String, m: Mem = Map()) = ...
  def run3(pg: String, m: Mem = Map()) = compute3(optimise(pg), jtable(optimise(pg)), 0, 0, m)

// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11203
// 
// time_needed(1, run3(load_bff("benchmark.bf")))



// (7)  Write a function combine which replaces sequences
// of repeated increment and decrement commands by appropriate
// two-character commands. For example for sequences of +
//
//              orig bf-cmds  | replacement
//            ------------------------------
//              +             | +A 
//              ++            | +B
//              +++           | +C
//                            |
//              ...           |
//                            | 
//              +++....+++    | +Z
//                (where length = 26)
//
//  Similar for the bf-command -, > and <. All other commands should
//  be unaffected by this change.
//
//  Adapt the compute4 and run4 functions such that they can deal
//  appropriately with such two-character commands.

/*val combo : List[Tuple2[String, Int]] = List(("A",1),("B",2),("C",3),("D",4),("E",5),("F",6),("G",7), ("H",8),("I",9),("J",10),
                                      ("K",11),("L",12),("M",13),("N",14),("O",15),("P",16),("Q",17),("R",18),("S",19),
                                      ("T",20),("U",21),("V",22),("W",23),("X",24),("Y",25),("Z",26))*/


//def combine(s: String) : String = ...

  def combine(s: String) : String = {

    def combination(s: String, i : Int) : String = {
        if (i+1 <= s.length){
          val sign = s(i)
          if(List('<','>','+','-').contains(sign)){
            val n = modification(s, i, i, s(i))
            combination(n._1, n._2+1)
          }
          else{
            combination(s, i+1)
          }
        }
        else s
    }

    combination(s,0)
  }

// testcase
// combine(load_bff("benchmark.bf"))


//def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = ...

  def compute4(prog: String, table: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
    if (pc == prog.length) mem
    else if (pc == -1) mem
    else{
      prog(pc) match{
        case '0'                        => compute4(prog, table, pc+1, mp, write(mem, mp, 0))
        case '>'                        => compute4(prog,table, pc+1, mp+(prog(pc + 1) - '@'),mem)
        case '<'                        => compute4(prog,table, pc+1, mp-(prog(pc + 1) - '@'), mem)
        case '+'                        => compute4(prog,table, pc+1, mp, write(mem, mp, sread(mem, mp)+(prog(pc + 1) - '@')))
        case '-'                        => compute4(prog,table, pc+1, mp, write(mem, mp, sread(mem, mp)-(prog(pc + 1) - '@')))
        case '.'                        => print(sread(mem,mp).toChar);compute4(prog,table, pc+1,mp,mem)
        case ','                        => compute4(prog,table, pc+1, mp, write(mem, mp, Console.in.read().toByte))
        case '[' if sread(mem, mp) == 0 => compute4(prog,table, table(pc), mp,mem)
        case '['                        => compute4(prog,table, pc+1, mp,mem)
        case ']' if sread(mem, mp) != 0 => compute4(prog,table,table(pc), mp, mem)
        case ']'                        => compute4(prog,table, pc+1,mp, mem)
        case _                          => compute4(prog,table, pc+1, mp, mem)
      }
    }
  }

// should call first optimise and then combine on the input string
//
//def run4(pg: String, m: Mem = Map()) = ...

  def run4(pg: String, m: Mem = Map()) ={
    val opt = combine(optimise(pg))
    compute4(opt, jtable(opt), 0, 0, m)
  }
  //def run4(pg: String, m: Mem = Map()) = compute4(combine(optimise(pg)), jtable(combine(optimise(pg))), 0, 0, m)

// testcases
// combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

// testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))
}
