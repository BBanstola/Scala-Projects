// Shunting Yard Algorithm
// including Associativity for Operators 
// =====================================

object CW9b {


// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		  "*" -> 2,
		  "/" -> 2,
      "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// (3) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.

// checking if the operator is valid
def is_op(op: String): Boolean = if (ops.contains(op)) true else false

def rightPrec(op1: String, op2: String) : Boolean = if (precs.get(op1).isDefined && precs.get(op2).isDefined && precs(op1) > precs(op2)) true else false

def leftPrec(op1: String, op2: String) : Boolean = if (precs.get(op1).isDefined && precs.get(op2).isDefined && precs(op1) >= precs(op2)) true else false

// def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = ...

  def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
    case heads::tails if heads == "(" => syard(tails, heads::st, out)
    case heads::tails if heads == ")" => syard(tails, st.drop(st.indexOf("(")+1),out:::st.take(st.indexOf("(")))
    case heads::tails if is_op(heads) == true => {
      if (assoc(heads) == RA) {

        if (st.nonEmpty && rightPrec(st.head, heads)) {
        val low_prec = st.filter(rightPrec(_,heads))
        val high_prec = st. diff(low_prec)
        syard(tails, heads::high_prec, out :::low_prec)
      }
        else syard(tails, heads::st, out)
      }
      else {
        if (st.nonEmpty && leftPrec(st.head, heads)) {
          val low_prec = st.filter(leftPrec(_,heads))
          val high_prec = st. diff(low_prec)
          syard(tails, heads::high_prec, out :::low_prec)
        }
        else syard(tails, heads::st, out)
      }
    }
    case heads::tails => syard(tails, st, out :+ heads)
    case _ => out :::st
  }

// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (4) Implement a compute function that produces a Long(!) for an
// input list of tokens in postfix notation.

//def compute(toks: Toks, st: List[Long] = Nil) : Long = ...

def compute(toks: Toks, st: List[Long] = Nil) : Long = toks match {
    case heads::tails if is_op(heads) => heads match {
      case "+" => compute(tails, (st(1) + st.head) :: st.drop(2))
      case "-" => compute(tails, (st(1) - st.head) :: st.drop(2))
      case "*" => compute(tails, (st(1) * st.head) :: st.drop(2))
      case "/" => compute(tails, (st(1) / st.head) :: st.drop(2))
      case "^" => compute(tails, math.pow(st(1),st.head).toLong :: st.drop(2))
    }
    case heads::tails => compute(tails, heads.toLong :: st)
    case Nil => st.head
}

// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}
