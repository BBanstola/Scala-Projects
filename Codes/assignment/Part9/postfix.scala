// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object CW9a {

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList


// (1) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.
//
// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  
// 

// for checking if the ops contains the operator or not
def is_op(op: String) : Boolean = if (ops.contains(op)) true else false

// checking for precedence order
def prec(op1: String, op2: String) : Boolean = if (precs.get(op1).isDefined && precs.get(op2).isDefined && precs(op1) >= precs(op2)) true else false

//recursive function for arranging the order of expressions in postfix
def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
	case heads::tails if heads == "(" => syard(tails, heads::st, out)
	case heads::tails if heads == ")" => syard(tails, st.drop(st.indexOf("(")+1),out:::st.take(st.indexOf("(")))
	case heads::tails if is_op(heads) == true => {
		if (st.nonEmpty && prec(st.head, heads)) {
			val high_prec = st.filter(!prec(_,heads))
			syard(tails, heads::high_prec, out :::st.diff(high_prec))
		}
		else syard(tails, heads::st, out)
	}
	case heads::tails => syard(tails, st, out :+ heads)
	case _ => out :::st
}


// test cases
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (2) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as arguments. The function should produce the
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.    

// for computing the expressions
def compute(toks: Toks, st: List[Int] = Nil) : Int = toks match {
	case heads::tails if is_op(heads) => heads match {
		case "+" => compute(tails, (st(1) + st.head) :: st.drop(2))
		case "-" => compute(tails, (st(1) - st.head) :: st.drop(2))
		case "*" => compute(tails, (st(1) * st.head) :: st.drop(2))
		case "/" => compute(tails, (st(1) / st.head) :: st.drop(2))
	}
	case heads::tails => compute(tails, heads.toInt :: st)
	case Nil => st.head
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}


