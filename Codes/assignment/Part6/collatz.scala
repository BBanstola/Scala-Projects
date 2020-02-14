// Basic Part about the 3n+1 conjecture
//======================================

object CW6a {

//(1) Complete the collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed, you can use an auxiliary function that
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million.

//def collatz(n: Long) : Long = ...

  def collatz(n: Long):Long ={
    require(n > 0 , "Number must be positive")                        // checking for basic requirement
    def steps(n:Long, count:Long):Long={                                // recursive function
      if (n == 1) count                                                 // function ending requirement
      else if (n % 2 == 0) steps(n/2, count + 1)                        // calculation in case of the number being even
      else  steps(n*3 +1, count + 1)                                    // calculation in case of the number being odd 
    }
    steps(n,0)                                                          // calling the recursive function
  }

//(2) Complete the collatz_max function below. It should
//    calculate how many steps are needed for each number 
//    from 1 up to a bound and then calculate the maximum number of
//    steps and the corresponding number that needs that many 
//    steps. Again, you should expect bounds in the range of 1
//    up to 1 Million. The first component of the pair is
//    the maximum number of steps and the second is the 
//    corresponding number.

//def collatz_max(bnd: Long) : (Long, Long) = ...

  def collatz_max(bnd: Long) : (Long, Long) = {
    require(bnd  >  0, "The bound should be a positive number")
    def calculate(bnd :Long, ans: (Long,Long)): (Long, Long) = {            // recursive function
      if (bnd == 1) ans                                                     // loop ending criteria
      else {
        if (collatz(bnd) > ans._1) calculate(bnd-1,(collatz(bnd),bnd))      // recurring process and calculating and assignment of values in ans
        else calculate(bnd-1,ans)                                           // looping through 
        }
      }
    calculate(bnd,(0,0))                                                    // calling the recursive function calculate
    }

// Test Case

//    def main(args: Array[String]): Unit = {
//      val ans1 =  collatz(9)                                                // Checking the no of steps required for 9
//      println ("Steps required : "+ans1)
//      val ans2 = collatz_max(1000000)                                       // Testing for a boundary between 1 and the number 10000000
//     println("Number: "+ans2._2 + " Steps required: "+ans2._1)
  //}
}

