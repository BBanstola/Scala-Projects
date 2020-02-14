// Preliminary Part about Code Similarity
//========================================


object CW7a {

  //(1) Complete the clean function below. It should find
  //    all words in a string using the regular expression
  //    \w+  and the library function
  //
  //         some_regex.findAllIn(some_string)
  //
  //    The words should be Returned as a list of strings.


  def clean(s: String): List[String] = {

    val wordPattern = "\\w+".r                // Defining regex to separate words from a string (\w+ seperates words from a string, .tolist puts the individual words in a list)

    wordPattern.findAllIn(s).toList            // Splitting the words and putting them in List
  }


  //(2) The function occurrences calculates the number of times
  //    strings occur in a list of strings. These occurrences should
  //    be calculated as a Map from strings to integers.

  // occurrences(List("a", "b", "b", "c", "d")) produces Map(a ‐> 1, b ‐> 2, c ‐> 1, d ‐> 1)

  def occurrences(xs: List[String]): Map[String, Int] = {

    xs.groupBy(l => l).map(t => (t._1, t._2.length))          // occurance of each word is calculated
  }

  //(3) This functions calculates the dot-product of two documents
  //    (list of strings). For this it calculates the occurrence
  //    maps from (2) and then multiplies the corresponding occurrences.
  //    If a string does not occur in a document, the product is zero.
  //    The function finally sums up all products.

  /* For each string, it multiplies the corresponding occurrences
  in each document. If a string does not occur in one of the documents,
  then the product for this string is zero*/

  def prod(lst1: List[String], lst2: List[String]): Double = {
    //val mergedMap = occurrences(lst1).map { case (k, v) => (k, v * occurrences(lst2).getOrElse(k, 0)) }               // Making a merged map combining two list where the key of each list will be present and their valued are multiplied if any

    //val dotProduct = mergedMap.foldLeft(0)(_+_._2)                                                                    // Adding all the values from the merged list

    occurrences(lst1).map { case (k, v) => (k, v * occurrences(lst2).getOrElse(k, 0)) }.foldLeft(0)(_ + _._2)           // above two steps in one
  }                                                                                                                     // foldleft adds the value

  //(4) Complete the functions overlap and similarity. The overlap of
  //    two documents is calculated by the formula given in the assignment
  //    description. The similarity of two strings is given by the overlap
  //    of the cleaned strings (see (1)).

  def max(lst1: List[String], lst2: List[String]): Double = {
    // Support function for finding max(d1^2,d2^2) for calculating overlap

    val max1 = occurrences(lst1).map { case (k, v) => (k, v * v) }.foldLeft(0)(_ + _._2) // Taking out max value from list 1 (summation of square value of the counts in d1)

    val max2 = occurrences(lst2).map { case (k, v) => (k, v * v) }.foldLeft(0)(_ + _._2) // Taking out max value from list 2

    if (max1 > max2) max1 else max2 // max(d1^2,d2^2)
  }

  // To calculate overlapping between two set of lists

  def overlap(lst1: List[String], lst2: List[String]): Double = prod(lst1, lst2) / max(lst1, lst2)


  // To calculate overlapping between two set of strings

  def similarity(s1: String, s2: String): Double = overlap(clean(s1), clean(s2))


  /* Test cases


val list1 = List("a", "b", "b", "c", "d")
val list2 = List("d", "b", "d", "b", "d")

occurrences(List("a", "b", "b", "c", "d"))   // Map(a -> 1, b -> 2, c -> 1, d -> 1)
occurrences(List("d", "b", "d", "b", "d"))   // Map(d -> 3, b -> 2)

prod(list1,list2) // 7

overlap(list1, list2)   // 0.5384615384615384
overlap(list2, list1)   // 0.5384615384615384
overlap(list1, list1)   // 1.0
overlap(list2, list2)   // 1.0

// Plagiarism examples from
// https://desales.libguides.com/avoidingplagiarism/examples

val orig1 = """There is a strong market demand for eco-tourism in
Australia. Its rich and diverse natural heritage ensures Australia's
capacity to attract international ecotourists and gives Australia a
comparative advantage in the highly competitive tourism industry."""

val plag1 = """There is a high market demand for eco-tourism in
Australia. Australia has a comparative advantage in the highly
competitive tourism industry due to its rich and varied natural
heritage which ensures Australia's capacity to attract international
ecotourists."""

similarity(orig1, plag1) // 0.8679245283018868


// Plagiarism examples from
// https://www.utc.edu/library/help/tutorials/plagiarism/examples-of-plagiarism.php

val orig2 = """No oil spill is entirely benign. Depending on timing and
location, even a relatively minor spill can cause significant harm to
individual organisms and entire populations. Oil spills can cause
impacts over a range of time scales, from days to years, or even
decades for certain spills. Impacts are typically divided into acute
(short-term) and chronic (long-term) effects. Both types are part of a
complicated and often controversial equation that is addressed after
an oil spill: ecosystem recovery."""

val plag2 = """There is no such thing as a "good" oil spill. If the
time and place are just right, even a small oil spill can cause damage
to sensitive ecosystems. Further, spills can cause harm days, months,
years, or even decades after they occur. Because of this, spills are
usually broken into short-term (acute) and long-term (chronic)
effects. Both of these types of harm must be addressed in ecosystem
recovery: a controversial tactic that is often implemented immediately
following an oil spill."""

overlap(clean(orig2), clean(plag2))  // 0.728
similarity(orig2, plag2)             // 0.728



// The punchline: everything above 0.6 looks suspicious and
// should be investigated by staff.

*/
}
