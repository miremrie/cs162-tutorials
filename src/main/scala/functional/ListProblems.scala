
package edu.ucsb.cs.cs162.tuts.functional

// A number of list problems.
object ListProblems {

	// Sums all the odd numbers in the list.
	def sumOdd(list: List[Int]): Int = list.filter(_ % 2 == 1).foldLeft(0)(_ + _)

	// Sums the two lists pairwise. 
	// Requires that the two lists are of the same length.
	// Examples: (1, 2, 3) + (4, 5, 6) = (5, 7, 9) 
	// Hint: look at List.zip and List.map in the Scala list documentation
    def sumPairs(left: List[Int], right: List[Int]): List[Int] =
    if (left.length != right.length) throw new IllegalArgumentException()
    else (left zip right).map((x) => x._1 + x._2)

	// Gets the penultimate element of a list safely, returning 
	//  `None` if there's no such element.
	// Examples: (1, 2, 3) -- penultimate --> 2
	// Hint: List.foldLeft can be useful here.
    def safePenultimate(list: List[Int]): Option[Int] =
    list.zipWithIndex.foldLeft(Option.empty[Int])
    {
        case (agr, x) =>
            if (x._2 == list.length - 2) Some(x._1)
            else agr
    }
}
