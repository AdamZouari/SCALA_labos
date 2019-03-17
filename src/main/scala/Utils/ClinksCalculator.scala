package Utils

/**
  * Contains the functions necessary to calculate the number of *clinks* when n people want to cheers.
  */
object ClinksCalculator {


  /**
    * Calculate the factorial of a given number.
    *
    * @param n the number to compute
    * @return n!
    */
  def factorial(n: Int): Int = factorial(n,1)

  private def factorial(n: Int, a: Int): Int = n match {
    case finish if n == 0 => a
    case _ => factorial(n-1, n*a)
  }
  /**
    * Calculate the combination of two given numbers.
    * @param n the first number
    * @param k the second number
    * @return n choose k
    */
  def calculateCombination(n: Int, k: Int): Int = factorial(n) /(factorial(k) * factorial(n-k))
}
