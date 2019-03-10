package Utils

object SpellChecker {
  /**
    * Calculate the Levenshtein distance between two words.
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenshtein distance between "s1" and "s2"
    */
  // TODO - Step 2
  // source https://oldfashionedsoftware.com/tag/levenshtein-distance/
  def stringDistance(s1: String, s2: String): Int = {

    val memo = scala.collection.mutable.Map[(List[Char],List[Char]),Int]()

    def min(a:Int, b:Int, c:Int) = Math.min( Math.min( a, b ), c)

    def sd(s1: List[Char], s2: List[Char]): Int = {
      if (memo.contains((s1,s2)) == false)
        memo((s1,s2)) = (s1, s2) match {
          case (_, Nil) => s1.length
          case (Nil, _) => s2.length
          case (c1::t1, c2::t2)  => min( sd(t1,s2) + 1, sd(s1,t2) + 1,
            sd(t1,t2) + (if (c1==c2) 0 else 1) )
        }
      memo((s1,s2))
    }
    sd( s1.toList, s2.toList )
  }

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number, this function just returns it.
    * @param misspelledWord the misspelled word to correct
    * @return the closest word from "misspelledWord"
    */
  // TODO - Step 2
  def getClosestWordInDictionary(misspelledWord: String): String = {

    // it's a pseudonym or a number
    if (misspelledWord.startsWith("_") || misspelledWord.matches("[0-9]+"))
      misspelledWord
    else{

      var distances = Map[String,Int]()
      for((k,v) <- Dictionary.dictionary )
        distances += (k -> stringDistance(misspelledWord,k))

      distances.toSeq.sortBy(_._2).head._1
    }
  }
}
