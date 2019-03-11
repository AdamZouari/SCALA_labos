package Chat

import Tokens._
import Utils.{Dictionary, SpellChecker}

import scala.collection.mutable.ListBuffer


class Tokenizer(input: String) {

  val cleaned = input.replaceAll("[.,!?*]", "").replaceAll("[']", " ").replaceAll("[ ]{2,}", " ").split(" ")
  var tokenized = ListBuffer[(String,Tokens.Token)]()

  /**
    * Separate the user's input into tokens.
    */
  def tokenize(): Unit = {

    for (x <- cleaned) {
      val word = x match {
        case dico if Dictionary.dictionary.contains(x) => Dictionary.dictionary(x)
        case numberOrPseudo if SpellChecker.isNumber(x) || SpellChecker.isPseudo(x) =>  x
        case _ => Dictionary.dictionary(SpellChecker.getClosestWordInDictionary(x))
      }
      tokenized += wordToToken(word)
    }
  }

  /**
    * Transform a word into its corresponding token.
    * @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  def wordToToken(word: String): (String, Tokens.Token) = word match {

    case "bonjour" => (word, Tokens.BONJOUR)
    case "je" => (word, Tokens.JE)
    case "etre" => (word, Tokens.ETRE)
    case "vouloir" => (word, Tokens.VOULOIR)
    case "biere" => (word, Tokens.BIERE)
    case "croissant" => (word, Tokens.CROISSANT)
    case "et" => (word, Tokens.ET)
    case "ou" => (word, Tokens.OU)
    case pseudo if SpellChecker.isPseudo(word) => (word, Tokens.PSEUDO)
    case number if SpellChecker.isNumber(word) => (word, Tokens.NUM)
    case _ => (word, Tokens.UNKNOWN)
  }

  /**
    * Get the next token of the user input, or OEL if there is no more token.
  	* @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  def nextToken(): (String, Token) = tokenized match {

    case empty if tokenized.isEmpty => ("eol",Tokens.EOL)
    case _ => {
      val token = tokenized.head
      tokenized = tokenized.tail
      token
    }
  }
}
