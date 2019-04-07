package main.scala.Chat

import Tokens._
import main.scala.Data.Products
import main.scala.Utils.Dictionary.dictionary
import main.scala.Utils.SpellChecker._

class Tokenizer(input: String) {
  var tokens: Array[(String, Token)] = Array()
  var currentTokenIndex = -1

  private def getTokenFromString(s: String): Token = s match {
    case "bonjour" => BONJOUR
    case "je" => JE
    case "etre" => ETRE
    case "vouloir" => VOULOIR
    case "et" => ET
    case "ou" => OU
    case "biere" => BIERE
    case "croissant" => CROISSANT
    case "assoiffe" => ASSOIFFE
    case "affame" => AFFAME
    case p if p.startsWith("_") && p.length > 1 => PSEUDO // If the word starts with '_' and has more than one character it is a pseudonym.
    case n if n.forall(Character.isDigit) => NUM // If every character is a number, the word thus is a number.
    case "solde" => SOLDE
    case "valoir" => COUTER
    case p if Products.ProductsList(Products.ProductType.Beer).contains(p) ||
              Products.ProductsList(Products.ProductType.Croissant).contains(p)
          => MARQUE

      //balance
    case "mon" => ME
    case "connaître" => CONNAITRE
    case "solde" => SOLDE
      //commande
    case "commander" => COMMANDER
    case "biere" => BIERE
    case "croissant" => CROISSANT
    case "chips" => CHIPS

    case "et" => ET
    case "ou" => OU

    case "combien" => COMBIEN
    case _ => UNKNOWN
  }

  def tokenize(): Unit = {
    val words = input
      .trim()
      .replaceAll("[.|,|!|?|*]", " ") // Remove punctuation.
      .replaceAll(" +|[']", " ") // Remove multiple spaces and replace apostrophes by a space.
      .split(" ")

    // Get each word's occurence in the dictionary or check for the closest word if it is not contained in the dictionary.
    val fromDictionnary = words.map(w => dictionary.getOrElse(w, getClosestWordInDictionary(w)))

    tokens = fromDictionnary.map(t => (t, getTokenFromString(t)))
  }

  def nextToken(): (String, Token) = {
    currentTokenIndex += 1

    if (currentTokenIndex < tokens.size) {
      return tokens(currentTokenIndex)
    } else {
      return ("EOL", EOL)
    }
  }
}
