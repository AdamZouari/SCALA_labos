package main.scala.Chat

import Tokens._
import Tree._
import main.scala.Data.Products

// TODO - step 4
class Parser(tokenizer: Tokenizer) {
  import tokenizer._

  var curTuple: (String, Token) = ("unknown", UNKNOWN)
  
  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit = curTuple = nextToken()

  /** "Eats" the expected token, or terminates with an error. */
  private def eat(token: Token): Unit = if (token == curToken) readToken() else expected(token)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
  // TODO (BONUS): find a way to display the string value of the tokens (e.g. "BIERE") instead of their integer value (e.g. 6).
  private def expected(token: Token, more: Token*): Nothing =
    fatalError(" expected: " +
      (token :: more.toList).mkString(" or ") +
      ", found: " + curToken)

  def fatalError(msg: String): Nothing = {
    println("Fatal error", msg)
    new Exception().printStackTrace()
    sys.exit(1)
  }

  /** the root method of the parser: parses an entry phrase */
  def parsePhrases() : ExprTree = {
    if (curToken == BONJOUR) eat(BONJOUR)
    if (curToken == JE ) {
      eat(JE)
      if(curToken == ETRE){
        eat(ETRE)
        if (curToken == ASSOIFFE) {
          // Here we do not "eat" the token, because we want to have a custom 2-parameters "expected" if the user gave a wrong token.
          readToken()
          Thirsty()
        }
        else if (curToken == AFFAME) {
          readToken()
          Hungry()
        }
        else if (curToken == PSEUDO){
          val currUser = curValue.substring(1).toLowerCase()
          eat(PSEUDO)
          Authenticate(currUser)
        }
        else expected(ASSOIFFE, AFFAME, PSEUDO)

      }
      else if(curToken == VOULOIR){
        eat(VOULOIR)
        if(curToken == COMMANDER){
          eat(COMMANDER)
          takeOrder(parseOrder())
        }
        else if (curToken == CONNAITRE){
          eat(CONNAITRE)
          eat(ME)
          eat(SOLDE)
          currentBalance()
        }
        else expected(CONNAITRE, COMMANDER)
      }
      else expected(ETRE, VOULOIR)
    }

    else if (curToken == COMBIEN){
      eat(COMBIEN)
      eat(COUTER)
      priceForItems(parseOrder())
    }
    else expected(BONJOUR, JE, COMBIEN)
  }

  def parseOrder() : ExprTree = {
    var expr : ExprTree = parseArticle()

    curToken match {
      case ET => {
        eat(ET)
        and(expr, parseOrder())
      }
      case OU => {
        eat(OU)
        or(expr, parseOrder())
      }
      case _ => expr
    }
  }

  def parseArticle() : ExprTree = {
    var nProducts : Int = 1
    var brand : String = null
    var typeProduct : Products.ProductType.Value = null

    if(curToken == NUM) {
      nProducts = curValue.toInt
      eat(NUM)

    }else{ //if we don't have a number, assume one
      readToken()
    }

    //check if we have a correct type of product
 //   if(Products.ProductType.isProductType(curValue)){
    if(curToken == BIERE) {
      typeProduct = Products.ProductType.Beer
      eat(BIERE)
    }else if (curToken == CROISSANT) {
      typeProduct = Products.ProductType.Croissant
      eat(CROISSANT)

    }else expected(BIERE, CROISSANT, CHIPS)

    //set by-default brand
    brand = Products.defaultType(typeProduct)
   // readToken()

    //set brand if user has given one
    if(curToken == MARQUE){
      brand = curValue
      eat(MARQUE)
    }
   // if(Products.ProductsList(typeProduct).contains() )
   // if(Products.ProductType.)

    //if(Products.ProductsList)
    Articles(Article(typeProduct,brand), nProducts)
  }

  // Start the process by reading the first token.
  readToken()
}
