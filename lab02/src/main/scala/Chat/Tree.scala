package main.scala.Chat

import main.scala.Data.Products
import main.scala.Data.UsersInfo
// TODO - step 3
object Tree {

  /**
    * This sealed trait represents a node of the tree and contains methods to compute it and write its output text in console.
    */
  sealed trait ExprTree {
    /**
      * Compute the price of the current node, then returns it. If the node is not a computational node, the method
      * returns 0.0.
      * For example if we had a "+" node, we would add the values of its two children, then return the result.
      * @return the result of the computation
      */
    def computePrice: Double = this match {
      case Article(typeProduct, brand) => Products.ProductsList(typeProduct)(brand)
      case Articles(article, n) => n * article.computePrice
      case and(leftOp, rightOp) => leftOp.computePrice + rightOp.computePrice
      case or(leftOp, rightOp) => Math.min(leftOp.computePrice, rightOp.computePrice)
      case _ => 0
    }

    /**
      * Return the output text of the current node, in order to write it in console.
      * @return the output text of the current node
      */
    def reply: String = this match {
      // Example cases
      case Thirsty() => "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry() => "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"

      case Authenticate(user) => {
        UsersInfo.login(user)
        "Bienvenue, " + user + "."
      }

      case priceForItems(items) =>
        "Cette selection vous revient à CHF" + items.computePrice + "."

      case order => {
        if(UsersInfo.isLoggedIn()){
          order match {
            case takeOrder(articles) => {
              val price = articles.computePrice
              val newBalance = UsersInfo.purchase(UsersInfo.getCurrentUser(), articles.computePrice)
              "Votre commande '" + articles.toString +
              "' vous coûte CHF" + price.toString +
              ", vous disposez encore de " + newBalance.toString + "."

            }

            case currentBalance() =>
              "Votre solde actuel est de " +
                UsersInfo.balance(UsersInfo.getCurrentUser()) +
              "."
          }
        }else "Veuillez vous identifier."
      }
    }
  }

  /**
    * Declarations of the nodes' types.
    */
  // Example cases
  case class Thirsty() extends ExprTree
  case class Hungry() extends ExprTree

  case class Authenticate(name: String) extends ExprTree

  case class takeOrder(items : ExprTree) extends ExprTree

  case class currentBalance() extends ExprTree
  case class priceForItems(items : ExprTree) extends ExprTree

  case class and(f: ExprTree, s: ExprTree) extends ExprTree {
    override def toString: String = f.toString + " et " + s.toString
  }
  case class or(f: ExprTree, s: ExprTree) extends ExprTree {
    override def toString: String =
      if (f.computePrice <= s.computePrice)
        f.toString
      else s.toString
  }

  case class Articles(article: Article, amount: Int) extends ExprTree {
    override def toString: String = amount.toString + " " + article.toString
  }
  case class Article(productType: Products.ProductType.Value, var brand: String ) extends ExprTree {

    if(brand == null || brand.isEmpty )
      brand = Products.defaultType(productType)

    override def toString: String = {
      productType.toString + " " + brand
    }
  }
}
