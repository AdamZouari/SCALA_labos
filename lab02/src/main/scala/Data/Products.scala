package main.scala.Data

object Products {

  // TODO: step 2 - here your will have an attribute that will contain the products (e.g. "bière"), their types (e.g. "Boxer"), and their prices (e.g. 2.0).
  // TODO: step 2 - You will also have to find a way to store the default type/brand of a product.
  object ProductType extends Enumeration {
    type ProductType = Value
    val Beer, Croissant = Value

    def isProductType(s: String) = values.exists(_.toString == s)
  }
  import ProductType._

  val BeerList: Map[String, Int] =
             Map( "Boxer"       -> 1,
                  "Farmer"      -> 1,
                  "Wittekop"    -> 2,
                  "PunkIPA"     -> 3,
                  "Jackhammer"  -> 3,
                  "Ténébreuse"  -> 4)

  val CroissantList: Map[String, Int] =
              Map( "Maison"     -> 2,
                   "Cailler"    -> 2)

  val ProductsList : Map[ProductType.Value, Map[String, Int]] =
              Map(  Beer -> BeerList,
                    Croissant -> CroissantList)

  def defaultType(pT : ProductType.Value) : String = pT match {
      case Beer => "Boxer"
      case Croissant => "Maison"
  }


}
/*
  var beer = collection.mutable.Map("AL" -> "Alabama")
  //def apply(name: String) = s"$name--${Random.nextLong}"

  def apply(productType : String) =
  def unapply(customerID: String): Option[String] = {
    val stringArray: Array[String] = customerID.split("--")
    if (stringArray.tail.nonEmpty) Some(stringArray.head) else None
  }
}

val customer1ID = CustomerID("Sukyoung")  // Sukyoung--23098234908
customer1ID match {
case CustomerID(name) => println(name)  // prints Sukyoung
case _
*/
