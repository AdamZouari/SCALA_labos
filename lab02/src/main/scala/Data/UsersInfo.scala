package main.scala.Data


object UsersInfo {

  // Will contain the name of the currently active user; default value is null.
  private var _activeUser: String = _

  // TODO: step 2 - create an attribute that will contain each user and its current balance.


  private val initialMoney = 30

  private var accounts = collection.mutable.Map[String, Double]().withDefaultValue(initialMoney)


  /**
    * Update an account by decreasing its balance.
    * @param user the user whose account will be updated
    * @param amount the amount to decrease
    * @return the new balance
    */
  // TODO: step 2
  def purchase(user: String, amount: Double): Double = {
    //no check if the user has enough money : we authorize credit,
    // best way to control our users.
    if( accounts.isDefinedAt(user) )
      accounts(user) -= amount

    accounts(user)
  }

  private def add(user: String) : Any = {

    if( ! accounts.isDefinedAt(user) )
      accounts(user) = initialMoney

  }

  def login(user: String) : Any = {
    add(user)
    _activeUser = user
  }

  def isLoggedIn() : Boolean = {
    _activeUser != null
  }

  def getCurrentUser() : String = {
    _activeUser
  }

  def balance(user: String) : Double = {
    accounts(user)
  }
}

