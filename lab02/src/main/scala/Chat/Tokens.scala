package main.scala.Chat

object Tokens {
  type Token = Int

  // Terms
  val BONJOUR: Token     = 0
  val JE: Token          = 1
  val ME: Token          = 16
  val DE: Token          = 17
  val LE: Token          = 18
  val MON: Token         = 19
  val SOLDE : Token      = 27

  val QUEL: Token        = 20
  val PRIX: Token        = 21
  val COMBIEN: Token     = 22
  // Actions
  val ETRE: Token        = 2
  val VOULOIR: Token     = 3
  val CONNAITRE: Token   = 23
  val VALOIR: Token      = 24
  val COMMANDER: Token   = 25
  val COUTER : Token     = 28
  // Operators
  val ET: Token          = 4
  val OU: Token          = 5
  // Products
  val BIERE: Token       = 6
  val CROISSANT: Token   = 7
  val CHIPS: Token       = 8
  // Utils
  val PSEUDO: Token      = 9
  val NUM: Token         = 10
  val UNKNOWN: Token     = 11
  val EOL: Token         = 12
  // Test
  val ASSOIFFE: Token    = 13
  val AFFAME: Token      = 14
  val MARQUE: Token      = 15

}
