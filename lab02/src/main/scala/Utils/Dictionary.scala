package main.scala.Utils

/**
* Contains the dictionary of the application, which is used to validate, correct and normalize words entered by the
* user.
*/
object Dictionary {
  // This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
  // we want to normalize the words "veux" and "aimerais" in on unique term: "vouloir").
  val dictionary: Map[String, String] = Map(
    "bonjour" -> "bonjour",
    "hello" -> "bonjour",
    "yo" -> "bonjour",
    "je" -> "je",
    "j" -> "je",
    "le" -> "le",
    "de" -> "de",
    "me" -> "me",
    "m'" -> "me",
    "mon" -> "mon",
    "appelle" -> "appeller",
    "est" -> "etre",
    "suis" -> "etre",
    "veux" -> "vouloir",
    "aimerais" -> "vouloir",
    "voudrais" -> "vouloir",
    "commander" -> "commander",
    "connaitre" -> "connaître",
    "connaître" -> "connaître",
    "biere" -> "biere",
    "bière" -> "biere",
    "bières" -> "biere",
    "croissant" -> "croissant",
    "croissants" -> "croissant",
    "solde" -> "solde",
    "et" -> "et",
    "ou" -> "ou",
    "assoiffé" -> "assoiffe",
    "assoiffée" -> "assoiffe",
    "affamé" -> "affame",
    "affamée" -> "affame",
    "combien" -> "combien",
    "coûte" -> "valoir",
    "coûtent" -> "valoir",
    "quel" -> "quel",
    "prix" -> "prix"
  )
}
