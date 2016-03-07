package com.koddsson
/*
 * kennitala.scala
 *
 * Various utils for Icelandic social security numbers.
 *
 */

object Kennitala {
  def isValid(kennitala: String): Boolean = {
    if(kennitala.length != 10) {
      return false;
    }

    val spreadKennitala = kennitala.map(_.toInt - 48)
    val randomNumbers = List(3, 2, 7, 6, 5, 4, 3, 2)

    val sum =
      (spreadKennitala, randomNumbers).zipped map (_ * _) sum

    var varNumber = 11 - (sum % 11)

    if (varNumber == 11) {
      varNumber = 0
    } else if (varNumber == 10) {
      return false
    }

    varNumber == (kennitala(8).toInt - 48)
  }
}
