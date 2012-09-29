package org.proofcafe.coqdoc

object Util {
  def isFillInHere(str : String) = {
    val s = str.trim()
    s == "FILL IN HERE" || s == "ここを埋めなさい"
  }
}
