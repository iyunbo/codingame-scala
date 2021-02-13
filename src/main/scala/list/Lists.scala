package org.iyunbo.coding
package list

object Lists {

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => List()
    case y :: ys => y match {
      case z: Int => z :: flatten(ys)
      case a :: as => a :: flatten(as) ::: flatten(ys)
    }
  }

}
