package org.iyunbo.coding
package list

object Lists {

  def flatten(xs: List[Any]): List[Any] = {
    xs match {
      case List() => List()
      case List(zs: List[Any]) => flatten(zs)
      case y :: ys => y match {
        case i: Int => i :: flatten(ys)
        case a :: as => a :: flatten(as) ::: flatten(ys)
      }
    }
  }

}
