package org.iyunbo.coding
package frp

class Var[T](exp: => T) extends Signal[T](exp) {
  override def update(e: => T): Unit = super.update(e)
}

object Var {
  def apply[T](exp: => T) = new Var(exp)
}
