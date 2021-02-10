package org.iyunbo.coding
package progfun

abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat

  def -(that: Nat): Nat

  def toInt: Int

  override def toString: String = toInt.toString

  override def equals(obj: Any): Boolean = obj.isInstanceOf[Nat] && obj.asInstanceOf[Nat].toInt == this.toInt
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new IllegalArgumentException("0.predecessor")

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if (that.isZero) this else throw new IllegalArgumentException("negative number")

  override def toInt: Int = 0

}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if (that.isZero)
    this
  else
    this.predecessor - that.predecessor

  override def toInt: Int = this.predecessor.toInt + 1
}
