package org.iyunbo.coding
package frp

class BackAccount {

  val balance: Var[Int] = Var(0)

  def deposit(amount: Int): Unit = {
    if (amount > 0) {
      val current = balance()
      balance() = current + amount
    }
  }

  def withdraw(amount: Int): Unit = {
    val current = balance()
    if (0 < amount && current > amount) {
      balance() = current - amount;
    } else {
      throw new Error("insufficient funds")
    }
  }
}
