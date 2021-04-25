package org.iyunbo.coding
package frp

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BankAccountTest extends AnyFlatSpec with should.Matchers {

  it should "deposit and withdraw money from a bank account" in {
    val account = new BackAccount
    account deposit 10
    account deposit 150

    account.balance() should be(160)

    account withdraw 90
    account.balance() should be(70)

    the [Error] thrownBy account.withdraw(90) should have message "insufficient funds"
  }
}
