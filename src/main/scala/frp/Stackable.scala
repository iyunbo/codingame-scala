package org.iyunbo.coding
package frp

class Stackable[T](init: T) {
  private var values: List[T] = List[T](init)

  def value: T = values.head

  def withValue[R](newValue: T)(op: => R): R = {
    // push in stack
    values = newValue :: values
    // compute
    try
      op
    finally
    // pop out stack
      values = values.tail
  }
}
