package org.iyunbo.coding
package frp

import frp.Signal.caller

class Signal[T](exp: => T) {
  private var expr: () => T = _
  private var va: T = _
  private var observers: Set[Signal[_]] = Set()

  update(exp)

  def apply(): T = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    va
  }

  protected def update(e: => T): Unit = {
    expr = () => e
    computeValue()
  }

  protected def computeValue(): Unit = {
    val newVa = caller.withValue(this)(expr())
    if (newVa != va) {
      va = newVa
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }
}

object Signal {
  private val caller = new Stackable[Signal[_]](NoSignal)

  def apply[T](exp: => T) = new Signal(exp)
}

object NoSignal extends Signal[Nothing](???) {
  override protected def computeValue(): Unit = ()
}