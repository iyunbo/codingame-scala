package org.iyunbo.coding
package frp

import frp.Signal.caller


class Signal[T](exp: => T) {
  private var expr: () => T = _
  private var va: T = _
  private var observers: Set[Signal[_]] = Set()

  update(exp)

  //catches cyclic signal s() = s() + 1, If you don't add this assert, then infinite
  //recursion and stackoverflow
  def apply(): T = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    va
  }

  //update method gets called during the initialization of the Signal or
  //someone calls an update operation on a Var, or the value of a dependent
  ///signal changes.
  //"protected" means only subclasses of Signal has access to this method
  //clients of the Signal cannot. This means clients of Signal cannot call update
  protected def update(e: => T): Unit = {
    expr = () => e
    computeValue()
  }

  protected def computeValue(): Unit = {
    // this will add the current Signal as the last caller
    val newVa = caller.withValue(this)(expr())
    //re-evaluating the callers
    if (newVa != va) {
      va = newVa
      val obs = observers
      observers = Set()
      // all caller should update their values
      obs.foreach(_.computeValue())
    }
  }
}

object Signal {

  import scala.util.DynamicVariable

  //private val caller = new StackableVariable[Signal[_]](NoSignal)    // <--global variable

  /* global variable caller! Global variables in concurrency is bad idea (results in
   * race conditions). One way to do that is to use synchronization, which comes
   * with its own problems (use of threads could create deadlock). We replace the
   * "new StackableVariable" below by "new DynamicVariable" in
   * scala.util.DynamicVariable to replace global state by thread-local state (each
   * thread accesses a separate copy of a variable.
   *
   */

  private val caller = new DynamicVariable[Signal[_]](NoSignal)

  def apply[T](exp: => T) = new Signal(exp)
}

object NoSignal extends Signal[Nothing](???) {
  override protected def computeValue(): Unit = ()
}