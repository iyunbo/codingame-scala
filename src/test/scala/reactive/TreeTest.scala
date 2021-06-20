package org.iyunbo.coding
package reactive

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TreeTest extends AnyFlatSpec with should.Matchers {

  it should "calculate the depth of a Tree" in {
    Tree.depth(Branch(Leaf(1), Leaf(2))) should be(1)
    Tree.depth(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) should be(2)
    Tree.depth(Leaf(1)) should be(0)
  }

  it should "find the max value of a Int Tree" in {
    Tree.maximumn(Branch(Leaf(1), Leaf(2))) should be(2)
    Tree.maximumn(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) should be(3)
    Tree.maximumn(Leaf(1)) should be(1)
  }

  it should "count the number of nodes in a Tree" in {
    Tree.countNodes(Branch(Leaf(1), Leaf(2))) should be(3)
    Tree.countNodes(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) should be(5)
    Tree.countNodes(Leaf(1)) should be(1)
  }

  it should "map a tree of type Int to type String" in {
    Tree.map(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2)))(i => s"value: $i") should be(
      Branch(Branch(Leaf("value: 1"), Leaf("value: 3")), Leaf("value: 2"))
    )
    Tree.map(Leaf(1))(_.toString) should be(Leaf("1"))
  }

}
