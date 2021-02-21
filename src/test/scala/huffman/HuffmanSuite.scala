package org.iyunbo.coding
package huffman

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit._

class HuffmanSuite {

  import Huffman._

  trait TestTrees {
    val t1: Fork = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2: Fork = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  @Test def `weight of a larger tree (10pts)`(): Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }


  @Test def `chars of a larger tree (10pts)`(): Unit =
    new TestTrees {
      assertEquals(List('a', 'b', 'd'), chars(t2))
    }

  @Test def `times of chars`(): Unit =
    new TestTrees {
      assertEquals(List(('a', 2), ('b', 1), ('d', 1)), times(List('a', 'b', 'd', 'a')))
    }

  @Test def `singleton of trees`(): Unit =
    new TestTrees {
      assertTrue(singleton(List(Leaf('a', 1))))
      assertFalse(singleton(List(Leaf('a', 1), Leaf('b', 2))))
      assertTrue(singleton(List()))
    }

  @Test def `string2chars hello world`(): Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))


  @Test def `make ordered leaf list for some frequency table (15pts)`(): Unit =
    assertEquals(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))


  @Test def `combine of some leaf list (15pts)`(): Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)), combine(leaflist))
  }

  @Test def `decode a very short text`(): Unit =
    new TestTrees {
      assertEquals("abbb".toList, decode(t1, List(0, 1, 1, 1)))
    }

  @Test def `encode a very short text`(): Unit =
    new TestTrees {
      assertEquals(List(0, 1, 0, 1), encode(t1)(List('a', 'b', 'a', 'b')))
    }

  @Test def `decode and encode a very short text should be identity (10pts)`(): Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Test def `quickly decode and encode a very short text should be identity`(): Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, quickEncode(t1)("ab".toList)))
    }

  @Test def `decoded secret`(): Unit =
    new TestTrees {
      println(String.valueOf(decodedSecret.toArray))
    }

  @Test def `create bigger code tree`(): Unit =
    new TestTrees {
      createCodeTree("ksdfj;ajfdkjsajfipewajfiajdfklsjdlkfjksjdkfljakdlsjdflkajlkfjlajkljflajdljflkajdkjfljaldjfapjfajajeijkjoytryeqt]rkmg][QOJHFAGFLASFD'AF".toList)
    }



  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
