package org.iyunbo.coding
package progfun

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   *
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `test forall`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(forall(s, _ < 3), "forall1")
      assert(forall(s, _ > 0), "forall2")
      assert(!forall(s, _ == 1), "forall3")
    }
  }

  @Test def `test exists`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(exists(s, _ < 3), "exist1")
      assert(exists(s, _ > 0), "exist2")
      assert(exists(s, _ == 1), "exist3")
      assert(exists(s, _ == 2), "exist4")
      assert(!exists(s, _ == 5), "exist5")
    }
  }

  @Test def `test map`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(exists(map(s, _ * 2), _ == 2), "map1")
      assert(exists(map(s, _ * 3), _ == 6), "map2")
      assert(forall(map(s, _ * 4), _ > 3), "map3")
    }
  }

  @Test def `test filter`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(exists(filter(s, _ % 2 == 0), _ == 2), "filter1")
      assert(!exists(filter(s, _ % 2 == 0), _ == 1), "filter1")
      assert(exists(filter(s, _ % 2 == 1), _ == 1), "filter2")
      assert(!exists(filter(s, _ % 2 == 1), _ == 2), "filter2")
      assert(!exists(filter(s, _ % 2 == 1), _ == 3), "filter3")
    }
  }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
