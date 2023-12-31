package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
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
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

  /**
   * This test is currently disabled (by using .ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */

  test("singleton set one contains one") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect contains only elements that exist in both sets") {
    new TestSets:
      val s = intersect(s1, union(s1, s2))

      assert(contains(s, 1), "Intersect 1")
  }

  test("All elements in the set are strictly less than 5") {
    new TestSets:
      val s = union(union(singletonSet(1), singletonSet(2)), union(singletonSet(3), singletonSet(4)))

      printSet(s)
      assert(forall(s, x => x < 5), "Forall 1")
  }

  test("All elements in the set are strictly less than 1000.") {
    new TestSets:
      val s = (x: Int) => x == -1000 || x == 0

      printSet(s)
      assert(forall(s, x => x < 1000), "Forall 2")
  }


  test("All elements in the set are strictly less than 1000.") {
    new TestSets:
      val s = (x: Int) => x == 1 || x == 3 || x == 4 || x == 5 || x == 7 || x == 1000

      printSet(s)
      assert(forall(s, x => x >= 5), "Forall 3")
  }

  test("2 should exist in the given set.") {
    new TestSets:
      val s = union(union(singletonSet(1), singletonSet(2)), union(singletonSet(3), singletonSet(4)))

      printSet(s)
      assert(exists(s, p => p == 2), "Forall 4")
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
