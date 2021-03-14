import org.scalatest.funsuite.AnyFunSuite

class BinaryHeapTest extends AnyFunSuite {

  test("Add an element to a heap") {
    val heap: BinaryHeap[Int] = new BinaryHeap[Int]()
    List(10, 15, 20, 17, 8).foreach(v => heap.add(v))
    assert(heap._items.sameElements(List(8, 10, 20, 17, 15)))
  }

  test("Pull an element from a heap") {
    val heap: BinaryHeap[Int] = new BinaryHeap[Int]()
    List(10, 15, 20, 17, 25).foreach(v => heap.add(v))
    heap.pull()
    assert(heap._items.sameElements(List(15, 17, 20, 25)))
  }

}