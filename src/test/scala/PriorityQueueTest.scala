import org.scalatest.funsuite.AnyFunSuite

class PriorityQueueTest extends AnyFunSuite {

  test("Test PQ isEmpty, nonEmpty") {
    val pq: PriorityQueue[String] = new PriorityQueue[String]()
    assert(pq.isEmpty)
    assert(!pq.nonEmpty)
    pq.enqueue("A")
    assert(!pq.isEmpty)
    assert(pq.nonEmpty)
  }

  test("Test PQ size") {
    val pq: PriorityQueue[String] = new PriorityQueue[String]()
    assert(pq.size == 0)
    pq.enqueue("A")
    assert(pq.size == 1)
    pq.enqueue("B")
    assert(pq.size == 2)
    pq.enqueue("C")
    assert(pq.size == 3)
  }

  test("Test single item"){
    val pq: PriorityQueue[String] = new PriorityQueue[String]()
    pq.enqueue("A")
    assert(pq.head == "A")
    assert(pq.size == 1)
    assert(!pq.isEmpty)
    assert(pq.nonEmpty)
  }

  test("Test element"){
    val pq: PriorityQueue[String] = new PriorityQueue[String]()
    List("A", "B", "C").foreach(c => pq.enqueue(c))
    assert(pq.head == "A")
    assert(pq.size == 3)
    assert(!pq.isEmpty)
    assert(pq.nonEmpty)
  }

  test("Test three element"){
    val pq: PriorityQueue[String] = new PriorityQueue[String]()
    pq.enqueue("B")
    pq.enqueue("A")
    assert(pq.head == "A")
  }

  test("Test remove"){
    val pq: PriorityQueue[String] = new PriorityQueue[String]()
    List("A", "B", "C").foreach(c => pq.enqueue(c))
    assert(pq.dequeue() == "A")
    assert(2 == pq.size)
    assert(pq.dequeue() == "B")
    assert(1 == pq.size)
    assert(pq.dequeue() == "C")
    assert(0 == pq.size)
  }

  test("Test remove, with custom ordering"){
    val pq: PriorityQueue[String] = new PriorityQueue[String]()(Ordering[String].reverse)
    List("A", "B", "C").foreach(c => pq.enqueue(c))
    assert(pq.dequeue() == "C")
    assert(2 == pq.size)
    assert(pq.dequeue() == "B")
    assert(1 == pq.size)
    assert(pq.dequeue() == "A")
    assert(0 == pq.size)
  }

}