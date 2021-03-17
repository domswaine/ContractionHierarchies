import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable
import scala.util.Random

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

  test("Large test"){
    val numbers_list: List[Int] = (0 to 100).toList
    val pq: PriorityQueue[Int] = new PriorityQueue[Int]()
    new Random(1).shuffle(numbers_list).foreach(n => pq.enqueue(n))
    assert(pq.dequeueAll() == numbers_list)
  }

  test("Test PQ element indexing"){
    val numbers_list: List[Int] = (0 to 20).toList
    val pq: PriorityQueue[Int] = new PriorityQueue[Int]()
    new Random(1).shuffle(numbers_list).foreach(n => {
      pq.enqueue(n)
      assert(pq._valid_indexing())
    })
    while(pq.nonEmpty){
      pq.dequeue()
      assert(pq._valid_indexing())
    }
  }

  test("Test increasing the priority of an item") {
    val d: mutable.Map[String, Int] = mutable.Map("A" -> 2, "B" -> 3, "C" -> 4, "D" -> 5)
    val pq: PriorityQueue[String] = new PriorityQueue[String]()(Ordering[Int].on(d))
    d.keySet.foreach(k => pq.enqueue(k))
    assert(pq.head == "A")
    d("C") = 0
    assert(pq.head == "A")
    pq.enqueue("C")
    assert(pq.head == "C")
  }

  test("Test decreasing the priority of an item") {
    val d: mutable.Map[String, Int] = mutable.Map("A" -> 2, "B" -> 3, "C" -> 4, "D" -> 5)
    val pq: PriorityQueue[String] = new PriorityQueue[String]()(Ordering[Int].on(d))
    d.keySet.foreach(k => pq.enqueue(k))
    assert(pq.head == "A")
    d("A") = 10
    assert(pq.head == "A")
    pq.enqueue("A")
    assert(pq.head == "B")
  }

}