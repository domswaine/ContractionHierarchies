import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class PriorityQueue[N](implicit val ord: Ordering[N]){
  private val items: ArrayBuffer[N] = ArrayBuffer.empty[N]

  def size: Int = items.size
  def isEmpty: Boolean = items.isEmpty
  def nonEmpty: Boolean = items.nonEmpty

  private def get_left_child_index(i: Int): Int = 2*i + 1
  private def get_right_child_index(i: Int): Int = 2*i +2
  private def get_parent_index(i: Int): Int = (i - 1) / 2

  private def get_smaller_child_index(index: Int): Int = {
    var smaller_child_index = get_left_child_index(index)
    if(has_right_child(index) && ord.lt(right_child(index), left_child(index))){
      smaller_child_index = get_right_child_index(index)
    }
    smaller_child_index
  }

  private def has_left_child(i: Int): Boolean = get_left_child_index(i) < size
  private def has_right_child(i: Int): Boolean = get_right_child_index(i) < size
  private def has_parent(i: Int): Boolean = get_parent_index(i) >= 0

  private def left_child(i: Int): N = items(get_left_child_index(i))
  private def right_child(i: Int): N = items(get_right_child_index(i))
  private def parent(i: Int): N = items(get_parent_index(i))

  private def swap(first_index: Int, second_index: Int): Unit = {
    val temp = items(first_index)
    items(first_index) = items(second_index)
    items(second_index) = temp
  }

  def head: N = {
    if(isEmpty){throw new IllegalStateException()}
    items.head
  }

  def dequeue(): N = {
    if(isEmpty){throw new IllegalStateException()}
    val head = items.head
    items(0) = items(size - 1)
    items.remove(size - 1)
    heapify_down(0)
    head
  }

  def enqueue(element: N): Unit = {
    items.addOne(element)
    heapify_up(size - 1)
  }

  @tailrec private def heapify_up(index: Int): Unit = {
    if(has_parent(index) && ord.gt(parent(index), items(index))){
      swap(get_parent_index(index), index)
      heapify_up(get_parent_index(index))
    }
  }

  @tailrec private def heapify_down(index: Int): Unit = {
    if(has_left_child(index)){
      val smaller_child_index: Int = get_smaller_child_index(index)
      if(ord.gteq(items(index), items(smaller_child_index))){
        swap(index, smaller_child_index)
        heapify_down(smaller_child_index)
      }
    }
  }

}