import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class PriorityQueue[N](implicit val ord: Ordering[N]){
  private val items: ArrayBuffer[N] = ArrayBuffer.empty[N]
  private val indexes: mutable.Map[N, Int] = mutable.Map.empty[N, Int]

  def size: Int = items.size
  def isEmpty: Boolean = items.isEmpty
  def nonEmpty: Boolean = items.nonEmpty

  private def parent(e: N): Option[N] = {
    val parent_index: Int = (indexes(e) - 1) / 2
    if(parent_index >= 0){Some(items(parent_index))}
    else None
  }

  private def smaller_child(e: N): Option[N] = {
    val left_child_index: Int = 2 * indexes(e) + 1
    val right_child_index: Int = left_child_index + 1
    if(right_child_index < size){Some(items(right_child_index))}
    else if(left_child_index < size){Some(items(left_child_index))}
    else None
  }

  private def swap(first: N, second: N): Unit = {
    val first_index: Int = indexes(first)
    val second_index: Int = indexes(second)
    val temp: N = first
    items(first_index) = second
    indexes.addOne(second -> first_index)
    items(second_index) = temp
    indexes.addOne(temp -> second_index)
  }

  def head: N = {
    if(isEmpty){throw new IllegalStateException()}
    items.head
  }

  def dequeue(): N = {
    if(isEmpty){throw new IllegalStateException()}
    val head = items.head
    indexes.remove(head)
    items(0) = items(size-1)
    indexes.addOne(items(0) -> 0)
    items.remove(size-1)
    if(nonEmpty){heapify_down(items(0))}
    head
  }

  def enqueue(element: N): Unit = {
    items.addOne(element)
    indexes.addOne(element -> (size-1))
    heapify_up(element)
  }

  @tailrec private def heapify_up(element: N): Unit = {
    val parent_element: Option[N] = parent(element)
    if(parent_element.isDefined && ord.gt(parent_element.get, element)){
      swap(parent_element.get, element)
      heapify_up(parent_element.get)
    }
  }

  @tailrec private def heapify_down(element: N): Unit = {
    val child_element: Option[N] = smaller_child(element)
    if(child_element.isDefined && ord.gteq(element, child_element.get)){
      swap(element, child_element.get)
      heapify_down(child_element.get)
    }
  }

}
