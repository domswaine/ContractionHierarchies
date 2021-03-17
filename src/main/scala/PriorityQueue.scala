import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class PriorityQueue[N](implicit val ord: Ordering[N]){
  private val items: ArrayBuffer[N] = ArrayBuffer.empty[N]
  private val indexes: mutable.Map[N, Int] = mutable.Map.empty[N, Int]

  def size: Int = items.size
  def isEmpty: Boolean = items.isEmpty
  def nonEmpty: Boolean = items.nonEmpty

  def _valid_indexing(): Boolean = {
    var is_valid: Boolean = true
    indexes.keySet.foreach(k => {
      if(items(indexes(k)) != k){is_valid = false}
    })
    is_valid
  }

  private def in_range(i: Int): Option[Int] = if(i < size && i >= 0){Some(i)} else None
  private def get_parent_index(i: Int): Option[Int] = in_range((i - 1) / 2)
  private def get_left_child_index(i: Int): Option[Int] = in_range(2*i + 1)
  private def get_right_child_index(i: Int): Option[Int] = in_range(2*i + 2)

  private def get_smaller_child_index(i: Int): Option[Int] = get_left_child_index(i) match {
    case None => None
    case Some(left) => get_right_child_index(i) match {
      case Some(right) if ord.lt(items(right), items(left)) => Some(right)
      case _ => Some(left)
    }
  }

  private def swap(first_index: Int, second_index: Int): Unit = {
    val temp = items(first_index)
    items(first_index) = items(second_index)
    items(second_index) = temp
    indexes(items(first_index)) = first_index
    indexes(items(second_index)) = second_index
  }

  def head: N = {
    if(isEmpty){throw new IllegalStateException()}
    items.head
  }

  def enqueue(element: N): Unit = {
    if(indexes.contains(element)){heapify(indexes(element))}
    else{
      items.addOne(element)
      indexes.addOne(element -> (size-1))
      heapifyUp(size - 1)
    }
  }

  def dequeue(): N = {
    if(isEmpty){throw new IllegalStateException()}
    val head = items.head
    swap(0, size-1)
    items.remove(size - 1)
    indexes.remove(head)
    heapifyDown(0)
    head
  }

  def dequeueAll(): Seq[N] = {
    val bf: ListBuffer[N] = new ListBuffer[N]
    while(nonEmpty){bf.addOne(dequeue())}
    bf.toList
  }

  private def heapify(index: Int): Unit = {
    heapifyUp(index)
    heapifyDown(index)
  }

  @tailrec private def heapifyUp(index: Int): Unit = {
    val parent_index: Option[Int] = get_parent_index(index)
    if(parent_index.isDefined && ord.gt(items(parent_index.get), items(index))){
      swap(parent_index.get, index)
      heapifyUp(parent_index.get)
    }
  }

  @tailrec private def heapifyDown(index: Int): Unit = {
    val smaller_child_index: Option[Int] = get_smaller_child_index(index)
    if(smaller_child_index.isDefined && ord.gteq(items(index), items(smaller_child_index.get))){
      swap(index, smaller_child_index.get)
      heapifyDown(smaller_child_index.get)
    }
  }

}
