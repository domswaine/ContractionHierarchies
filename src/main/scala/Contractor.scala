import scala.annotation.tailrec
import scala.collection.mutable

class Contractor[N](original_graph: Digraph[N]) {
  type PQ = mutable.PriorityQueue[N]
  type AugEdges = List[(N, N, N)]

  def get_augmenting_edges(ordering: Ordering[N]): AugEdges = {
    val pq: mutable.PriorityQueue[N] = mutable.PriorityQueue.empty(ordering)
    original_graph.nodes.foreach(n => pq.enqueue(n))
    get_augmenting_edges(pq, original_graph, Nil)
  }

  @tailrec
  private def get_augmenting_edges(pq: PQ, graph: Digraph[N], aug: AugEdges): AugEdges = {
    if(pq.nonEmpty){
      val (new_graph, new_aug) = graph.contract(pq.dequeue())
      get_augmenting_edges(pq, new_graph, aug ++ new_aug)
    }
    else aug
  }

}

object Contractor {
  val natural_int_ordering: Ordering[Int] = Ordering[Int].reverse
}