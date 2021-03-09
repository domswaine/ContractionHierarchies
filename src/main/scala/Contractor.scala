import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Contractor[N](original_graph: Digraph[N]) {
  type edge = (N, N)
  type AugEdge = (N, N, N)
  type costed_edge = (edge, Float)
  type PQ = mutable.PriorityQueue[N]

  private var graph: Digraph[N] = original_graph
  val augmenting_edges: ListBuffer[AugEdge] = ListBuffer.empty[AugEdge]

  def get_paths_through(n: N): List[(N, N)]
    = for(in <- graph.get_incoming(n); out <- graph.get_outgoing(n); if in!=out) yield (in, out)

  def get_costed_paths_through(n: N): List[((N, N), Float)]
    = get_paths_through(n).map{ case (from, to) => ((from, to), graph.get_cost(from, n) + graph.get_cost(n, to)) }

  private def _contract_node(n: N): (Digraph[N], List[AugEdge]) = {
    val without_node: Digraph[N] = graph.remove(n)
    val augmentations: ListBuffer[costed_edge] = ListBuffer.empty[costed_edge]
    this.get_costed_paths_through(n).foreach{ case ((from, to), original_cost) =>
      val new_cost: Float = without_node.shortest_path(from, to)
      if(new_cost>=original_cost){augmentations.addOne((from, to) -> original_cost)}
    }
    (without_node.add(augmentations.toList), augmentations.toList.map(ae => (ae._1._1, n, ae._1._2)))
  }

  def contract_node(n: N): List[AugEdge] = {
    val (new_graph, new_aug_edges) = _contract_node(n)
    graph = new_graph
    augmenting_edges.addAll(new_aug_edges)
    new_aug_edges
  }

  def get_nodes(): List[N] = graph.nodes.toList

  def edge_difference(n: N): Int = _contract_node(n)._1.edge_count() - graph.edge_count()

  def contract(ordering: Ordering[N]): Unit = {
    val pq: PQ = mutable.PriorityQueue.empty(ordering)
    get_nodes().foreach(n => pq.enqueue(n))
    contract(pq)
  }

  def contract(pq: PQ): Unit = while(pq.nonEmpty){
    while(pq.nonEmpty){
      contract_node(pq.dequeue())
    }
  }

}

object Contractor {
  val natural_int_ordering: Ordering[Int] = Ordering[Int].reverse
}