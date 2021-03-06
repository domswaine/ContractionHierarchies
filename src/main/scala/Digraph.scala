import scala.collection.mutable.ListBuffer

case class Digraph[N](nodes: Set[N], edges: Map[(N, N), Float]) {
  private val in: Map[N, List[N]] = edges.keySet.groupBy(_._2).map{ case k->v => k->v.map(_._1).toList }
  private val out: Map[N, List[N]] = edges.keySet.groupBy(_._1).map{ case k->v => k->v.map(_._2).toList }

  def this(nodes: Set[N], edges: List[(N, N, Float)])
    = this(nodes, edges.map(e => ((e._1, e._2), e._3)).toMap)

  def edges_count(): Int = edges.keySet.size
  def get_cost(from: N, to: N): Float = edges(from, to)
  def get_incoming(n: N): List[N] = in.getOrElse(n, Nil)
  def get_outgoing(n: N): List[N] = out.getOrElse(n, Nil)

  def dijkstra_shortest_path(from: N, to: N): Float
    = Dijkstra.shortest_path(this, from, to)

  type edge = (N, N)
  type costed_edge = (edge, Float)

  def get_paths_through(n: N): List[(N, N)]
    = for(in <- get_incoming(n); out <- get_outgoing(n); if in!=out) yield (in, out)

  def get_costed_paths_through(n: N): List[((N, N), Float)]
    = get_paths_through(n).map{ case (from, to) => ((from, to), get_cost(from, n) + get_cost(n, to)) }

  def remove(n: N): Digraph[N]
    = Digraph(nodes.excl(n), edges.filter{ case k->_ => k._1 != n && k._2 != n })

  def add(new_edge: costed_edge): Digraph[N] = add(Seq(new_edge))
  def add(new_edges: Seq[costed_edge]): Digraph[N] = Digraph(nodes, edges ++ new_edges)

  def contract(n: N): (Digraph[N], List[(N, N, N)]) = {
    val without_node: Digraph[N] = this.remove(n)
    val augmentations: ListBuffer[costed_edge] = ListBuffer.empty[costed_edge]
    this.get_costed_paths_through(n).foreach{ case ((from, to), original_cost) =>
      val new_cost: Float = without_node.dijkstra_shortest_path(from, to)
      if(new_cost>=original_cost){augmentations.addOne((from, to) -> original_cost)}
    }
    (without_node.add(augmentations.toList), augmentations.toList.map(ae => (ae._1._1, n, ae._1._2)))
  }

  def edge_difference(n: N): Int = contract(n)._1.edges_count() - edges_count()

  def get_augmenting_edges(ordering: Ordering[N]): List[(N, N, N)]
    = new Contractor[N](this).get_augmenting_edges(ordering)

}