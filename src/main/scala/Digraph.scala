case class Digraph[N](nodes: Set[N], edges: Map[(N, N), Float]) {
  def this(nodes: Set[N], edges: List[(N, N, Float)]) = this(nodes, edges.map(e => ((e._1, e._2), e._3)).toMap)
  private val in: Map[N, List[N]] = edges.keySet.groupBy(_._2).map{ case k->v => k->v.map(_._1).toList }
  private val out: Map[N, List[N]] = edges.keySet.groupBy(_._1).map{ case k->v => k->v.map(_._2).toList }

  type edge = (N, N)
  type costed_edge = (edge, Float)

  def add(new_edge: costed_edge): Digraph[N] = add(Seq(new_edge))
  def add(new_edges: Seq[costed_edge]): Digraph[N] = Digraph(nodes, edges ++ new_edges)
  def remove(n: N): Digraph[N] = Digraph(nodes.excl(n), edges.filter{ case k->_ => k._1 != n && k._2 != n })

  def edge_count(): Int = edges.keySet.size
  def get_cost(from: N, to: N): Float = edges(from, to)
  def get_incoming(n: N): List[N] = in.getOrElse(n, Nil)
  def get_outgoing(n: N): List[N] = out.getOrElse(n, Nil)

  def shortest_path(from: N, to: N): Float
    = new BidirectionalDijkstra[N](this, from, to).shortest_path_cost()
}