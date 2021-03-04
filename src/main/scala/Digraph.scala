case class Digraph[N](nodes: Set[N], edges: Map[(N, N), Float]) {
  private val in: Map[N, List[N]] = edges.keySet.groupBy(_._2).map{ case k->v => k->v.map(_._1).toList }
  private val out: Map[N, List[N]] = edges.keySet.groupBy(_._1).map{ case k->v => k->v.map(_._2).toList }

  def this(nodes: Set[N], edges: List[(N, N, Float)])
    = this(nodes, edges.map(e => ((e._1, e._2), e._3)).toMap)

  def get_edge(from: N, to: N): (N, N, Float)
    = (from, to, edges(from, to))

  def get_incoming(n: N): List[N] = in.getOrElse(n, Nil)
  def get_outgoing(n: N): List[N] = out.getOrElse(n, Nil)
  def get_incoming_edges(to: N): List[(N, N, Float)] = get_incoming(to).map(from => get_edge(from, to))
  def get_outgoing_edges(from: N): List[(N, N, Float)] = get_outgoing(from).map(to => get_edge(from, to))

  def dijkstra_shortest_path(from: N, to: N): Float
    = Dijkstra.shortest_path(this, from, to)

  def get_paths_through(n: N): List[(N, N)]
    = for(in <- get_incoming(n); out <- get_outgoing(n); if in!=out) yield (in, out)

  def remove(n: N): Digraph[N]
    = Digraph(nodes.excl(n), edges.filter{ case k->_ => k._1 != n && k._2 != n })

  def add(e: ((N, N), Float)): Digraph[N] = Digraph(nodes, edges + e)

}