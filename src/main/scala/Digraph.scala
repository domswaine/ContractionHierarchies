case class Digraph[N](nodes: Set[N], edges: List[(N, N, Float)]) {
  def get_outgoing(node: N): List[(N, N, Float)] = edges.filter(_._1 == node)
  def dijkstra_shortest_path(from: N, to: N): Float = Dijkstra.shortest_path(this, from, to)
}