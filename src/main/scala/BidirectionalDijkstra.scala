class BidirectionalDijkstra[N](graph: Digraph[N], from: N, to: N) {
  val forward: Dijkstra[N] = new Dijkstra[N](graph, from, Dijkstra.Direction.Forward)
  val backward: Dijkstra[N] = new Dijkstra[N](graph, to, Dijkstra.Direction.Backward)

  def get_intersection(): Set[N] = {
    forward.expanded_nodes() intersect backward.expanded_nodes()
  }

  def shortest_path_cost(): Float = {
    var intersection: Set[N] = get_intersection()
    while(intersection.isEmpty && !(forward.finished() && backward.finished())){
      if(!forward.finished()){forward.step()}
      if(!backward.finished()){backward.step()}
      intersection = get_intersection()
    }
    if(intersection.isEmpty){Float.PositiveInfinity}
    else{intersection.map(n => forward.shortest_path_cost(n) + backward.shortest_path_cost(n)).min}
  }
}
