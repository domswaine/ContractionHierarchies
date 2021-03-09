object Graphs {

  // Graph as per article on Contraction Hierarchies by Michael Tandy
  def get_graph1(): Digraph[Int] = {
    val g1_nodes: Set[Int] = Set(1, 2, 3, 4, 5)
    val g1_edges: List[(Int, Int, Float)] = List(
      (1, 3, 1), (3, 1, 1), (2, 3, 4), (3, 2, 4), (2, 4, 1), (4, 2, 1),
      (3, 5, 1), (5, 3, 1), (4, 5, 1), (5, 4, 1)
    )
    new Digraph(g1_nodes, g1_edges)
  }

  // Graph as per Monash University slides on Contraction Hierarchies
  def get_graph2(): Digraph[Int] = {
    val g2_nodes: Set[Int] = (1 to 11).toSet
    val g2_undirected_edges: List[(Int, Int, Float)] = List(
      (1, 2, 1), (1, 6, 1), (1, 10, 1), (3, 5, 1), (3, 11, 1), (4, 5, 1),
      (4, 11, 2), (7, 8, 1), (7, 9, 1), (9, 10, 1), (9, 11, 1)
    )
    val g2_directed_edges: List[(Int, Int, Float)]
    = g2_undirected_edges ++ g2_undirected_edges.map(e => (e._2, e._1, e._3))
    new Digraph(g2_nodes, g2_directed_edges)
  }

  def get_graph3(): Digraph[Int] = {
    val g3_nodes: Set[Int] = (1 to 4).toSet
    val g3_edges: List[(Int, Int, Float)] = List((1, 2, 1), (1, 3, 2), (2, 4, 2), (3, 4, 3))
    new Digraph[Int](g3_nodes, g3_edges)
  }

}
