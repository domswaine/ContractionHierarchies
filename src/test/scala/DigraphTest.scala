import org.scalatest.funsuite.AnyFunSuite

class DigraphTest extends AnyFunSuite {
  val graph: Digraph[Int] = Graphs.get_graph1()
  val graph2: Digraph[Int] = Graphs.get_graph2()

  test("Paths through node") {
    assert(graph.get_paths_through(1).toSet == Set.empty)
    assert(graph.get_paths_through(2).toSet == Set((3, 4), (4, 3)))
    assert(graph.get_paths_through(3).toSet == Set((1, 2), (1, 5), (2, 1), (2, 5), (5, 1), (5, 2)))
    assert(graph.get_paths_through(4).toSet == Set((2, 5), (5, 2)))
    assert(graph.get_paths_through(5).toSet == Set((3, 4), (4, 3)))
  }

  test("Remove node from graph") {
    val after: Digraph[Int] = graph.remove(3)
    assert(after.nodes == Set(1, 2, 4, 5))
    assert(after.edges.keySet == Set((2, 4), (4, 2), (4, 5), (5, 4)))
  }

  test("Add edge to graph") {
    val edge: ((Int, Int), Float) = (1, 5) -> 2
    assert(graph.add(edge).edges.keySet -- graph.edges.keySet == Set(edge._1))
  }

  test("Contract graph node"){
    val (n_graph: Digraph[Int], aug_edges: List[(Int, Int, Int)]) = graph.contract(3)
    assert(n_graph.nodes.size == 4)
    assert(n_graph.edges.size == 8)
    assert(aug_edges.toSet == Set((1,3,2), (1,3,5), (5,3,1), (2,3,1)))
  }

  test("Calculate edge difference for contracting a given node"){
    assert(graph.edge_difference(3) == -2)
  }

  test("Calculate CH augmenting edges"){
    val aug_edges: Set[(Int, Int, Int)] = List(
      (2,1,6), (2,1,10), (6,1,2), (6,1,10), (10,1,2), (10,1,6),
      (11,3,5), (5,3,11), (9,7,8), (8,7,9), (10,9,11), (11,9,10)
    ).toSet
    assert(graph2.get_augmenting_edges(Contractor.natural_int_ordering).toSet == aug_edges)
  }

}