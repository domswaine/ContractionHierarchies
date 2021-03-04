import org.scalatest.funsuite.AnyFunSuite

class DigraphTest extends AnyFunSuite {

  val nodes: Set[Int] = Set[Int](1, 2, 3, 4, 5)
  val edges: List[(Int, Int, Float)] = List(
    (1, 3, 1), (3, 1, 1), (2, 3, 4), (3, 2, 4), (2, 4, 1), (4, 2, 1),
    (3, 5, 1), (5, 3, 1), (4, 5, 1), (5, 4, 1)
  )
  val graph: Digraph[Int] = new Digraph(nodes, edges)

  test("Shortest path in an directed graph") {
    assert(graph.dijkstra_shortest_path(1, 1) == 0)
    assert(graph.dijkstra_shortest_path(1, 2) == 4)
    assert(graph.dijkstra_shortest_path(1, 3) == 1)
    assert(graph.dijkstra_shortest_path(1, 4) == 3)
    assert(graph.dijkstra_shortest_path(1, 5) == 2)
    assert(graph.dijkstra_shortest_path(1, 6) == Float.PositiveInfinity)
  }

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

}