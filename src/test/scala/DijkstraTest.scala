import org.scalatest.funsuite.AnyFunSuite

class DijkstraTest extends AnyFunSuite {

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

}