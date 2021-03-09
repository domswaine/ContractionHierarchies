import org.scalatest.funsuite.AnyFunSuite

class DigraphTest extends AnyFunSuite {
  val graph: Digraph[Int] = Graphs.get_graph1()

  test("Add edge to graph") {
    val edge: ((Int, Int), Float) = (1, 5) -> 2
    assert(graph.add(edge).edges.keySet -- graph.edges.keySet == Set(edge._1))
  }

  test("Remove node from graph") {
    val after: Digraph[Int] = graph.remove(3)
    assert(after.nodes == Set(1, 2, 4, 5))
    assert(after.edges.keySet == Set((2, 4), (4, 2), (4, 5), (5, 4)))
  }

}