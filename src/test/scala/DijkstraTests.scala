import Dijkstra.Direction
import org.scalatest.funsuite.AnyFunSuite

class DijkstraTests extends AnyFunSuite {
  val graph1: Digraph[Int] = Graphs.get_graph1()
  val graph3: Digraph[Int] = Graphs.get_graph3()

  test("Dijkstra shortest path (forward) in a directed graph"){
    assert(new Dijkstra[Int](graph3, 1).shortest_path_cost(1) == 0)
    assert(new Dijkstra[Int](graph3, 1).shortest_path_cost(4) == 3)
    assert(new Dijkstra[Int](graph3, 4).shortest_path_cost(1) == Float.PositiveInfinity)
  }

  test("Dijkstra shortest path (backwards) in a directed graph"){
    assert(new Dijkstra[Int](graph3, 1, Direction.Backward).shortest_path_cost(1) == 0)
    assert(new Dijkstra[Int](graph3, 4, Direction.Backward).shortest_path_cost(1) == 3)
    assert(new Dijkstra[Int](graph3, 1, Direction.Backward).shortest_path_cost(4) == Float.PositiveInfinity)
  }

  test("Dijkstra shortest path (bidirectional) in a directed graph"){
    assert(new BidirectionalDijkstra[Int](graph3, 1, 1).shortest_path_cost() == 0)
    assert(new BidirectionalDijkstra[Int](graph3, 1, 4).shortest_path_cost() == 3)
    assert(new BidirectionalDijkstra[Int](graph3, 4, 1).shortest_path_cost() == Float.PositiveInfinity)
  }

  test("Dijkstra shortest path (forwards) in an undirected graph"){
    assert(new Dijkstra[Int](graph1, 1).shortest_path_cost(1) == 0)
    assert(new Dijkstra[Int](graph1, 1).shortest_path_cost(2) == 4)
    assert(new Dijkstra[Int](graph1, 1).shortest_path_cost(3) == 1)
    assert(new Dijkstra[Int](graph1, 1).shortest_path_cost(4) == 3)
    assert(new Dijkstra[Int](graph1, 1).shortest_path_cost(5) == 2)
    assert(new Dijkstra[Int](graph1, 1).shortest_path_cost(6) == Float.PositiveInfinity)
  }
}