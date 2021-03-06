import Dijkstra.Direction
import Dijkstra.Direction.Direction
import scala.collection.mutable

class Dijkstra[N](graph: Digraph[N], from: N, direction: Direction = Direction.Forward) {
  val dist: mutable.Map[N, Float] = mutable.Map(from -> 0)
  val prev: mutable.Map[N, Option[N]] = mutable.Map(from -> None)
  val expanded: mutable.Set[N] = mutable.Set.empty[N]
  val queue: mutable.PriorityQueue[N] = mutable.PriorityQueue(from)(Ordering[Float].on(dist).reverse)

  private def get_onwards_nodes(a: N): List[(N, Float)] = direction match {
    case Direction.Forward => graph.get_outgoing(a).map(b => (b, dist(a) + graph.get_cost(a, b)))
    case Direction.Backward => graph.get_incoming(a).map(b => (b, dist(a) + graph.get_cost(b, a)))
  }

  private def step(): Unit = {
    val exp_node: N = queue.dequeue()
    if(!expanded.contains(exp_node)){
      expanded.add(exp_node)
      get_onwards_nodes(exp_node).foreach{ case (onward_node, new_cost) =>
        if(!dist.contains(onward_node) || new_cost < dist(onward_node)){
          prev += onward_node -> Some(exp_node)
          dist += onward_node -> new_cost
          queue.addOne(onward_node)
        }
      }
    }
  }

  def shortest_path(to: N): Float = {
    while(queue.nonEmpty && !expanded.contains(to)){step()}
    dist.getOrElse(to, Float.PositiveInfinity)
  }
}

object Dijkstra {
  object Direction extends Enumeration {
    type Direction = Value
    val Forward, Backward = Value
  }
}
