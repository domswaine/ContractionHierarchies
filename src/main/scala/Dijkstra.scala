import Dijkstra.Direction
import Dijkstra.Direction.Direction

import scala.annotation.tailrec
import scala.collection.mutable

class Dijkstra[N](graph: Digraph[N], root: N, direction: Direction = Direction.Forward) {
  private val dist: mutable.Map[N, Float] = mutable.Map(root -> 0)
  private val prev: mutable.Map[N, Option[N]] = mutable.Map(root -> None)
  private val expanded: mutable.Set[N] = mutable.Set.empty[N]
  private val queue: mutable.PriorityQueue[N] = mutable.PriorityQueue(root)(Ordering[Float].on(dist).reverse)

  def expanded_nodes(): Set[N] = expanded.toSet

  private def get_onwards_nodes(node: N): List[(N, Float)] = direction match {
    case Direction.Forward => graph.get_outgoing(node).map(out => (out, dist(node) + graph.get_cost(node, out)))
    case Direction.Backward => graph.get_incoming(node).map(in => (in, dist(node) + graph.get_cost(in, node)))
  }

  @tailrec final def finished(): Boolean = {
    if(queue.isEmpty){true}
    else if(!expanded.contains(queue.head)){false}
    else{
      queue.dequeue()
      finished()
    }
  }

  def step(): Unit = {
    val exp_node: N = queue.dequeue()
    expanded.add(exp_node)
    get_onwards_nodes(exp_node).foreach{ case (onward_node, new_cost) =>
      if(!dist.contains(onward_node) || new_cost < dist(onward_node)){
        prev += onward_node -> Some(exp_node)
        dist += onward_node -> new_cost
        queue.addOne(onward_node)
      }
    }
  }

  private def build_tree_until_node(to: N): Unit = {
    while(!finished() && !expanded.contains(to)){step()}
  }

  def shortest_path_cost(to: N): Float = {
    build_tree_until_node(to)
    dist.getOrElse(to, Float.PositiveInfinity)
  }
}

object Dijkstra {
  object Direction extends Enumeration {
    type Direction = Value
    val Forward, Backward = Value
  }
}
