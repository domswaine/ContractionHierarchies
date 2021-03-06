import scala.collection.mutable

class Dijkstra[N](graph: Digraph[N], from: N) {
  val dist: mutable.Map[N, Float] = mutable.Map(from -> 0)
  val prev: mutable.Map[N, Option[N]] = mutable.Map(from -> None)
  val expanded: mutable.Set[N] = mutable.Set.empty[N]
  val queue: mutable.PriorityQueue[N] = mutable.PriorityQueue(from)(Ordering[Float].on(dist).reverse)

  private def step(): Unit = {
    val exp_node: N = queue.dequeue()
    if(!expanded.contains(exp_node)){
      expanded.add(exp_node)
      graph.get_outgoing(exp_node).foreach(onward_node => {
        val new_cost: Float = dist(exp_node) + graph.get_cost(exp_node, onward_node)
        if(!dist.contains(onward_node) || new_cost < dist(onward_node)){
          prev += onward_node -> Some(exp_node)
          dist += onward_node -> new_cost
          queue.addOne(onward_node)
        }
      })
    }
  }

  def shortest_path(to: N): Float = {
    while(queue.nonEmpty && !expanded.contains(to)){step()}
    dist.getOrElse(to, Float.PositiveInfinity)
  }
}
