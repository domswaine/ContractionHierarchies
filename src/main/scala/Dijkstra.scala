import scala.collection.mutable

object Dijkstra {

  def shortest_path[N](graph: Digraph[N], from: N, to: N): Float = {
    val dist: mutable.Map[N, Float] = mutable.Map(from -> 0)
    val expanded: mutable.Set[N] = mutable.Set.empty
    val queue: mutable.PriorityQueue[N] = mutable.PriorityQueue(from)(Ordering[Float].on(dist).reverse)
    while(queue.nonEmpty && !expanded.contains(to)){
      val exp_node = queue.dequeue()
      if(!expanded.contains(exp_node)){
        expanded.add(exp_node)
        graph.get_outgoing(exp_node).foreach(onward_node => {
          val new_cost: Float = dist(exp_node) + graph.get_cost(exp_node, onward_node)
          if(!dist.contains(onward_node) || new_cost < dist(onward_node)){
            dist += onward_node -> new_cost
            queue.addOne(onward_node)
          }
        })
      }
    }
    dist.getOrElse(to, Float.PositiveInfinity)
  }

}
