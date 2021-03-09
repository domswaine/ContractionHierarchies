import org.scalatest.funsuite.AnyFunSuite

class ContractorTest extends AnyFunSuite {
  val graph: Digraph[Int] = Graphs.get_graph1()
  val graph2: Digraph[Int] = Graphs.get_graph2()

  test("Paths through node") {
    assert(new Contractor[Int](graph).get_paths_through(1).toSet == Set.empty)
    assert(new Contractor[Int](graph).get_paths_through(2).toSet == Set((3, 4), (4, 3)))
    assert(new Contractor[Int](graph).get_paths_through(3).toSet == Set((1, 2), (1, 5), (2, 1), (2, 5), (5, 1), (5, 2)))
    assert(new Contractor[Int](graph).get_paths_through(4).toSet == Set((2, 5), (5, 2)))
    assert(new Contractor[Int](graph).get_paths_through(5).toSet == Set((3, 4), (4, 3)))
  }

  test("Contract graph node"){
    val contractor: Contractor[Int] = new Contractor[Int](graph)
    assert(contractor.contract_node(3).toSet == Set((1,3,2), (1,3,5), (5,3,1), (2,3,1)))
  }

  test("Calculate edge difference for contracting a given node"){
    assert(new Contractor[Int](graph).edge_difference(3) == -2)
  }

  test("Contract graph node (Monash)"){
    val contractor: Contractor[Int] = new Contractor[Int](graph2)
    assert(contractor.contract_node(1).toSet == Set((2,1,6), (6,1,2), (2,1,10), (10,1,2), (6,1,10), (10,1,6)))
    assert(contractor.contract_node(2).toSet == Set.empty)
    assert(contractor.contract_node(3).toSet == Set((5,3,11), (11,3,5)))
    assert(contractor.contract_node(4).toSet == Set.empty)
    assert(contractor.contract_node(5).toSet == Set.empty)
    assert(contractor.contract_node(6).toSet == Set.empty)
    assert(contractor.contract_node(7).toSet == Set((8,7,9), (9,7,8)))
    assert(contractor.contract_node(8).toSet == Set.empty)
    assert(contractor.contract_node(9).toSet == Set((10,9,11), (11,9,10)))
    assert(contractor.contract_node(10).toSet == Set.empty)
    assert(contractor.contract_node(11).toSet == Set.empty)
  }

  test("Calculate CH augmenting edges"){
    val aug_edges: Set[(Int, Int, Int)] = List(
      (2,1,6), (2,1,10), (6,1,2), (6,1,10), (10,1,2), (10,1,6),
      (11,3,5), (5,3,11), (9,7,8), (8,7,9), (10,9,11), (11,9,10)
    ).toSet
    val contractor: Contractor[Int] = new Contractor[Int](graph2)
    contractor.contract(Contractor.natural_int_ordering)
    assert(contractor.augmenting_edges.toSet == aug_edges)
  }

}