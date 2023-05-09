import scala.collection.mutable

case class Edge(from: Char, to: Char, dist: Int) {
  override def equals(other: Any): Boolean = other match {
    case that: Edge =>
      val cond1 = this.from == that.from && this.to == that.to && this.dist == that.dist
      val cond2 = this.from == that.to && this.to == that.from && this.dist == that.dist
      cond1 || cond2
    case _ => false
  }
}

object ShortestPath {

  val nodes = 'A' to 'N'
  val edges = Seq(
    Edge('A', 'B', 9),
    Edge('A', 'C', 6),
    Edge('A', 'D', 6),
    Edge('B', 'A', 9),
    Edge('B', 'E', 2),
    Edge('C', 'A', 6),
    Edge('C', 'E', 9),
    Edge('C', 'G', 6),
    Edge('D', 'A', 6),
    Edge('D', 'F', 3),
    Edge('E', 'B', 2),
    Edge('E', 'C', 9),
    Edge('E', 'I', 1),
    Edge('F', 'D', 3),
    Edge('F', 'H', 5),
    Edge('F', 'J', 9),
    Edge('G', 'C', 6),
    Edge('G', 'I', 3),
    Edge('G', 'J', 9),
    Edge('H', 'F', 5),
    Edge('H', 'K', 5),
    Edge('I', 'E', 1),
    Edge('I', 'G', 3),
    Edge('J', 'F', 9),
    Edge('J', 'G', 9),
    Edge('J', 'K', 4),
    Edge('J', 'L', 7),
    Edge('J', 'M', 6),
    Edge('K', 'H', 5),
    Edge('K', 'J', 4),
    Edge('K', 'M', 1),
    Edge('L', 'J', 7),
    Edge('L', 'N', 3),
    Edge('M', 'J', 6),
    Edge('M', 'K', 1),
    Edge('M', 'N', 2),
    Edge('N', 'L', 3),
    Edge('N', 'M', 2),
  )

  def bellmanFord(start: Char, goal: Char): Unit = {
    var dists = nodes.map(n => (n -> Int.MaxValue)).toMap
    dists += start -> 0

    var isUpdated = true
    while (isUpdated) {
      isUpdated = false
      edges.foreach(edge => {
        val (from, to) = (edge.from, edge.to)
        val isNotMaxValue = dists(from) != Int.MaxValue
        val isLonger = dists(to) > dists(from) + edge.dist
        if (isNotMaxValue && isLonger) {
          dists += to -> (dists(from) + edge.dist)
          isUpdated = true
        }
      })
    }
    println(dists)
    println(dists(goal))
  }

  def dijkstra(start: Char, goal: Char): Unit = {
    var dists = nodes.map(n => (n -> Int.MaxValue)).toMap
    dists += start -> 0
    var set = Set[Edge]()

    var isUpdated = true
    while (isUpdated) {
      isUpdated = false
      edges.foreach(edge => {
        if (!set.contains(edge)) {
          val (from, to) = (edge.from, edge.to)
          val isNotMaxValue = dists(from) != Int.MaxValue
          val isLonger = dists(to) > dists(from) + edge.dist
          if (isNotMaxValue && isLonger) {
            dists += to -> (dists(from) + edge.dist)
            set += edge
            isUpdated = true
          }
        }
      })
    }
    println(dists)
    println(dists(goal))
  }

  def warshallFloyd(start: Char, goal: Char): Unit = {
    var dists = Map[(Char, Char), Int]()
    for (x <- nodes; y <- nodes) {
      dists += (x, y) -> (if (x == y) 0 else 100000)
    }
    dists ++= edges.map(edge => (edge.from, edge.to) -> edge.dist)
    for (m <- nodes; x <- nodes; y <- nodes if x != y && x != m && y != m) {
      dists += (x, y) -> dists(x, y).min(dists(x, m) + dists(m, y))
    }
    println(dists)
    println(dists(start, goal))
  }
}
