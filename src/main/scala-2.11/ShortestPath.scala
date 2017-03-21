/**
  * Created by soichiro_yoshimura on 2016/06/27.
  */
case class Edge(from: Char, to: Char, distance: Int)

object ShortestPath {

  /**
    * 頂点
    */
  val vertexes = 'A' to 'N'

  /**
    * 辺
    */
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
    Edge('N', 'M', 2)
  )

  def solveByBellmanFord(start: Char, goal: Char): Unit = {
    // 各頂点までの距離の初期化
    var distances = vertexes.map(v => (v -> Int.MaxValue)).toMap
    distances = distances + (start -> 0)

    var isUpdated = true
    while (isUpdated) {
      isUpdated = false
      edges.foreach { e =>
        if(distances(e.from) != Int.MaxValue
          && distances(e.to) > distances(e.from) + e.distance) {
          distances = distances + (e.to -> (distances(e.from) + e.distance))
          isUpdated = true
        }
      }
    }

    println(distances)
    println(distances(goal))
  }

  def solveByDijkstra(start: Char, goal: Char): Unit = {
    // 各頂点までの距離の初期化
    var distances = vertexes.map(v => (v -> Int.MaxValue)).toMap
    distances = distances + (start -> 0)

    var usedEdges: Set[Edge] = Set()
    var isUpdated = true

    while (isUpdated) {
      isUpdated = false
      edges.foreach { e =>
        if(!usedEdges.contains(e)
          && distances(e.from) != Int.MaxValue
          && distances(e.to) > distances(e.from) + e.distance) {
          distances = distances + (e.to -> (distances(e.from) + e.distance))
          usedEdges = usedEdges + e
          isUpdated = true
        }
      }
    }

    println(distances)
    println(distances(goal))
  }

  def solveByWarshallFloyd(start: Char, goal: Char): Unit = {
    val edgeDistances = edges.map(e => Seq(e.from, e.to) -> e.distance).toMap
    var d: Map[Seq[Char], Int] = Map()

    // 辺の距離を初期化
    for (i <- vertexes; j <- vertexes) {
      if (i == j) d += Seq(i, j) -> 0
      else if (edgeDistances.contains(Seq(i, j))) d += Seq(i, j) -> edgeDistances(Seq(i, j))
      else d += Seq(i, j) -> Int.MaxValue / 2
    }

    // 解く
    for (i <- vertexes; j <- vertexes; k <- vertexes) {
      val updateDistance = Math.min(d(Seq(i, j)), d(Seq(i, k)) + d(Seq(k, j)))
      d += Seq(i, j) -> updateDistance
    }

    println(d)
    println(d(Seq(start, goal)))
  }
}
