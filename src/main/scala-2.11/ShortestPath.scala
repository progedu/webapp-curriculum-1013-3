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
    // let dist be a |V| × |V| array of minimum distances initialized to ∞ (infinity)
    // 頂点同士の距離を保持した距離マトリクスを作成
    val size = edges.length
    val dist = Array.ofDim[Int](size, size)

    // let next be a |V| × |V| array of vertex indices initialized to null
    // 経路情報のための経路マトリクスを作成
    val next = Array.ofDim[Char](size, size)

    // 距離マトリクスと経路マトリクスの初期化
    for(i <- 0 until size) {
      for (j <- 0 until size){
        // 頂点間の距離を初期化：自分自身への距離は０、それ以外へは整数の最大値を初期値とする
        dist(i)(j) = if(i == j) 0 else Int.MaxValue
        next(i)(j) = '_'
      }
    }

    // for each edge (u,v)
    // すでに判明している頂点間の距離をマトリクスに格納していく
    edges.foreach {e =>
      // dist[u][v] ← w(u,v)  // the weight of the edge (u,v)
      val (row, col, distance) = (charToNum(e.from), charToNum(e.to), e.distance) // 'A'-> 0, 'B'-> 1 ...
      dist(row)(col) = distance
      next(row)(col) = e.to // next[u][v] ← v
    }

    // 中間地点を足掛かりに、頂点間の最短距離の更新をしていくループ
    for(k <- 0 until size) {
      for(i <- 0 until size) {
        for(j <- 0 until size) {
          if (dist(i)(j) > (dist(i)(k) + dist(k)(j))
            && dist(i)(k) != Int.MaxValue && dist(k)(j) != Int.MaxValue) { // 負の数に反転してしまうのを阻止
            dist(i)(j) = dist(i)(k) + dist(k)(j)
            next(i)(j) = next(i)(k)
          }
        }
      }
    }

    // 始点uと終点vの文字を対応するインデックスに変換
    var u = charToNum(start)
    var v = charToNum(goal)
    
    // 最短距離を表示
    println(dist(u)(v)) //  'A' -- 'N' == 22

    // ゴールまでに通った経路の作成
    var paths: Seq[Char] = Seq()
    paths = paths :+ numToChar(u)
    while(u != v ) {
      u = charToNum(next(u)(v))
      paths = paths :+ numToChar(u)
    }

    // スタートからゴールまでの経路を表示
    println(paths)  
  }

  // 配列のインデックス処理の関係で'A'<--->0、'B'<--->1、になるように、文字と数値 の変換を行う
  def charToNum(v: Char): Int = v.toInt - 65
  def numToChar(i: Int): Char = (i + 65).toChar

}
