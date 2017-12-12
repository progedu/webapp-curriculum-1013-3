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
    //2点間の距離のmap
    var n = vertexes.length
    var dmap = Array.ofDim[Int](n,n)


    for(i<- 0 to n-1){
      for(j <- 0 to n-1){
        println(i,j)
        if(i == j){
          dmap(i)(j) = 0
        }
        else{
          dmap(i)(j) =  Integer.MAX_VALUE / 2
        }
      }
    }

    //グラフ読み込み
    edges.foreach { e =>

      var from = e.from
      var to = e.to
      var distance = e.distance

      // 'A'をInt化した値+1
      dmap(from.toInt - 65)(to.toInt - 65) = distance
    }
    //距離計算
      for(k<-0 to n-1){
        for(i<-0 to n-1){
          for(j<-0 to n-1){
            dmap(i)(j) = Math.min(dmap(i)(j),dmap(i)(k)+dmap(k)(j))
          }
        }
      }

    //経路を表示
    for(i<-0 to n-1){
      for(j<-0 to n-1){
        if(dmap(i)(j) != Integer.MAX_VALUE && i != j){
          printf("%s,%s=>%d, ",(i+'A').toChar,(j+'A').toChar,dmap(i)(j))
        }
      }
    }

    //start - goal の最短経路を表示
    println(dmap(start - 'A')(goal - 'A'))

  }

}
