case class Edge(from:Char,to:Char,distance:Int)

object ShortestPath {
  //頂点
  val vertexes='A'to'N'

  //辺
  val edges=Seq(
    Edge('A','B',9),
    Edge('A','D',6),
    Edge('A','C',6),
    Edge('B','E',2),
    Edge('C','E',9),
    Edge('C','G',6),
    Edge('D','F',3),
    Edge('E','I',1),
    Edge('F','H',5),
    Edge('F','J',9),
    Edge('G','J',9),
    Edge('G','I',3),
    Edge('H','K',5),
    Edge('J','K',4),
    Edge('J','M',6),
    Edge('J','L',7),
    Edge('K','M',1),
    Edge('L','N',3),
    Edge('M','N',2),
  )

  def solveByBellmanFord(start:Char,goal :Char):Unit={
    //各頂点までの距離の初期化
    var distances =vertexes.map(v=>(v -> Int.MaxValue)).toMap
    distances=distances+(start->0)

    var isUpdated =true
    while(isUpdated){
      isUpdated=false
      edges.foreach(e=>
      if(distances(e.from)!=Int.MaxValue
      && distances(e.to) >distances(e.from)+e.distance ){
        distances=distances+(e.to ->(distances(e.from)+e.distance))
        isUpdated=true;
      }
      )
    }
    println(distances)
    println(distances(goal))
  }

  def solveByWarshallFloyd(start: Char, goal: Char): Unit = {
    var distanceMap:Map[(Char,Char),Int]=vertexes.map(v=>((v,v)->0)).toMap
    distanceMap=distanceMap ++ edges.map(e=>(e.from,e.to)->e.distance)
    def distance(v1: Char, v2: Char): Int = distanceMap.getOrElse((v1, v2), Int.MaxValue / 2)
    for (v1 <- vertexes; v2 <- vertexes; v3 <- vertexes) {
      distanceMap =  distanceMap + ((v2, v3) -> math.min(distance(v2, v3), distance(v2, v1) + distance(v1, v3)))
    }
    println(distanceMap)
    println(distanceMap((start, goal)))
  }
}
