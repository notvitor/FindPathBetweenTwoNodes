import scala.collection.mutable._

object Main extends App {

  //(1) Example requested.
  val graph1: HashMap[Int, ListBuffer[Int]] = HashMap[Int,ListBuffer[Int]]()
  graph1 +=(1 -> ListBuffer(1, 2),
            2 -> ListBuffer(4),
            3 -> ListBuffer(5))

  val graph2: HashMap[String, ListBuffer[String]] = HashMap[String,ListBuffer[String]]()
  graph2 +=("A" -> ListBuffer("B", "C"),
            "B" -> ListBuffer("F"),
            "C" -> ListBuffer("F"),
            "D" -> ListBuffer("F"),
            "E" -> ListBuffer("D"),
            "F" -> ListBuffer("E"))

  println(findPathRecursive(graph1, 1, 4, new ListBuffer[Int]()))
  println(findPathRecursive(graph2, "A", "D", new ListBuffer[String]()))
  println(findPathRecursive(graph2, "A", "E", new ListBuffer[String]()))
  println(findPathRecursive(graph2, "A", "F", new ListBuffer[String]()))

  //Naive implementation using recursion and backtracking.
  //The last parameter is used to bypass the cyclic behavior as we check for the presence of a node at line 33.
  //A similar approach to this implementation could be given by just modifying some parts of this method.
  //The Find-All-Paths and Shortest-Path algorithms could be considered as a variation of the given example.
  def findPathRecursive[T](graph: HashMap[T, ListBuffer[T]], startNode: T, endNode: T, path: ListBuffer[T]): ListBuffer[T] = {
    path += startNode
    if (startNode == endNode) return path
    if (!graph.contains(startNode)) return ListBuffer[T]()

    for (node <- graph(startNode))
      if (!path.contains(node)) {
        val newPath: ListBuffer[T] = findPathRecursive(graph, node, endNode, path)
        if (newPath.nonEmpty) return newPath
      }
    ListBuffer()
  }
}
