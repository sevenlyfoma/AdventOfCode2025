@main def hello(): Unit =
  var values = loadValues("test.txt")

  println(values)



def loadValues(filename: String): ArrayBuffer[(Int, Int, Int)] = 
  var values = ArrayBuffer[(Int, Int, Int)]()
  var lines =  Source.fromResource(filename).getLines()


  values

