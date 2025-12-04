@main def hello(): Unit =
  var values = loadValues("test.txt")


def loadValues(filename: String): ArrayBuffer[String] =
  var values = ArrayBuffer[String]()

  var lines =  Source.fromResource(filename).getLines()

  values