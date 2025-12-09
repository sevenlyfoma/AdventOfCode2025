import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer
import scala.math.pow

@main def hello(): Unit =
  var values = loadValues("test.txt")

  println(values)


def loadValues(filename: String): ArrayBuffer[(Long, Long)] = 
  var values = ArrayBuffer[(Long, Long)]()
  var lines =  Source.fromResource(filename).getLines()

  for (line <- lines) do
    var split = line.split(",")
    values.addOne((split(0).toLong,split(1).toLong))

  values
