import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer

@main def hello(): Unit =
  loadValues("test.txt")


def loadValues(filename: String): ArrayBuffer[(Int, Int)] =
  var values = ArrayBuffer[(Int, Int)]()

  var textInput =  Source.fromResource(filename).getLines().next()

  for pair <- textInput.split(",") do
    var split = pair.split("-")
    var one = split(0).toInt
    var two = split(1).toInt
    var tup: (Int, Int) = (one, two)
    values.addOne(tup)
    

  println(values);
  
  values