import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer

@main def main(): Unit =
 star_one()


def star_one(): Unit = 
  // var inputs = loadValues("test.txt")
  var inputs = loadValues("input.txt")
  
  var mapped  = inputs.map(findHighJoltage(_))

  var reduced = mapped.reduceLeft((x, y) => x + y)

  println(reduced)

def findHighJoltage(voltages: String): Int =
  var voltageList = voltages.split("").array.map(_.toInt)
  
  var highest = 0;
  var secondHighest = 0;

  for (x <- 0 until voltageList.length) do
    val v = voltageList(x);
    if (x != voltageList.length-1) then //If we arent at the very last slot then we can possibly find the highest first digit
      if (v > highest) then
        highest = v;
        secondHighest = 0; //Reset the second highest, since it must come after the highest
      else if (v > secondHighest) then
        secondHighest = v
    else if (v > secondHighest) then
        secondHighest = v
      
  var num = highest*10 + secondHighest

  num

def loadValues(filename: String): ArrayBuffer[String] =
  var values = ArrayBuffer[String]()

  var lines =  Source.fromResource(filename).getLines()

  values.addAll(lines)

  values