import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer

@main def main(): Unit =
  star_one()

def star_one(): Unit = 
  // var ranges = loadValues("test.txt")
  var ranges = loadValues("input.txt")
  var totalSum: Long = 0;
  for (x <- ranges) do
    var doubles = findRepeatsInPairs(x);
    for (y <- doubles) do
      totalSum = totalSum + y

  println(totalSum)


def loadValues(filename: String): ArrayBuffer[(String, String)] =
  var values = ArrayBuffer[(String, String)]()

  var textInput =  Source.fromResource(filename).getLines().next()

  for pair <- textInput.split(",") do
    var split = pair.split("-")
    var one = split(0)
    var two = split(1)
    var tup: (String, String) = (one, two)
    values.addOne(tup)
  
  values

//Need longs cos the input numbers are big
def findRepeatsInPairs(pair: (String, String)): ArrayBuffer[Long] =
  var values = ArrayBuffer[Long]()

  var smaller = pair._1;
  var larger = pair._2;

  var larger_length = larger.length();
  var larger_tophalf_length = larger_length/2;
  if (larger_length % 2 == 1) then  //If odd number of digits we need to to take the larger half, but division rounds down, so add 1
     larger_tophalf_length += 1

  var smaller_length = smaller.length();
  var smaller_tophalf_length = smaller_length/2; // we want to round down for smaller

  //Extract the top half
  var larger_tophalf = larger.substring(0, larger_tophalf_length);
  var smaller_tophalf = smaller.substring(0, smaller_tophalf_length);

  //Means they are a single digit long
  if (smaller_tophalf_length == 0)
    smaller_tophalf = "0"
  if (larger_tophalf_length == 0)
    larger_tophalf = "0"
    

  for (x <- Range.inclusive(smaller_tophalf.toInt, larger_tophalf.toInt)) do
    //Concat the tophalf with itself then turn to a long
    var doubled = ((x.toString) + (x.toString)).toLong
    
    //Check if its in range
    if (doubled >= smaller.toLong && doubled <= larger.toLong){
      values.addOne(doubled)
    }



  values
  


