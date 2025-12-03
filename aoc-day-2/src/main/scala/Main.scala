import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer

@main def main(): Unit =
  // star_one()
  // println(findRepeatsInPairs(("95", "115")))
  // println(findRepeatsInPairsWithChunkSize(("998", "1012"), 1))
  star_two()


def star_two(): Unit = 
  // var ranges = loadValues("test.txt")
  var ranges = loadValues("input.txt")

  var totalSum: Long = 0;
  for (x <- ranges) do
    var doubles = findRepeatsInPairs(x);
    println(doubles)
    for (y <- doubles) do
      totalSum = totalSum + y

  println(totalSum)


def findRepeatsInPairs(pair: (String, String)): ArrayBuffer[Long] =
  var values = ArrayBuffer[Long]()
  
  var smaller = pair._1;
  var larger = pair._2;

  var test_length = 1;

  while (test_length <= larger.length()/2) do
    values = values.appendAll(findRepeatsInPairsWithChunkSize(pair, test_length))

    test_length += 1;

  values = values.distinct;


  values

def findRepeatsInPairsWithChunkSize(pair: (String, String), chunkSize: Int): ArrayBuffer[Long] =
  var values = ArrayBuffer[Long]()

  var minValueOfChunk = "1"
  var maxValueOfChunk = "9"
  var c = chunkSize - 1;
  for (x <- Range.inclusive(1,chunkSize-1)) do
    minValueOfChunk = minValueOfChunk + "0"
    maxValueOfChunk = maxValueOfChunk + "9"

  var smaller = pair._1;
  var larger = pair._2;

  var chunk_larger = larger.substring(0, chunkSize);

  while (smaller.length() < larger.length()) do 
    smaller = "0" + smaller
    chunk_larger = chunk_larger + "0"

  var chunk_smaller = smaller.substring(0, chunkSize);
  
  
  
  if (chunk_smaller.toInt < minValueOfChunk.toInt) then
    chunk_smaller = minValueOfChunk
  
  if (chunk_larger.toInt > maxValueOfChunk.toInt) then
    chunk_larger = maxValueOfChunk

  for (x <- Range.inclusive(chunk_smaller.toInt, chunk_larger.toInt)) do
    var concat_target = x.toString() + x.toString();
    while concat_target.length() <= larger.length() do
      var v = concat_target.toLong 
      if ( v >= smaller.toLong && v <= larger.toLong) then
        values.addOne(v)
      concat_target = concat_target + x.toString();



  values



def star_one(): Unit = 
  // var ranges = loadValues("test.txt")
  var ranges = loadValues("input.txt")
  var totalSum: Long = 0;
  for (x <- ranges) do
    var doubles = findDoublesInPairs(x);
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
def findDoublesInPairs(pair: (String, String)): ArrayBuffer[Long] =
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
  


