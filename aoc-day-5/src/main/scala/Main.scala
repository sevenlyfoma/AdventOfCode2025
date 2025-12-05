import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer

@main def main(): Unit =
  star_one()

def star_one(): Unit = 
  // var values = loadValues("test.txt")
  var values = loadValues("input.txt")

  var count = countInRange(values._1, values._2)

  println(count)
  ()

def countInRange(ranges: ArrayBuffer[NumRange], tests: ArrayBuffer[Long]): Int =
  var count = 0
  
  
  for (test <- tests) do
    var notFinished = true;
    var position = 0;
    while (notFinished) do

      if (ranges(position).inRange(test)) then
        count += 1
        notFinished = false;


      position += 1;
      if (position >= ranges.length) then
        notFinished = false;
  

  count


class NumRange(startR: Long, endR: Long):
  def inRange(test: Long): Boolean =
    (test >= startR) && (test <= endR)

  override def toString(): String =
    "range(" + startR.toString() + "-" + endR.toString() + ")"


def loadValues(filename: String): (ArrayBuffer[NumRange], ArrayBuffer[Long]) =
  var ranges = ArrayBuffer[NumRange]()
  var testNums = ArrayBuffer[Long]()

  var lines =  Source.fromResource(filename).getLines()

  var line = lines.next()
  while (!line.equals("")) do
    var splitLine = line.split("-")
    val numRange = NumRange(splitLine(0).toLong, splitLine(1).toLong)
    ranges.addOne(numRange)
    line = lines.next()
  
  for (x <- lines) do 
    testNums.addOne(x.toLong)

 

  (ranges, testNums)