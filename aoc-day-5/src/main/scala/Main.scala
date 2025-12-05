import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer

@main def main(): Unit =
  star_one()
  star_two()

def star_one(): Unit = 
  // var values = loadValues("test.txt")
  var values = loadValues("input.txt")

  var count = countInRange(values._1, values._2)

  println(count)
  ()

def star_two(): Unit = 
  // var values = loadValues("test.txt")._1
  var values = loadValues("input.txt")._1


  var folded = foldRangesRecursive(values)

  var count = sumRanges(folded)

  println(count)

def sumRanges(ranges: ArrayBuffer[NumRange]) : Long =
  var sum = 0.toLong;
  for (r <- ranges) do
    sum += (r.getEndR() - r.getStartR()) + 1.toLong
  
  return sum

def foldRangesRecursive(ranges: ArrayBuffer[NumRange]): ArrayBuffer[NumRange] =
  if (ranges.length == 1) then
    return ranges
  var allPartsButHead = foldRangesRecursive(ranges.tail)

  var head = ranges.head;

  var newRanges = ArrayBuffer[NumRange]();

  for (r <- allPartsButHead) do
    if (head.inRange(r.getStartR()) || head.inRange(r.getEndR()) || r.inRange(head.getStartR()) || r.inRange(head.getEndR())) then
      var start = 0.toLong
      var end = 0.toLong
      if (head.getStartR() < r.getStartR()) then
        start = head.getStartR()
      else
        start = r.getStartR()

      if (head.getEndR() > r.getEndR()) then
        end = head.getEndR()
      else
        end = r.getEndR()
      head = NumRange(start, end)
    else
      newRanges.addOne(r)
  
  newRanges.addOne(head)

  newRanges 




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

  def getInRange(): ArrayBuffer[Long] = 
    var ret = ArrayBuffer[Long]()
    for (x <- startR to endR) do 
      ret.addOne(x)

    ret
    
  def getStartR(): Long =
    startR
  def getEndR(): Long =
    endR


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