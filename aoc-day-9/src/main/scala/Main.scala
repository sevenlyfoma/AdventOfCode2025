import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer
import scala.math.pow
import scala.math.abs

@main def hello(): Unit =
  // star_one()

  ()

def star_one(): Unit =
  // var values = loadValues("test.txt")
  var values = loadValues("input.txt")

  var biggest = justCalculateItAll(values)

  println(biggest)


def getDistance(p1: (Long, Long), p2: (Long, Long)): Double = 
  var sumSquare = pow((p1._1 - p2._1),2)+pow((p1._2 - p2._2),2)

  var rt = pow(sumSquare, 0.5)

  rt

def getSize(p1: (Long, Long), p2: (Long, Long)): Long = 
  var x = (abs(p1._1 - p2._1)+1) * (abs(p1._2 - p2._2) + 1)
  x

def justCalculateItAll(inputs: ArrayBuffer[(Long, Long)]): Long =
  var res: Long = 0;

  var unsorted = ArrayBuffer[(Long, Long, Long)]();

  for (x <- 0 until inputs.length) do
    for (y <- 0 until inputs.length) do
      if (y > x) then
        unsorted.addOne((x, y, getSize(inputs(x), inputs(y))))

  
  var sorted = unsorted.sortBy(_._3).reverse

  res = sorted(0)._3


  res
//   var unsorted = ArrayBuffer[(Long, Long, Long)]()

//   // unsorted = inputs.map()


//   0.toLong

  



def findBiggestInQuadrants(inputs: ArrayBuffer[(Long, Long)], midpoint: (Long, Long)): Long =

  var topleft: (Long, Long, Double) = (0,0,0.0)
  var topright: (Long, Long, Double) = (0,0,0.0)
  var botleft: (Long, Long, Double) = (0,0,0.0)
  var botright: (Long, Long, Double) = (0,0,0.0)

  var square: Long = 0

  for ((x,y) <- inputs) do
    var distance = getDistance((x,y), midpoint)
    if (x >= midpoint._1) then //right
      if (y >= midpoint._2) then //bottom
        if (distance > botright._3) then
          botright = (x,y,distance)
      else // Top
        if (distance > topright._3) then
          topright = (x,y,distance)
    else // left
      if (y >= midpoint._2) then //bottom
        if (distance > botleft._3) then
          botleft = (x,y,distance)
      else // Top
        if (distance > topleft._3) then
          topleft = (x,y,distance)

  var tlbr = (botright._1 - topleft._1 +1) * (botright._2 - topleft._2+1)
  var trbl = (topright._1 - botleft._1 +1) * (botleft._2 - topright._2+ 1)

  if (tlbr > trbl) then
    square = tlbr
  else
    square = trbl

  println(tlbr)
  println(trbl)


  println(topleft)
  println(topright)
  println(botleft)
  println(botright)

  square

  


def identifyMidpoint(inputs: ArrayBuffer[(Long, Long)]): (Long, Long) =
  var highestX: Long = -1;
  var lowestX: Long = Long.MaxValue;

  var highestY: Long = -1;
  var lowestY: Long = Long.MaxValue;

  for ((x,y) <- inputs) do
    if (x > highestX) then
      highestX = x
    if (x < lowestX) then
      lowestX = x

    if (y > highestY) then
      highestY = y
    if (y < lowestY) then
      lowestY = y

  ((highestX+lowestX)/2, (highestY+lowestY)/2)



def loadValues(filename: String): ArrayBuffer[(Long, Long)] = 
  var values = ArrayBuffer[(Long, Long)]()
  var lines =  Source.fromResource(filename).getLines()

  for (line <- lines) do
    var split = line.split(",")
    values.addOne((split(0).toLong,split(1).toLong))

  values
