import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer
import scala.math.pow
import scala.math.abs
import scala.util.boundary, boundary.break


@main def hello(): Unit =
  // star_one()
  star_two()

  ()

def star_two(): Unit =
  // var values = loadValues("test.txt")
  var values = loadValues("input.txt")

  var outsideLines = getOutSideLines(values)

  // println(values)
  // println(outsideLines)

  var res = findLargestInBounds(values, outsideLines)

  println()
  println(res)

  // var inBounds = checkSqaureInBounds(values, outsideLines, (5424,67450), (94703,50308), false)

  // println(inBounds)
  

  ()



def findLargestInBounds(inputs: ArrayBuffer[(Long, Long)], outsideLines: ArrayBuffer[Line]): Long =
  var res: Long = 0;

  var unsorted = ArrayBuffer[(Long, Long, Long)]();

  var totalToCheck = inputs.length * inputs.length/2

  var count = 0;

  var largest = 0.toLong
  var largestp1: (Long, Long) =(0,0)
  var largestp2: (Long, Long) = (0,0)

  for (x <- 0 until inputs.length) do
    for (y <- 0 until inputs.length) do
      if (y > x) then
        var p1 = inputs(x)
        var p2 = inputs(y)

        var size = getSize(p1, p2);
        if (size > largest) then
          if (checkSqaureInBounds(inputs, outsideLines, p1, p2, false)== true) then
            largest = siz
            largestp1 = p1
            largestp2 = p2


        // println(p1)
        // println(p2)
        // var inBounds = checkSqaureInBounds(inputs, outsideLines, p1, p2, false);
        // println(inBounds)
        // println()

        // if (getSize(inputs(x), inputs(y)) == 1530527040.toLong) then
        //   println(p1)
        //   println(p2)
        //   println()

        // if (inBounds == true) then
        //   unsorted.addOne((x, y, getSize(inputs(x), inputs(y))))

      count += 1;
      // if (count % 1000 == 0) then
      // println("Checked " + count + "/" + totalToCheck)

  // var sorted = unsorted.sortBy(_._3).reverse
  // res = sorted(0)._3

  // var p1 = inputs(sorted(0)._1.toInt);
  // var p2 = inputs(sorted(0)._2.toInt);

  checkSqaureInBounds(inputs, outsideLines, largestp1, largestp2, true)

  res = largest
  res


def checkSqaureInBounds(inputs: ArrayBuffer[(Long, Long)], outsideLines: ArrayBuffer[Line], corner1:(Long, Long), corner2:(Long, Long), debug: Boolean): Boolean =

  if (debug) then
    println(corner1)
    println(corner2)


  var corner3 = (corner1._1, corner2._2)
  var corner4 = (corner2._1, corner1._2)

  if (debug) then
    println(corner3)
    println(corner4)

  var squareCorners = Array(corner3, corner4)


  // var squareCorners =  ArrayBuffer[(Long, Long)]()

  // var higherX = corner1._1
  // var lowerX = corner2._1

  // var higherY = corner1._2
  // var lowerY = corner2._2

  // if (corner2._1 > corner1._1) then
  //   higherX = corner2._1
  //   lowerX = corner1._1

  // if (corner2._2 > corner1._2) then
  //   higherY = corner2._2
  //   lowerY = corner1._2

  // for (x <- lowerX to higherX) do
  //   squareCorners.addOne((x, higherY))
  //   squareCorners.addOne((x, lowerY))
  // for (y <- lowerY to higherY) do
  //   squareCorners.addOne((higherX, y))
  //   squareCorners.addOne((lowerX, y))
  
  // squareCorners = squareCorners.distinct


  var squareCornersNotOnLines = ArrayBuffer[(Long, Long)]()

  for (c <- squareCorners) do
    if (checkPointSitsOnAnyLine(outsideLines, c)) then
      ()
    else if (checkPointSitsOnAnyRed(inputs, c)) then
      ()
    else
      squareCornersNotOnLines.addOne(c)

  if (debug) then
    println(squareCornersNotOnLines)

  // println(squareCornersNotOnLines)

  var inBounds = true
  var count = 0
  boundary:
    for (c <- squareCornersNotOnLines) do
      var closestNorth: Option[Line] = None
      var closestSouth: Option[Line] = None
      var closestEast: Option[Line] = None
      var closestWest: Option[Line] = None

      var closestNorthDistance = Long.MinValue
      var closestSouthDistance = Long.MaxValue
      var closestEastDistance= Long.MaxValue
      var closestWestDistance = Long.MinValue

      if (debug) then
        println("lines for")
        println(c)

      for (l <- outsideLines) do  
        if (l.checkPointInBounds(c) == true) then

          if (debug) then
            println(l)

          var distDir = findDistanceFromLine(c, l);

          var dist = distDir._1
          var dir = distDir._2


          if (dir == CompassDirection.North) then
            if (dist > closestNorthDistance) then
              closestNorthDistance = dist
              closestNorth = Some(l)
          else if (dir == CompassDirection.South) then
            if (dist < closestSouthDistance) then
              closestSouthDistance = dist
              closestSouth = Some(l)
          else if (dir == CompassDirection.East) then
            if (dist < closestEastDistance) then
              closestEastDistance = dist
              closestEast = Some(l)
          else if (dir == CompassDirection.West) then
            if (dist > closestWestDistance) then
              closestWestDistance = dist
              closestWest= Some(l)
      if (closestNorth.isEmpty || closestSouth.isEmpty || closestEast.isEmpty || closestWest.isEmpty) then
        inBounds = false;
        break()

      else

        if (debug) then
          println()
          println(closestNorthDistance)
          println(closestSouthDistance)
          println(closestEastDistance)
          println(closestWestDistance)

          println(closestNorth)
          println(closestSouth)
          println(closestEast)
          println(closestWest)

      
        if (closestNorth.get.getOutDir() == OutsideDirection.Positive) then 
          inBounds = false;
          break()
        if (closestSouth.get.getOutDir() == OutsideDirection.Negative) then 
          inBounds = false;
          break()

        if (closestWest.get.getOutDir() == OutsideDirection.Positive) then 
          inBounds = false;
          break()
        if (closestEast.get.getOutDir() == OutsideDirection.Negative) then 
          inBounds = false;
          break()



  inBounds

enum CompassDirection:
  case North, South, East, West


def findDistanceFromLine(point: (Long, Long), line: Line): (Long, CompassDirection) =
  var distance = 0.toLong;

  var comp = CompassDirection.North

  if (line.getAlDir() == AlignmentDirection.Leftright) then
    distance = line.getY1() - point._2
    if (distance > 0) then
      comp = CompassDirection.South
    else
      comp = CompassDirection.North
  else
    distance = line.getX1() - point._1
    if (distance > 0) then
      comp = CompassDirection.East
    else
      comp = CompassDirection.West

  


  (distance, comp);



def checkPointSitsOnAnyRed(inputs: ArrayBuffer[(Long, Long)], point:(Long, Long)): Boolean = 
  var sitsOnAny = false;
  for (p <- inputs) do
    if (p == point) then
      sitsOnAny = true;

  sitsOnAny

def checkPointSitsOnAnyLine(outsideLines: ArrayBuffer[Line], point:(Long, Long)): Boolean =
  var sitsOnAny = false;

  for (l <- outsideLines) do
    sitsOnAny = sitsOnAny || l.checkPointSitsOn(point)

  sitsOnAny


class Line(var x1: Long, var x2: Long, var y1: Long, var y2: Long):
  private var outDir: OutsideDirection = OutsideDirection.Negative;
  private var alDir: AlignmentDirection = AlignmentDirection.Updown;

  if (y1 == y2) then
    alDir = AlignmentDirection.Leftright

  if (alDir == AlignmentDirection.Leftright) then
    if (x1 > x2) then
      outDir = OutsideDirection.Positive
  else
    if (y1 < y2) then
        outDir = OutsideDirection.Positive

  def getX1(): Long =
    x1

  def getX2(): Long =
    x2
  
  def getY1(): Long =
    y1

  def getY2(): Long =
    y2

  def getAlDir(): AlignmentDirection =
    alDir
  
  def getOutDir(): OutsideDirection =
    outDir

  override def toString(): String =
    var x = "(" + x1.toString() + "," + y1.toString() + ")" + "->" + "(" + x2.toString() + "," + y2.toString() + ") "  + alDir.toString() + " " + outDir.toString()
    
    x

  def checkPointOutside(point: (Long, Long)): Boolean =
    var isOutside = false;

    //Check if the line is within the bounds of us
    if (checkPointInBounds(point) == true) then
      if (alDir == AlignmentDirection.Leftright) then
        if (outDir == OutsideDirection.Positive) then
          if (point._2 > y1) then
            isOutside = true

        else
          if (point._2 < y1) then
            isOutside = true
      else
        if (outDir == OutsideDirection.Positive) then
          if (point._1 > x1) then
            isOutside = true

        else
          if (point._1  < x1) then
            isOutside = true


    isOutside


  def checkPointInBounds(point: (Long, Long)): Boolean =
    var inBounds = false;

    if (alDir == AlignmentDirection.Leftright) then
      var higherX: Long = x2;
      var lowerX: Long = x1;
      if (x1 > x2) then 
        higherX = x1;
        lowerX = x2;

      if (point._1 >= lowerX && point._1 <= higherX) then
        inBounds = true;
    else
      var higherY: Long = y2;
      var lowerY: Long = y1;
      if (y1 > y2) then 
        higherY = y1;
        lowerY = y2;

      if (point._2 >= lowerY && point._2 <= higherY) then
        inBounds = true;
    
    inBounds

  def checkPointSitsOn(point: (Long, Long)): Boolean =
    var sitsOn = false;

    if (alDir == AlignmentDirection.Leftright) then
      var higherX: Long = x2;
      var lowerX: Long = x1;
      if (x1 > x2) then 
        higherX = x1;
        lowerX = x2;

      if (point._2 == y1 && point._1 >= lowerX && point._1 <= higherX) then
        sitsOn = true;
    else
      var higherY: Long = y2;
      var lowerY: Long = y1;
      if (y1 > y2) then 
        higherY = y1;
        lowerY = y2;

      if (point._1 == x1 && point._2 >= lowerY && point._2 <= higherY) then
        sitsOn = true;
    
    sitsOn


  def checkLineOutside(other: Line): Boolean = 
    var isOutside = false;

    //Only do the check if the lines are perpendicular to each other
    if (alDir != other.getOutDir()) then
      //Check if the line is within the bounds of us
      if (checkInBounds(other) == true) then
        if (alDir == AlignmentDirection.Leftright) then
          if (outDir == OutsideDirection.Positive) then
            if (other.getY1() > y1 || other.getY2() > y1) then
              isOutside = true

          else
            if (other.getY1() < y1 || other.getY2() < y1) then
              isOutside = true
        else
          if (outDir == OutsideDirection.Positive) then
            if (other.getX1() > x1 || other.getX2() > x1) then
              isOutside = true

            else
              if (other.getX1() < x1 || other.getX2() < x1) then
                isOutside = true


    isOutside
    

  def checkInBounds(other: Line): Boolean =
    var inBounds = false;

    if (alDir == AlignmentDirection.Leftright) then
      var higherX: Long = x2;
      var lowerX: Long = x1;
      if (x1 > x2) then 
        higherX = x1;
        lowerX = x2;

      if (other.getX1() >= lowerX && other.getX1() <= higherX) then
        inBounds = true;
    else
      var higherY: Long = y2;
      var lowerY: Long = y1;
      if (y1 > y2) then 
        higherY = y1;
        lowerY = y2;

      if (other.getY1() >= lowerY && other.getY1() <= higherY) then
        inBounds = true;
    
    inBounds
  





enum OutsideDirection:
  case Positive, Negative

enum AlignmentDirection:
  case Updown, Leftright


def getOutSideLines(inputs: ArrayBuffer[(Long, Long)]): ArrayBuffer[Line] =
  var values = ArrayBuffer[Line]()

  for (i <- 0 until inputs.length) do
    var p1 = inputs(i)
    var p2 = inputs((i+1)%inputs.length)

    var x1 = p1._1
    var y1 = p1._2;
    var x2 = p2._1
    var y2 = p2._2

    // if (x1 == x2) then
    //   ()
    // else if (x1 > x2) then
    //   x1 -= 1
    //   x2 += 1
    // else
    //   x1 += 1
    //   x2 -= 1

    // if (y1 == y2) then
    //   ()
    // else if (y1 > y2) then
    //   y1 -= 1
    //   y2 += 1
    // else
    //   y1 += 1
    //   y2 -= 1

    var newLine = Line(x1, x2, y1, y2)
    // var newLine = Line(x2,x1,y2,y1)

    

    values.addOne(newLine);

  values


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
