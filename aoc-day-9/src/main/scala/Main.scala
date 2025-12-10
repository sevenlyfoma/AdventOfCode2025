import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer
import scala.math.pow
import scala.math.abs
import scala.util.boundary, boundary.break


@main def hello(): Unit =
  star_one()
  star_two()

  ()

def star_one(): Unit =
  // var values = loadValues("test.txt")
  var values = loadValues("input.txt")

  var biggest = justCalculateItAll(values)

  println(biggest)

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

def star_two(): Unit =
    var values = loadValues("test.txt")
    // var values = loadValues("input.txt")
    // var values = loadValues("test2.txt")
  
    var outsideLines = getOutSideLines(values)

    // println(values)
    // println(outsideLines)

    // var isInside = checkInsideShape(outsideLines, (9,5), (2,3))

    var largest = findLargestInBounds(values, outsideLines);

    println(largest)

def findLargestInBounds(inputs: ArrayBuffer[(Long, Long)], outsideLines: ArrayBuffer[Line]): Long =
  var res: Long = 0;

//   var unsorted = ArrayBuffer[(Long, Long, Long)]();

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
          if (checkInsideShape(outsideLines, p1, p2, false)) then
            largest = size
            largestp1 = p1
            largestp2 = p2


        // if (size == 1530527040) then 
        //     var ins = checkInsideShape(outsideLines, p1, p2, true)
        //     println()
        //     println("correct answer inside?")
        //     println(ins)

        // if (size == 50) then
        //     println(checkInsideShape(outsideLines, p1, p2, true))
        //     println()
                    


        

      count += 1;

//   println(largestp1)
//   println(largestp2)


  res = largest
  res

def getSize(p1: (Long, Long), p2: (Long, Long)): Long = 
  var x = (abs(p1._1 - p2._1)+1) * (abs(p1._2 - p2._2) + 1)
  x


def checkInsideShape(outsideLines: ArrayBuffer[Line], p1: (Long, Long), p2: (Long, Long), debug: Boolean): Boolean = 
    var isInside = true;

    if (debug) then
        println("debugging")
        println(p1)
        println(p2)

    var line1 = Line(p1._1, p1._1, p1._2, p2._2)
    var line2 = Line(p1._1, p2._1, p2._2, p2._2)
    var line3 = Line(p2._1, p2._1, p2._2, p1._2)
    var line4 = Line(p2._1, p1._1, p1._2, p1._2)

    var boxLines = Array(line1, line2, line3, line4)

    var boxLinesBuffer = ArrayBuffer[Line]();

    boxLinesBuffer.addAll(boxLines)

    if (debug) then
        println("BOXLINES")
        println(boxLinesBuffer)

    var linesThatAreOutsideOfSomeLine = ArrayBuffer[Line]();

    for (bl <- boxLinesBuffer) do
        var linesThatBound = ArrayBuffer[Line]();
        for (ol <- outsideLines) do
            if (checkLinesInBounds(ol, bl)) then
                linesThatBound.addOne(ol)

        var closestPositiveDistance = Long.MaxValue;
        var closestNegativeDistance = Long.MinValue;
        var closestPositiveLine: Option[Line] = None
        var closestNegativeLine: Option[Line] = None

        var lineToCheckOutside =  ArrayBuffer[Line]();
        for (ol <- linesThatBound) do
            var dists = getDistanceFromLine(ol, bl, debug);

            if (dists._1 == -1 || dists._2 == 1) then
                lineToCheckOutside.addOne(ol);
            else     
                if (dists._1 < closestPositiveDistance) then
                    closestPositiveDistance = dists._1
                    closestPositiveLine = Some(ol);

                if (dists._2 > closestNegativeDistance) then
                    closestNegativeDistance = dists._2
                    closestNegativeLine = Some(ol);

        if (debug) then
            println("closest pos neg")
            println(closestPositiveLine)
            println(closestNegativeLine)
            println()
        
        if (closestPositiveLine.nonEmpty) then
            lineToCheckOutside.addOne(closestPositiveLine.get)
        
        if (closestNegativeLine.nonEmpty) then
            lineToCheckOutside.addOne(closestNegativeLine.get)

        for (lll <- lineToCheckOutside) do
            var res = lineOutsideAndByHowMuch(lll, bl);
            if (res._1) then
                linesThatAreOutsideOfSomeLine.addOne(res._2)


        
            // var res = lineOutsideAndByHowMuch(ol, bl);
            // if (res._1) then
            //     linesThatAreOutsideOfSomeLine.addOne(res._2)
    
    linesThatAreOutsideOfSomeLine = linesThatAreOutsideOfSomeLine.distinct


    if (debug) then
        println("lINES OUTSIDE OF OTHERS")
        println(linesThatAreOutsideOfSomeLine)

    
    var allLinesInsideOthers = linesThatAreOutsideOfSomeLine.map(x => isLineTotallyInsideAnOutsideLine(x, outsideLines))

    // println(allLinesInsideOthers)

    for (b <- allLinesInsideOthers) do
        isInside = isInside && b;

    

    isInside

def getDistanceFromLine(outline: Line, boxLine: Line, debug: Boolean): (Long, Long) =
    var posDist = Long.MaxValue
    var negDist = Long.MinValue
    var p1 = Long.MaxValue
    var p2 = Long.MinValue

    if (outline.getAlDir() == AlignmentDirection.Leftright) then
        p1 = outline.y1 - boxLine.y1
        p2 = outline.y1 - boxLine.y2
    else
        p1 = outline.x1 - boxLine.x1
        p2 = outline.x1 - boxLine.x2

    
    
    

    if (p1 >= 0) then
        if (p1 < posDist) then
            posDist = p1
    if (p1 <= 0) then
        if (p1 > negDist) then
            negDist = p1

    if (p2 >= 0) then
        if (p2 < posDist) then
            posDist = p2
    if (p2 <= 0) then
        if (p2 > negDist) then
            negDist = p2

    if ((p1 > 0 && p2 < 0) || (p1 <0 && p2 > 0)) then
        posDist = -1;
        negDist = 1;

    
    if (debug) then
        println("subrtractions for distance")
        println(outline)
        println (p1)
        println (p2)
        println((posDist, negDist))
        println()



    
    (posDist, negDist)

def isLineTotallyInsideAnOutsideLine(l: Line, outsideLines: ArrayBuffer[Line]): Boolean = 
    var ret = false;

    for (ol <- outsideLines) do
        if (ol.getAlDir() == l.getAlDir()) then
            if (ol.getAlDir() == AlignmentDirection.Leftright) then
                if (l.y1 == ol.y1) then
                    if (((l.x1 >= ol.x1 && l.x1 <= ol.x2) || (l.x1 <= ol.x1 && l.x1 >= ol.x2)) && ((l.x2 >= ol.x1 && l.x2 <= ol.x2) || (l.x2 <= ol.x1 && l.x2 >= ol.x2))) then
                        ret = true;
            else
                if (l.x1 == ol.x1) then
                    if (((l.y1 >= ol.y1 && l.y1 <= ol.y2) || (l.y1 <= ol.y1 && l.y1 >= ol.y2)) && ((l.y2 >= ol.y1 && l.y2 <= ol.y2) || (l.y2 <= ol.y1 && l.y2 >= ol.y2))) then
                        ret = true;

    ret


def lineOutsideAndByHowMuch(l1: Line, l2: Line): (Boolean, Line) =
    var isOutside = false;
    var outsideSegment = Line(0,0,0,0);

    //We only care if a line is outside if theyre perpendicular
    if (l1.getAlDir() != l2.getAlDir())  then
        if (checkLinesInBounds(l1, l2)) then
            if (l1.getAlDir() == AlignmentDirection.Leftright) then
                var point1 = l1.y1 - l2.y1 //5 -> inside
                var point2 = l1.y1 - l2.y2 //-2 -> outside
                if (l1.getOutDir() == OutsideDirection.Positive) then
                    if (point1 < 0  && point2 <0) then
                        isOutside = true;
                        outsideSegment = l2
                    else if (point1 < 0) then
                        isOutside = true;
                        outsideSegment = Line(l2.x1,l2.x2,l2.y1,l2.y2+point2+1)
                    else if (point2 < 0) then
                        isOutside = true;
                        outsideSegment = Line(l2.x1,l2.x2,l2.y1+point1+1,l2.y2)
                else
                    if (point1 > 0  && point2 > 0) then
                        isOutside = true;
                        outsideSegment = l2
                    else if (point1 > 0) then
                        isOutside = true;
                        outsideSegment = Line(l2.x1,l2.x2,l2.y1,l2.y2+point2-1)
                    else if (point2 > 0) then
                        isOutside = true;
                        outsideSegment = Line(l2.x1,l2.x2,l2.y1+point1-1,l2.y2)
            else
                var point1 = l1.x1 - l2.x1 //5 -> inside
                var point2 = l1.x1 - l2.x2 //-2 -> outside
                if (l1.getOutDir() == OutsideDirection.Positive) then
                    if (point1 < 0  && point2 <0) then
                        isOutside = true;
                        outsideSegment = l2
                    else if (point1 < 0) then
                        isOutside = true;
                        outsideSegment = Line(l2.x1, l2.x2+point2+1, l2.y1, l2.y1)
                    else if (point2 < 0) then
                        isOutside = true;
                        outsideSegment = Line(l2.x1+point1+1, l2.x2, l2.y1, l2.y1)
                else
                    if (point1 > 0  && point2 > 0) then
                        isOutside = true;
                        outsideSegment = l2
                    else if (point1 > 0) then
                        isOutside = true;
                        outsideSegment = Line(l2.x1, l2.x2+point2-1, l2.y1, l2.y1)
                    else if (point2 > 0) then
                        isOutside = true;
                        outsideSegment = Line(l2.x1+point1-1, l2.x2, l2.y1, l2.y1)



    (isOutside, outsideSegment);

def checkLinesInBounds(l1: Line, l2: Line): Boolean = 
    var inBounds = false;

    if (l1.getAlDir() != l2.getAlDir())  then
        if (l1.getAlDir() == AlignmentDirection.Leftright) then
            if ((l2.x1 >= l1.x1 && l2.x1 <= l1.x2) || (l2.x1 >= l1.x2 && l2.x1 <= l1.x1) )
                inBounds = true;
        else
            if ((l2.y1 >= l1.y1 && l2.y1 <= l1.y2) || (l2.y1 >= l1.y2 && l2.y1 <= l1.y1) )
                inBounds = true;



    inBounds



def loadValues(filename: String): ArrayBuffer[(Long, Long)] = 
  var values = ArrayBuffer[(Long, Long)]()
  var lines =  Source.fromResource(filename).getLines()

  for (line <- lines) do
    var split = line.split(",")
    values.addOne((split(0).toLong,split(1).toLong))

  values

def getOutSideLines(inputs: ArrayBuffer[(Long, Long)]): ArrayBuffer[Line] =
  var values = ArrayBuffer[Line]()

  for (i <- 0 until inputs.length) do
    var p1 = inputs(i)
    var p2 = inputs((i+1)%inputs.length)

    var x1 = p1._1
    var y1 = p1._2;
    var x2 = p2._1
    var y2 = p2._2

    var newLine = Line(x1, x2, y1, y2)
    // var newLine = Line(x2,x1,y2,y1)

    

    values.addOne(newLine);

  values

enum OutsideDirection:
  case Positive, Negative

enum AlignmentDirection:
  case Updown, Leftright

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