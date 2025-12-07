import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec


@main def hello(): Unit =
  star_one()
  star_two()

def star_two(): Unit =
  // var grid = loadValues("test.txt")
  var grid = loadValues("input.txt")
  
  // println(grid)

  var sourceCoords = findSource(grid)

  var splitters = findSplitters(grid)

  var maxX = grid(0).length - 1

  var maxY = grid.length - 1

  var collisions = countCollisionsQuantum(grid, sourceCoords, splitters.map(x => (x._1, x._2, 0.toLong)), maxX, maxY);

  println(collisions)
  ()



def countCollisionsQuantum(grid: ArrayBuffer[ArrayBuffer[String]], sourceCoords:  (Int, Int), splitters:  ArrayBuffer[(Int, Int, Long)], maxX: Int, maxY: Int): Long = 
  var numHit: Long = 0.toLong;

  //No hit lasers are the lasers that never hit the target, the final number in the tuple represents how many timelines they would "hit" with if they did split
  //By summing these values we get the number of worlds
  var noHitLasers = ArrayBuffer[(Int, Int, Long)]()

  //Our generated lasers is the lasers we are going to look for a splitter for on a certain iteration
  var generatedLasers = ArrayBuffer[(Int, Int, Long)]()

  generatedLasers.addOne((sourceCoords._1, sourceCoords._2, 1))

  //Iterate down the grid
  for (y <- 0 to maxY) do
    //Go through all our splitters
    for (s <- splitters) do
      //IF they are at the current y level down, generate their lasers based on the number of times theyve been hit
      if (s._2 == y) then 
        //Make sure lasers arent generated off the grid
        if (s._1 - 1 >= 0) then
          //Add a generated laser with a "power" value equal to the number of times the splitter has been hit
          generatedLasers.addOne((s._1 - 1, s._2, s._3))
        if (s._1 + 1 <= maxX) then
          generatedLasers.addOne((s._1 + 1, s._2, s._3))
   
    //Iterate through all our generated lasers
    for (c <- 0 until generatedLasers.length) do
      var laser = generatedLasers(c);
      //use a variable to track whether weve hit a splitter
      //Once we have hit a splitter we dont want to hit anymore splitters as the laserr should terminate
      //It would be nicer to use a while loop here so we could exit the loop as soon as we hit one byt oh well
      var foundOne = false;
      for (x <- 0 until splitters.length) do
        var s = splitters(x)
        //IF x of splitter and laser is same and the splitter is further down the grid in the y then we count a collision to the splitter
        if (s._1 == laser._1 && s._2 > laser._2 && foundOne == false) then
          foundOne = true
          //Splitter keeps track of the number of times it is hit in its tuple
          //The number of times it is hit is equal to the power of the laser
          splitters(x) = (s._1, s._2, s._3 + laser._3)
      //If a laser doesnt hit any splitters, its a no hit laser
      if (foundOne == false) then
        noHitLasers.addOne(laser)
    //All generated lasers are dealt with now, we will make new ones in the next loop
    generatedLasers = ArrayBuffer[(Int, Int, Long)]()
    
  //Count collective power of noHitLasers
  for (noHits <- noHitLasers) do
    numHit += noHits._3

  numHit
  

def star_one(): Unit = 
  // var grid = loadValues("test.txt")
  var grid = loadValues("input.txt")
  
  // println(grid)

  var sourceCoords = findSource(grid)
  var sourceCoordsInput = ArrayBuffer[(Int, Int)]();
  sourceCoordsInput.addOne(sourceCoords)

  var splitters = findSplitters(grid)


  var maxX = grid(0).length - 1

  var collisions = countCollisionsRecursive(grid, sourceCoordsInput, splitters.map(x => (x._1, x._2, false)), maxX);

  println(collisions)
  ()


def countCollisionsRecursive(grid: ArrayBuffer[ArrayBuffer[String]], sourceCoords:  ArrayBuffer[(Int, Int)], splitters:  ArrayBuffer[(Int, Int, Boolean)], maxX: Int): Int = 
  var laserOrigins = ArrayBuffer[(Int, Int)]();

  var numHit = 0;

  for (l <- sourceCoords) do
    var foundOne = false;
    for (x <- 0 until splitters.length) do
      var s = splitters(x)
      if (s._1 == l._1 && s._2 > l._2 && foundOne == false) then
        foundOne = true
        if (s._3 == false)
          splitters(x) = (s._1, s._2, true)
          numHit += 1;
          if (s._1 - 1 >= 0) then
            laserOrigins.addOne((s._1 - 1, s._2))
          if (s._1 + 1 <= maxX) then
            laserOrigins.addOne((s._1 + 1, s._2))


  if (numHit != 0)
    numHit = numHit + countCollisionsRecursive(grid, laserOrigins, splitters, maxX)
  
  numHit
  
def printGrid(grid: ArrayBuffer[ArrayBuffer[String]]): Unit =
  for (y <- grid) do
    for (x <- y) do
      print(x)
    println()

  ()

  

def findSplitters(grid: ArrayBuffer[ArrayBuffer[String]]): ArrayBuffer[(Int, Int)] = 
  var splitters = ArrayBuffer[(Int, Int)]();
  for (y <- 0 until grid.length) do
    for (x <- 0 until grid(y).length) do
      if (grid(y)(x) == "^") then
        splitters.addOne((x,y))
  
  splitters


def findSource(grid: ArrayBuffer[ArrayBuffer[String]]): (Int, Int) = 
  var ox = -1;
  var oy = -1;
  for (y <- 0 until grid.length) do
    for (x <- 0 until grid(y).length) do
      if (grid(y)(x) == "S") then
        ox = x;
        oy = y;
  (ox,oy)


def loadValues(filename: String): ArrayBuffer[ArrayBuffer[String]] = 
  var lines =  Source.fromResource(filename).getLines()

  var allinputs = ArrayBuffer[ArrayBuffer[String]]();

  for (line <- lines) do 
    // println(line)
    var lineBuffer = ArrayBuffer[String]()
    for (x <- line) do
      lineBuffer.addOne(x.toString())
    allinputs.addOne(lineBuffer)

  allinputs


  
  
