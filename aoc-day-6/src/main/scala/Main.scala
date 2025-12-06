import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer

@main def hello(): Unit =
  
  star_one();

def star_one(): Unit = 
  // var values = loadValues("test.txt")
  var values = loadValues("input.txt")

  println(values)

  var calced = values.map(_.doCalc())

  var summed = calced.reduce((x:Long,y:Long) => x + y)
  
  println(summed)

class Operation(values: ArrayBuffer[Long], op: String): 

  def doCalc(): Long = 
    var result: Long = 0
    if (op == "+") then
      result = values.reduce((x:Long,y:Long) => x + y)
    if (op == "*") then 
      result = values.reduce((x:Long,y:Long) => x * y)
    result

  override def toString(): String =
    "Operation(" + values.toString() + "," + op + ")"


def loadValuesNew(filename: String): ArrayBuffer[Operation] = 
  var lines =  Source.fromResource(filename).getLines()

  var allinputs = ArrayBuffer[ArrayBuffer[String]]();

  var values = ArrayBuffer[Operation]()


  values




def loadValues(filename: String): ArrayBuffer[Operation] = 
  var values = ArrayBuffer[Operation]()
  var lines =  Source.fromResource(filename).getLines()

  var allinputs = ArrayBuffer[ArrayBuffer[String]]();

  for (line <- lines) do 
    var lineBuffer = ArrayBuffer[String]()
    var word = ""
    var justAdded = false
    for (x <- line) do
      justAdded = false
      if (x == ' ') then
        if (!word.equals("")) then
          lineBuffer.addOne(word)
          word = ""
          justAdded = true
      else
        word = word + x.toString()
    if (!justAdded) then
      lineBuffer.addOne(word)

    // println(lineBuffer)
    allinputs.addOne(lineBuffer)

  for (i <- 0 until allinputs(0).length) do 
    var vals = ArrayBuffer[Long]()
    for (j <- 0 until allinputs.length - 1) do 
      vals.addOne(allinputs(j)(i).toLong)
    
    var op = Operation(vals, allinputs(allinputs.length-1)(i))
    values.addOne(op)

  values