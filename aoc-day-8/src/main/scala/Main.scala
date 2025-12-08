import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer
import scala.math.pow

@main def hello(): Unit =
  star_one()
  star_two()
  

def star_two(): Unit = 
  // var values = loadValues("test.txt")
  var values = loadValues("input.txt")

  var sorted = getSortedDistances(values)

  var finalPair = connect_until_together(sorted, values.length)

  var toTuples = (values(finalPair._1), values(finalPair._2))

  println(toTuples._1._1 * toTuples._2._1)





def connect_until_together(inputs: ArrayBuffer[(Int, Int, Double)], nDistinct: Int): (Int, Int) =
  var accountedFor = ArrayBuffer[Int]()

  var ret = ArrayBuffer[ArrayBuffer[Int]]()
  
  var listsIN = ArrayBuffer[Int]()

  var pair: (Int, Int, Double) = (0,0,0.0)

  var i = 0;
  
  while (accountedFor.length != nDistinct || ret.length != 1)
    pair = inputs(i)
    accountedFor.addOne(pair._1)
    accountedFor.addOne(pair._2)
    accountedFor = accountedFor.distinct
    for (k <- 0 until ret.length)
      var connected = ret(k)
      if (connected.contains(pair._1) || connected.contains(pair._2)) then
        listsIN.addOne(k)
    

    if (listsIN.length == 0) then
      var newConnection = ArrayBuffer[Int]()
      newConnection.addOne(pair._1)
      newConnection.addOne(pair._2)
      ret.addOne(newConnection)

    else if (listsIN.length == 1) then 
      var connection = ret(listsIN(0))
      connection.addOne(pair._1)
      connection.addOne(pair._2)
      ret(listsIN(0)) = connection.distinct

    else
      var newRet = ArrayBuffer[ArrayBuffer[Int]]()
      var newConnection = ArrayBuffer[Int]()
      for (j <- 0 until ret.length) do
          if (listsIN.contains(j)) then
            newConnection = newConnection.concat(ret(j)).distinct
          else
            newRet.addOne(ret(j))
      newRet.addOne(newConnection)
      ret = newRet

    listsIN = ArrayBuffer[Int]()
    i += 1;

  // println(ret)

  (pair._1, pair._2)



def star_one(): Unit = 
  var values = loadValues("input.txt")
  // var values = loadValues("test.txt")

  // println(values)

  var sorted = getSortedDistances(values)

  // println(sorted)

  var nshort = nShortest(sorted, 1000).sortBy(_.length).reverse
  // var nshort = nShortest(sorted, 10).sortBy(_.length).reverse


  // println(nshort)

  var product = 1;
  for (i <- 0 until 3) do
    product *= nshort(i).length

  println(product)

def nShortest(inputs: ArrayBuffer[(Int, Int, Double)], n: Int): ArrayBuffer[ArrayBuffer[Int]] =
  var ret = ArrayBuffer[ArrayBuffer[Int]]()
  
  var listsIN = ArrayBuffer[Int]()
  
  for (i <- 0 until n) do
    var pair = inputs(i)
    for (k <- 0 until ret.length)
      var connected = ret(k)
      if (connected.contains(pair._1) || connected.contains(pair._2)) then
        listsIN.addOne(k)
    

    if (listsIN.length == 0) then
      var newConnection = ArrayBuffer[Int]()
      newConnection.addOne(pair._1)
      newConnection.addOne(pair._2)
      ret.addOne(newConnection)

    else if (listsIN.length == 1) then 
      var connection = ret(listsIN(0))
      connection.addOne(pair._1)
      connection.addOne(pair._2)
      ret(listsIN(0)) = connection.distinct

    else
      var newRet = ArrayBuffer[ArrayBuffer[Int]]()
      var newConnection = ArrayBuffer[Int]()
      for (j <- 0 until ret.length) do
          if (listsIN.contains(j)) then
            newConnection = newConnection.concat(ret(j)).distinct
          else
            newRet.addOne(ret(j))
      newRet.addOne(newConnection)
      ret = newRet

    listsIN = ArrayBuffer[Int]()

  ret






def getDistance(p1: (Long, Long, Long), p2: (Long, Long, Long)): Double =
  var sumSquare = pow((p1._1 - p2._1),2)+pow((p1._2 - p2._2),2)+pow((p1._3 - p2._3),2)

  var rt = pow(sumSquare, 0.5)

  rt


def getSortedDistances(inputs: ArrayBuffer[(Long, Long, Long)]): ArrayBuffer[(Int, Int, Double)] =

  var unsorted = ArrayBuffer[(Int, Int, Double)]()

  for (x <- 0 until inputs.length) do
    for (y <- 0 until inputs.length) do
      if (y > x) then
        unsorted.addOne((x, y, getDistance(inputs(x), inputs(y))))

  println("Finished Finding All Distances")

  var sorted = unsorted.sortBy(_._3)

  println("Finished sorting")

  sorted


def loadValues(filename: String): ArrayBuffer[(Long, Long, Long)] = 
  var values = ArrayBuffer[(Long, Long, Long)]()
  var lines =  Source.fromResource(filename).getLines()

  for (line <- lines) do
    var split = line.split(",")
    values.addOne((split(0).toLong,split(1).toLong,split(2).toLong))

  values

