import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer

//Start 8:48
//Pause at 9:22
//Restart at 9:40
//Star 1 finished at 9:49

@main def main(): Unit =
  star_one()
  

def star_one(): Unit =

  // var values = loadValues("test.txt")
  var values = loadValues("input.txt")

  var marked = markForkliftable(values);

  var count = count_xs(marked);

  println(count)


def count_xs(input: ArrayBuffer[ArrayBuffer[String]]): Int =
  var count = 0;
  for (x <- input) do 
    for (y <- x) do
      if (y.equals("x")) then
        count += 1

  count
  

def printGrid(input: ArrayBuffer[ArrayBuffer[String]]): Unit =  
  for (x <- input) do 
    for (y <- x) do
      print(y)
    println()

def markForkliftable(input: ArrayBuffer[ArrayBuffer[String]]): ArrayBuffer[ArrayBuffer[String]] = 
  for (y <- 0 until input.length) do
    var row = input(y);
    for (x <- 0 until row.length) do 
      if (row(x).equals("@")) then
        var adjfree = 0;
        for (inner_x <- x-1 to x+1) do 
          for (inner_y <- y-1 to y+1) do
            if (inner_x < 0 || inner_x >= row.length || inner_y < 0 || inner_y >= input.length) then
              adjfree +=1
            else
              if input(inner_y)(inner_x).equals("@") || input(inner_y)(inner_x).equals("x") then
                ()
              else
                adjfree +=1
        if adjfree >= 5 then
          row(x) = "x";


  input


def loadValues(filename: String): ArrayBuffer[ArrayBuffer[String]] =
  var values = ArrayBuffer[String]()

  var lines =  Source.fromResource(filename).getLines()

  values.addAll(lines)

  var split = values.map(_.split(""))

  var finValues = ArrayBuffer[ArrayBuffer[String]]();

  for (x <- split) do 
    var z = ArrayBuffer[String]()
    z.addAll(x)
    finValues.addOne(z)

  finValues