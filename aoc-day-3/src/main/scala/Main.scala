import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer

@main def main(): Unit =
//  star_one()
  star_one_better()
  star_two()


def star_one_better(): Unit =
  var inputs = loadValues("input.txt")
  // var inputs = loadValues("test.txt")
    
  var mapped  = inputs.map((x:String) => findHighJoltageGeneral(x, 2))

  // println(mapped)

  var reduced = mapped.reduceLeft((x, y) => x + y)

  println(reduced)


def star_two(): Unit =
  var inputs = loadValues("input.txt")
  // var inputs = loadValues("test.txt")
    
  var mapped  = inputs.map((x:String) => findHighJoltageGeneral(x, 12))

  // println(mapped)

  var reduced = mapped.reduceLeft((x, y) => x + y)

  println(reduced)


def findHighJoltageGeneral(voltageList: String, digits: Int): Long =
  // println()
  var highest_digits = findHighJoltageRecursive(voltageList.split("").array.map(_.toLong), digits)

  var result = highest_digits.map(_.toString()).reduceLeft((x,y) => x+y).toLong //Expensive, is a power of 10 based solution better?

  result

def findHighJoltageRecursive(voltageList: Array[Long], digits: Int): Array[Long] =
  // println(voltageList.to(ArrayBuffer).toString + " " + digits.toString())
  var highest_digits = new Array[Long](digits);

  var highest: Long = 0;
  var highest_index = -1;

  for (x <- 0 to voltageList.length-digits) do //Bound for loop by number of digits, the first digit must be at least "digits" from the end
    val v = voltageList(x);

    if (v > highest){
      highest = v;
      highest_index = x;
      
    }
  
  if (digits == 1) then
        
    highest_digits = Array(highest)
  else
    highest_digits = Array(highest).concat(findHighJoltageRecursive(voltageList.slice(highest_index+1, voltageList.length), digits-1))

  highest_digits




def star_one(): Unit = 
  // var inputs = loadValues("test.txt")
  var inputs = loadValues("input.txt")
  
  var mapped  = inputs.map(findHighJoltage(_))

  var reduced = mapped.reduceLeft((x, y) => x + y)

  println(reduced)

def findHighJoltage(voltages: String): Int =
  var voltageList = voltages.split("").array.map(_.toInt)
  
  var highest = 0;
  var secondHighest = 0;

  for (x <- 0 until voltageList.length) do
    val v = voltageList(x);
    if (x != voltageList.length-1) then //If we arent at the very last slot then we can possibly find the highest first digit
      if (v > highest) then
        highest = v;
        secondHighest = 0; //Reset the second highest, since it must come after the highest
      else if (v > secondHighest) then
        secondHighest = v
    else if (v > secondHighest) then
        secondHighest = v
      
  var num = highest*10 + secondHighest

  num

def loadValues(filename: String): ArrayBuffer[String] =
  var values = ArrayBuffer[String]()

  var lines =  Source.fromResource(filename).getLines()

  values.addAll(lines)

  values