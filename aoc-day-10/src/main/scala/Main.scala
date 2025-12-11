import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer
import scala.math.pow
import scala.math.abs
import scala.util.boundary, boundary.break


@main def hello(): Unit =
  var machines = loadValues("test.txt")
  println(machines)


class Machine(var lights: ArrayBuffer[Int], var buttons: ArrayBuffer[ArrayBuffer[Int]], var joltages: ArrayBuffer[Int]):
  override def toString(): String =
    "Lights: " +lights.toString() + " Buttons: " + buttons.toString() + " Joltages: " + joltages.toString();

def loadValues(filename: String): ArrayBuffer[Machine] = 
  var values = ArrayBuffer[Machine]()
  var lines =  Source.fromResource(filename).getLines()

  for (line <- lines) do
    var split = line.split(" ")
    
    var lightsString = split(0)

    var buttonsStrings = split.slice(1, split.length-1)

    var joltagesString = split(split.length-1)

    var lights = lightsString.slice(1,lightsString.length()-1).map((x => if (x == '.') then 0 else 1)).to(ArrayBuffer)

    var buttons = ArrayBuffer[ArrayBuffer[Int]]();

    for (bs<- buttonsStrings) do
      var button = bs.slice(1,bs.length()-1).split(",").map(x=> x.toInt).to(ArrayBuffer)
      buttons.addOne(button)

    var joltages = joltagesString.slice(1, joltagesString.length()-1).split(",").map(x=>x.toInt).to(ArrayBuffer)


    var m = Machine(lights, buttons, joltages)

    values.addOne(m);



  values
