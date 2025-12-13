import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer
import scala.math.pow
import scala.math.abs
import scala.util.boundary, boundary.break


@main def hello(): Unit =
  // var machines = loadValues("test.txt")
  // // println(machines)

  // // getAllButtonCombinationsOfSizeN(machines(0).buttons.length, 2)

  // var subsets = getPowerSet(machines(0).buttons.length)

  // println(findShortestSequence(machines(0), subsets))
  // star_one();
  star_two();
  ()


//Nice solution to copy at some point
//https://www.reddit.com/r/adventofcode/comments/1pk87hl/comment/ntp4njq/

//Linear algebra solution to understand at some point
//https://www.reddit.com/r/adventofcode/comments/1pl8nsa/2025_day_10_part_2_is_this_even_possible_without/


def star_two(): Unit = 
  var machines = loadValues("input.txt")
  // var machines = loadValues("test.txt")

  var sum = 0;

  var count = 0;

  for (m <- machines) do
    sum += findShortestJoltageSequence(m)

    count += 1;

    println(count)

  println(sum)
  
  // for (i <- 1 until 15) do 
  //   println(i)
  //   println(getCombinationsOfSizeN(4, i))

  // println(getCombinationsOfSizeN(4, 10))

def findShortestJoltageSequence(m: Machine): Int = 
  var shortestSequence = m.joltages.max - 1;
  var found = false;
  var numberOfButtons = m.buttons.length

  var newLights = m.joltages.clone()

  while (!found) do
    shortestSequence += 1;
    

    var counters = ArrayBuffer[Int]();
    for (i <-0 until shortestSequence) do
      counters.addOne(numberOfButtons-1);

    while (counters(0) != -1 && (found == false)) do
      found = found || testJoltageSequence(m, counters, newLights);

      counters(counters.length-1) -= 1

      updateCountersAfterSubtractionUnit(numberOfButtons, counters.length-1, counters);
    

    // var combinations = getCombinationsOfSizeN(m.buttons.length, shortestSequence);


    // for (c <- combinations) do
      

    // if (shortestSequence > 10) then
    //   found = true
    //   shortestSequence = 101
  // println(shortestSequence)
  
  shortestSequence


def testJoltageSequence(m: Machine, buttonPresses: ArrayBuffer[Int], newLights: ArrayBuffer[Int]): Boolean =
  for (i<- 0 until m.joltages.length) do
    newLights(i) = m.joltages(i)


  for (i <- buttonPresses) do
    var button = m.buttons(i)
    for (j <- button) do
      newLights(j) -= 1;

  var allOff = true;

  for (x <- newLights) do
    allOff = allOff && (x <= 0)

  // newLights.map(x => if (x == 0) then true else false).reduce((x,y) => x && y)

  allOff
def updateCountersAfterSubtractionUnit(numberOfButtons: Int, pivot: Int, counters: ArrayBuffer[Int]): Unit = 
  if (counters(pivot) == -1) then
    if (pivot == 0) then
      ()
    else
      counters(pivot - 1) -= 1;
      counters(pivot) = numberOfButtons - 1;

      updateCountersAfterSubtractionUnit(numberOfButtons, (pivot-1), counters);

  else
    ()

// def updateCountersAfterSubtraction(numberOfButtons: Int, pivot: Int, counters: ArrayBuffer[Int]): ArrayBuffer[Int] =
//   if (counters(pivot) == -1) then
//     if (pivot == 0) then
//       counters
//     else
//       counters(pivot - 1) -= 1;
//       counters(pivot) = numberOfButtons - 1;

//       updateCountersAfterSubtraction(numberOfButtons, (pivot-1), counters);

//   else
//     counters



// def getCombinationsOfSizeN(numberOfButtons: Int, n: Int): ArrayBuffer[ArrayBuffer[Int]] = 
//   var combinations = ArrayBuffer[ArrayBuffer[Int]]()

//   var counters = ArrayBuffer[Int]();
//   for (i <-0 until n) do
//     counters.addOne(numberOfButtons-1);

//   while (counters(0) != -1) do
//     combinations.addOne(counters.clone())

//     counters(counters.length-1) -= 1

//     counters = updateCountersAfterSubtraction(numberOfButtons, counters);


//   combinations = combinations.map(x => x.sorted).distinct

//   combinations

// def updateCountersAfterSubtraction(numberOfButtons: Int, counters: ArrayBuffer[Int]): ArrayBuffer[Int] =
//   if (counters(counters.length-1) == -1) then
//     if (counters.length == 1) then
//       counters
//     else
//       var smallerCounters = counters.slice(0, counters.length-1);
//       smallerCounters(smallerCounters.length - 1) -= 1;

//       var newCounters = updateCountersAfterSubtraction(numberOfButtons, smallerCounters);
//       newCounters.addOne(numberOfButtons-1)
//       newCounters

//   else
//     counters


def star_one(): Unit =
  // var machines = loadValues("test.txt")
  var machines = loadValues("input.txt")

  var sum = 0;

  for (m <- machines) do
    var combinations = getPowerSet(m.buttons.length);

    sum += findShortestSequence(m, combinations);

  println(sum)



def findShortestSequence(m: Machine, combinations: Iterator[Set[Int]]): Int = 
  var haveFoundOne = false
  var minPresses = 0;

  while (!haveFoundOne) do
    var combination = combinations.next();

    if (testSequence(m, combination)) then
      minPresses = combination.size
      haveFoundOne = true
    
  minPresses




def testSequence(m: Machine, buttonPresses: Set[Int]): Boolean =
  var newLights = m.lights.clone();

  for (i <- buttonPresses.iterator) do
    var button = m.buttons(i)
    for (j <- button) do
      var light = newLights(j)
      if (light == 0) then
        newLights(j) = 1
      else
        newLights(j) = 0

  newLights.map(x => if (x == 0) then true else false).reduce((x,y) => x && y)


def getPowerSet(numberOfButtons: Int): Iterator[Set[Int]] = 

  var s = Set[Int]()

  for (i <- 0 until numberOfButtons) do
    s = s + i;
  
  s.subsets()
    

 
  

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
