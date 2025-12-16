import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer
import scala.math.pow
import scala.math.abs
import scala.util.boundary, boundary.break


@main def hello(): Unit =

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

  // println(find_joltages_setup(machines(0)));
  // println(find_joltages_setup(machines(1)));
  // println(find_joltages_setup(machines(2)));

  var sum = 0;
  var count = 0;

  for (m <- machines) do
    var x =find_joltages_setup(m)
    sum += x
    // println(x)
    count += 1;
    println(count)

  println(sum)
  

def find_joltages_setup(m: Machine): Int =
  var powerSet = getPowerSet(m.buttons.length).to(ArrayBuffer);

  find_joltages_tail_recursive_start(m, powerSet)
  // find_jolatages_recursive(m, powerSet, 1, 0)

def find_joltages_tail_recursive_start(m: Machine, powerSet: ArrayBuffer[Set[Int]]): Int = 
  // println(m.joltages)
  if (m.joltages.map(x=> (x <= 0) ).reduce((x, y) => x && y) == false) then
    m.changeLightsToJoltageOdds();
    var singleSeq = findShortestSequenceAsSeq(m, powerSet.iterator)// * multiplicationValue

    

    var m2 = Machine(m.lights.clone(), m.buttons.clone(), m.joltages.clone());

    m2.changeLightsToJoltageOdds();

    var secondSingleSeq = findSecondShortestSequenceAsSeq(m2, powerSet.iterator)

    m.reduceJoltagesByButtons(singleSeq._2);
    m.halfJoltages();

    m2.reduceJoltagesByButtons(secondSingleSeq._2);
    m2.halfJoltages();

    var t1 = find_jolatages_tail_recursive(m, powerSet, 2, singleSeq._1 )

    var t2 = find_jolatages_tail_recursive(m2, powerSet.clone(), 2, secondSingleSeq._1)


    // println(t1.toString() + " " + t2.toString())

    if (t1<t2) then
      t1
    else
      t2
  else
    0

def find_jolatages_tail_recursive(m: Machine, powerSet: ArrayBuffer[Set[Int]], multiplicationValue: Int, runningTotal: Int): Int = 
  // println(m.joltages)
  if (m.joltages.map(x=> (x <= 0)).reduce((x, y) => x && y) == false) then
    m.changeLightsToJoltageOdds();
    // println(m.lights)

    var singleSeq = findShortestSequenceAsSeq(m, powerSet.iterator) 

    var seqSize = singleSeq._1 * multiplicationValue
    var seq = singleSeq._2

  

    m.reduceJoltagesByButtons(seq);
    // println(m.joltages)
    m.halfJoltages();

    // println(m.lights)

    // println(singleSeq)

    
    find_jolatages_tail_recursive(m, powerSet, multiplicationValue*2, runningTotal+seqSize)
  else
    runningTotal


def find_jolatages_recursive(m: Machine, powerSet: ArrayBuffer[Set[Int]], multiplicationValue: Int, runningTotal: Int): Int = 
  // println()
  // println(m.joltages)
  
  if (m.joltages.reduce((x, y) => x + y) != 0) then
    if (multiplicationValue == 1) then
      m.changeLightsToJoltageOdds();
      var singleSeq = findShortestSequenceAsSeq(m, powerSet.iterator)// * multiplicationValue

      

      var m2 = Machine(m.lights.clone(), m.buttons.clone(), m.joltages.clone());

      m2.changeLightsToJoltageOdds();

      var secondSingleSeq = findSecondShortestSequenceAsSeq(m2, powerSet.iterator)

      m.reduceJoltagesByButtons(singleSeq._2);
      m.halfJoltages();

      m2.reduceJoltagesByButtons(secondSingleSeq._2);
      m2.halfJoltages();

      var x1 = find_jolatages_recursive(m, powerSet, 2, 0)

      var x2 = find_jolatages_recursive(m2, powerSet.clone(), 2,0)

      var t1 = singleSeq._1 + 2*x1
      var t2 = secondSingleSeq._1 + 2*x2

      // println(t1.toString() + " " + t2.toString())

      if (t1<t2) then
        t1
      else
        t2
    else
      m.changeLightsToJoltageOdds();
      // println(m.lights)

      var singleSeq = findShortestSequenceAsSeq(m, powerSet.iterator)// * multiplicationValue

      var seqSize = singleSeq._1
      var seq = singleSeq._2

    

      m.reduceJoltagesByButtons(seq);
      // println(m.joltages)
      m.halfJoltages();

      // println(m.lights)

      // println(singleSeq)

      
      var x = find_jolatages_recursive(m, powerSet, 2, 0)

      seqSize + 2* (x)


  else
    0//runningTotal


def findSecondShortestSequenceAsSeq(m: Machine, combinations: Iterator[Set[Int]]): (Int, Set[Int]) = 
  var haveFoundOne = false
  var minPresses = Int.MaxValue;
  var minComb = Set[Int]()


  while (!haveFoundOne) do
    if (combinations.hasNext) then
      var combination = combinations.next();

      if (testSequence(m, combination)) then
        if (combination.size <= minPresses) then
            minPresses = combination.size
            minComb = combination
          
        else
          haveFoundOne = true
          minPresses = combination.size
          minComb = combination
    else
      haveFoundOne = true

    
    
  (minPresses, minComb)


  




  

def star_one(): Unit =
  // var machines = loadValues("test.txt")
  var machines = loadValues("input.txt")

  var sum = 0;

  for (m <- machines) do
    var combinations = getPowerSet(m.buttons.length);

    sum += findShortestSequence(m, combinations);

  println(sum)


def findShortestSequenceAsSeq(m: Machine, combinations: Iterator[Set[Int]]): (Int, Set[Int]) = 
  var haveFoundOne = false
  var minPresses = Int.MaxValue;
  var minComb = Set[Int]()

  while (!haveFoundOne) do
    if (combinations.hasNext) then
      var combination = combinations.next();

      if (testSequence(m, combination)) then
        minPresses = combination.size
        minComb = combination
        haveFoundOne = true
    else
      haveFoundOne = true
    
  (minPresses, minComb)


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

  def changeLightsToJoltageOdds(): Unit =
    for (i <- 0 until lights.length) do
      if (joltages(i) % 2 == 0) then
        lights(i) = 0
      else
        lights(i) = 1

  def reduceJoltagesByLights(): Unit = 
    for (i <- 0 until lights.length) do
      if (lights(i) != 0) then
        joltages(i) -= 1;

  def reduceJoltagesByButtons(set: Set[Int]): Unit = 

    for (i <- set) do
      var but = buttons(i)
      for (j <- but) do
        joltages(j) -= 1

  
  def halfJoltages(): Unit =
    for (i <- 0 until lights.length) do
      joltages(i) /= 2;


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



// [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
// [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
// [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}

// import scala.io.Source
// import scala.math
// import scala.collection.mutable.ArrayBuffer
// import scala.math.pow
// import scala.math.abs
// import scala.util.boundary, boundary.break


// @main def hello(): Unit =

//   // star_one();
//   star_two();
//   ()


// //Nice solution to copy at some point
// //https://www.reddit.com/r/adventofcode/comments/1pk87hl/comment/ntp4njq/

// //Linear algebra solution to understand at some point
// //https://www.reddit.com/r/adventofcode/comments/1pl8nsa/2025_day_10_part_2_is_this_even_possible_without/


// def star_two(): Unit = 
//   // var machines = loadValues("input.txt")
//   var machines = loadValues("test.txt")

//   println(find_joltages_setup(machines(0)));
  

// def find_joltages_setup(m: Machine): Int =
//   var powerSet = getPowerSet(m.buttons.length).to(ArrayBuffer);

//   find_jolatages_recursive(m, powerSet, 1, 0);


// def find_jolatages_recursive(m: Machine, powerSet: ArrayBuffer[Set[Int]], multiplicationValue: Int, runningTotal: Int): Int = 
  
//   if (m.joltages.reduce((x, y) => x + y) != 0) then

//     // if (multiplicationValue == 0) then
//     //   var singleSeq = findShortestSequenceAsSeq(m, powerSet.iterator)// * multiplicationValue

//     //   var secondSingleSeq = findSecondShortestSequenceAsSeq(m, powerSet.iterator)

//     //   var m2 = Machine(m.lights.clone(), m.buttons.clone(), m.joltages.clone());

//     //   m.reduceJoltagesByButtons(singleSeq._2);
//     //   m.halfJoltages();

//     //   m2.reduceJoltagesByButtons(secondSingleSeq._2);
//     //   m2.halfJoltages();

//     //   var x1 = find_jolatages_recursive(m, powerSet, 2, 0)

//     //   var x2 = find_jolatages_recursive(m2, powerSet, 2,0)

//     //   var t1 = singleSeq._1 + 2*x1
//     //   var t2 = secondSingleSeq._1 + 2*x2

//     //   if (t1<t2) then
//     //     t1
//     //   else
//     //     t2

//     // else


//     var singleSeq = findShortestSequenceAsSeq(m, powerSet.iterator)// * multiplicationValue
//     var seqSize = singleSeq._1
//     var seq = singleSeq._2


//     m.reduceJoltagesByButtons(seq);
//     m.halfJoltages();

    
//     var x = find_jolatages_recursive(m, powerSet, multiplicationValue*2, runningTotal + seqSize)

//     seqSize + 2* (x)


//   else
//     0//runningTotal 
  




  

// def star_one(): Unit =
//   // var machines = loadValues("test.txt")
//   var machines = loadValues("input.txt")

//   var sum = 0;

//   for (m <- machines) do
//     var combinations = getPowerSet(m.buttons.length);

//     sum += findShortestSequence(m, combinations);

//   println(sum)

// def findSecondShortestSequenceAsSeq(m: Machine, combinations: Iterator[Set[Int]]): (Int, Set[Int]) = 
//   var haveFoundOne = false
//   var minPresses = Int.MaxValue;
//   var minComb = Set[Int]()


//   while (!haveFoundOne) do
//     var combination = combinations.next();

//     if (testSequence(m, combination)) then
//       if (combination.size <= minPresses) then
//           minPresses = combination.size
//           minComb = combination
        
//       else
//         haveFoundOne = true
//         minPresses = combination.size
//         minComb = combination

    
    
//   (minPresses, minComb)


// def findShortestSequenceAsSeq(m: Machine, combinations: Iterator[Set[Int]]): (Int, Set[Int]) = 
//   var haveFoundOne = false
//   var minPresses = Int.MaxValue;
//   var minComb = Set[Int]()

//   var highestSlotCount = 0

//   while (!haveFoundOne) do
//     var combination = combinations.next();

//     if (testSequence(m, combination)) then
//       minPresses = combination.size
//       minComb = combination
//       haveFoundOne = true
        
        


    
    
//   (minPresses, minComb)


// def findShortestSequence(m: Machine, combinations: Iterator[Set[Int]]): Int = 
//   var haveFoundOne = false
//   var minPresses = 0;

//   while (!haveFoundOne) do
//     var combination = combinations.next();

//     if (testSequence(m, combination)) then
//       minPresses = combination.size
//       haveFoundOne = true
    
//   minPresses




// def testSequence(m: Machine, buttonPresses: Set[Int]): Boolean =
//   var newLights = m.lights.clone();

//   for (i <- buttonPresses.iterator) do
//     var button = m.buttons(i)
//     for (j <- button) do
//       var light = newLights(j)
//       if (light == 0) then
//         newLights(j) = 1
//       else
//         newLights(j) = 0

//   newLights.map(x => if (x == 0) then true else false).reduce((x,y) => x && y)


// def getPowerSet(numberOfButtons: Int): Iterator[Set[Int]] = 

//   var s = Set[Int]()

//   for (i <- 0 until numberOfButtons) do
//     s = s + i;
  
//   s.subsets()
    

 
  

// class Machine(var lights: ArrayBuffer[Int], var buttons: ArrayBuffer[ArrayBuffer[Int]], var joltages: ArrayBuffer[Int]):
//   override def toString(): String =
//     "Lights: " +lights.toString() + " Buttons: " + buttons.toString() + " Joltages: " + joltages.toString();

//   def changeLightsToJoltageOdds(): Unit =
//     for (i <- 0 until lights.length) do
//       if (joltages(i) % 2 == 0) then
//         lights(i) = 0
//       else
//         lights(i) = 1

//   def reduceJoltagesByLights(): Unit = 
//     for (i <- 0 until lights.length) do
//       if (lights(i) != 0) then
//         joltages(i) -= 1;

//   def reduceJoltagesByButtons(set: Set[Int]): Unit = 

//     for (i <- set) do
//       var but = buttons(i)
//       for (j <- but) do
//         joltages(j) -= 1

  
//   def halfJoltages(): Unit =
//     for (i <- 0 until lights.length) do
//       joltages(i) /= 2;


// def loadValues(filename: String): ArrayBuffer[Machine] = 
//   var values = ArrayBuffer[Machine]()
//   var lines =  Source.fromResource(filename).getLines()

//   for (line <- lines) do
//     var split = line.split(" ")
    
//     var lightsString = split(0)

//     var buttonsStrings = split.slice(1, split.length-1)

//     var joltagesString = split(split.length-1)

//     var lights = lightsString.slice(1,lightsString.length()-1).map((x => if (x == '.') then 0 else 1)).to(ArrayBuffer)

//     var buttons = ArrayBuffer[ArrayBuffer[Int]]();

//     for (bs<- buttonsStrings) do
//       var button = bs.slice(1,bs.length()-1).split(",").map(x=> x.toInt).to(ArrayBuffer)
//       buttons.addOne(button)

//     var joltages = joltagesString.slice(1, joltagesString.length()-1).split(",").map(x=>x.toInt).to(ArrayBuffer)


//     var m = Machine(lights, buttons, joltages)

//     values.addOne(m);



//   values
