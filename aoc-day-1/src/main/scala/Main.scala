import scala.io.Source
import scala.math


@main def main(): Unit =
  star_one()
  // star_two()

def star_two(): Unit = 
  //val lines = Source.fromFile("src/main/resources/input.txt").getLines();
  val lines = Source.fromFile("src/main/resources/test1.txt").getLines();

  var position = 50;
  var virualPos = 0;
  var zeroCount = 0;
  var clicksAdd = 0;

  for line <- lines do
    //Get left or right
    var lr = line.charAt(0);
    //Assume nice input
    var num = (line.substring(1)).toInt


    if (lr == 'L')
      //need to add 100 first so that wrap around works
      
      position = (100 + ((position - num) % 100)) % 100
    else 
      virualPos = position + num;
      clicksAdd = virualPos/100;
      position = (position + num) % 100

    if (position == 0)
      zeroCount += clicksAdd;


  println(zeroCount)


  ()

def star_one(): Unit = 
  val lines = Source.fromFile("src/main/resources/input.txt").getLines();
  // val lines = Source.fromFile("src/main/resources/test1.txt").getLines();

  var position = 50;
  var zeroCount = 0;


  for line <- lines do
    //Get left or right
    var lr = line.charAt(0);
    //Assume nice input
    var num = (line.substring(1)).toInt


    if (lr == 'L')
      //need to add 100 first so that wrap around works
      // position = ((100 + position) - num) % 100

      position = (100 + ((position - num) % 100)) % 100
    else 
      position = (position + num) % 100

    if (position == 0)
      zeroCount += 1;

    // println(position);
    // println(zeroCount);
    // println();


  println(zeroCount)
 



// L towards lower numbers R towards higher numbers 0-99 distance value 5 L10 -> 95 11 R8 -> 19
// The dial starts at 50
// Count the number of times the dial points at 0