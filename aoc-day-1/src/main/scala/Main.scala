import scala.io.Source
import scala.math


@main def main(): Unit =
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
      position = ((100 + position) - num) % 100
    else 
      position = (position + num) % 100

    if (position == 0)
      zeroCount += 1;


  println(zeroCount)

 



// L towards lower numbers R towards higher numbers 0-99 distance value 5 L10 -> 95 11 R8 -> 19
// The dial starts at 50
// Count the number of times the dial points at 0