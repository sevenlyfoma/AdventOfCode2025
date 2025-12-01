import scala.io.Source
import scala.math


@main def main(): Unit =
  star_one()
  star_two()

def star_two(): Unit = 
  val lines = Source.fromFile("src/main/resources/input.txt").getLines();
  // val lines = Source.fromFile("src/main/resources/test1.txt").getLines();

  var position = 50;
  var virtualPos = 0;
  var zeroCount = 0;
  var clicksAdd = 0;

  for line <- lines do
    //Get left or right
    var lr = line.charAt(0);
    //Assume nice input
    var num = (line.substring(1)).toInt

    clicksAdd = 0;


    if (lr == 'L')

      //Get "virtual position" which represents where we are in a 'non circular calculation'
      virtualPos = position - num;
      //If virtual position is less than or equal to zero, then we have gone past the zero mark at elast ones
      if (virtualPos <= 0)
        
        //Number of times we have gone past zero is at least 1 + the number of times -100 can be divided into the virtual position
        clicksAdd = 1 + virtualPos/(-100)
        //Need special case for if starting position is 0 as otherwise it gets double counted
        if (position == 0)
          clicksAdd -= 1;
      // Calculate new position like in star one
      position = (100 + ((position - num) % 100)) % 100
    else 
      virtualPos = position + num;
      //Number of clicks is how many hundreds digits we have
      clicksAdd = virtualPos/100;

      position = (position + num) % 100

    zeroCount += clicksAdd;

    // println(position);
    // println(zeroCount);
    // println();


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

      // First get the virtual position and modulo it by 100 to see how far into the engative we are
      // Then add it to 100, if positive, say 49, we get 149 which will mod 100 to 49
      // if negative, say -18, we get 82 which mod 100 = 82
      position = (100 + ((position - num) % 100)) % 100
    else 
      position = (position + num) % 100

    if (position == 0)
      zeroCount += 1;

    // println(position);
    // println(zeroCount);
    // println();


  println(zeroCount)
 
