import scala.io.Source


@main def main(): Unit =
  // val lines = Source.fromFile("src/main/resources/input.txt").getLines();
  val lines = Source.fromFile("src/main/resources/test1.txt").getLines();

  val position = 50;
  val zeroCount = 0;


  for line <- lines do
    //Get left or right
    val lr = line.charAt(0);
    //Assume nice input
    val num = (line.substring(1)).toInt


    if (lr == 'L')
      ()
    else 
      ()

    println(position)
    println(zeroCount)
    println()

 



// L towards lower numbers R towards higher numbers 0-99 distance value 5 L10 -> 95 11 R8 -> 19
// The dial starts at 50
// Count the number of times the dial points at 0