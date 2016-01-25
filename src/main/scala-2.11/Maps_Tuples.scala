import scala.collection.mutable
import scala.io.Source

/**
 * Created by Barn on 24/01/2016.
 */
object Maps_Tuples extends App {

  /**
   * 1
   */
  val covet: Map[String, Double] = Map(
    "Barbour coat" -> 150,
    "Astorflex Shoes" -> 100,
    "Holiday" -> 500,
    "Climbing pass" -> 300
  )

  for((key, value) <- covet) println(key + "   " + value)

  def reduce10(x: Double) = x * 0.9
  val reduced = covet.map(j => (j._1, reduce10(j._2)))
  println("Map Reduced by 10%: " + reduced)

  val newMap = covet map {
    case (k, v) => (k, v * 0.9)
  }
  println(newMap)

  def readInFile: Array[String] = {
    val source = Source.fromFile("/Users/Barn/Desktop/ioFile.txt", "UTF-8") //Don't have to include encoding type
    val tokens = source.mkString.split("\\s+") //Split the text file at each space OR many spaces.
    tokens
  }

  /**
   * 2
   */
  def scalaReadFile = {
    val tokens = readInFile
    val frequency = scala.collection.mutable.Map[String, Int]()

    //For each word in the file token.
    //check to frequency Map for current token - if exists +1 to the value Else return 0
    for(t <- tokens) frequency(t) = frequency.getOrElse(t, 0) + 1

    val padding = frequency.keysIterator.map(_.length) max

    for((key, value) <- frequency) printf("Key: %-" + padding +"s|   Value: %s\n", key, value )
  }
  scalaReadFile

  /**
   * 3
   */
  def countWordsImmutable = {
    val tokens = readInFile
    var wordCount = Map[String, Int]()
    //+ adds a new key value pair to a new map. It returns a new map
    tokens foreach { token =>
     wordCount +=  token ->  (wordCount.getOrElse(token, 0) + 1)
    }
  }
  countWordsImmutable

  /**
   * 4
   */
  def countWordsSorted = {
    val tokens = readInFile
    var count = scala.collection.immutable.SortedMap[String, Int]()
    tokens.foreach { t =>
      count += t -> (count.getOrElse(t, 0) + 1)
    }
//    for((key, value) <- count) println("Key: "+key + "  Value: " + value)
  }
  countWordsSorted

  /**
   * 6
   */
  import java.util.Calendar._
  val linkedHashMap = mutable.LinkedHashMap[String, Int]()

  linkedHashMap += ("Monday" -> MONDAY)
  linkedHashMap += ("Tuesday" -> TUESDAY)
  linkedHashMap += ("Wednesday" -> WEDNESDAY)
  linkedHashMap += ("Thursday" -> THURSDAY)

  for ((key, value) <- linkedHashMap)  printf("%s=%d\n", key, value)

  /**
   * 7
   */
  import scala.collection.JavaConversions.propertiesAsScalaMap
  val props: scala.collection.Map[String, String] = System.getProperties()

  /**
   * 2 good ways to find the largest key length in a map
   */
  val maxxy = props.keySet.map(_.length).max
  val maxReduce = props.keysIterator.reduceLeft((x,y) => if (x.length > y.length) x else y).length

  for((k, v) <- props) printf("%-" + maxxy + "s   |   %s\n", k, v)


  /**
   * 8
   */
  def minmax(values: Array[Int]): Tuple2[Int, Int] = {
    (values.min, values.max)
  }

  /**
   * 9
   */
  def lteqgt(values: Array[Int], v: Int): Tuple3[Int, Int, Int] = {
    val lessthan = values.filter(_ < v).length
    val equal = values.count(_ == v)
    val greater = values.count(_ > v)
    (lessthan, equal, greater)
  }

  val ar = Array(10,20,30,40,50, 60, 70, 80,90)
  println(lteqgt(ar, 50))

  val h ="hello"
  val e = "world"

  println(h.zip(e))

}