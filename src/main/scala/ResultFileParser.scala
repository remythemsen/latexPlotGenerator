import scala.io.Source

/**
  * Created by remeeh on 12/19/16.
  */
class ResultFileParser(filepath:String) {
  val file:Iterator[String] = Source.fromFile(filepath).getLines

  def hasNext:Boolean = file.hasNext
  def next:ResultSet = {
    val res = file.next.toString.split(" ")
    ResultSet(
      res(0).toInt, // n
      res(1).toInt, // m
      res(3).toInt, // L
      res(6).toDouble, // recall
      res(14).toDouble // avg Time
    )
  }
}
