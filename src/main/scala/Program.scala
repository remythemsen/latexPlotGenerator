import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Program extends App {
  val marks = Array('x', '*', '+', 'o')
  val colors = Array("red", "gray", "purple")
  val lineTypes = Array("dashed")

  // variations:
  val styles = for {
    m <- marks
    c <- colors
    lt <- lineTypes
  } yield (m, c, lt)


  // Read in results
  val filePath = "data/result"
  val outDir = "data"
  val parser = new ResultFileParser(filePath)

  // Generate buckets (each representing a line on the graph an L - M config)
  val buckets = new mutable.HashMap[(Int, Int), ArrayBuffer[(Int, Double, Double)]]()

  while(parser.hasNext) {
    val result = parser.next
    // Get 'key'
    val key = (result.functions, result.tables)

    // throw the data tuple into a bucket
    if(buckets.contains(key))
      buckets(key) += Tuple3(result.dataSetSize, result.recall, result.speed)
    else
      buckets += ((key, ArrayBuffer(Tuple3(result.dataSetSize, result.recall, result.speed))))
  }

  // Get printer
  val outFile = new File(outDir+"/formattedResult.data")
  val bw = new BufferedWriter(new FileWriter(outFile))

  // Format and print each bucket
  var i = 0
  for(b <- buckets.keysIterator) {
    bw.write(formatPlot(buckets(b), b._1, b._2, styles(i)._1, styles(i)._2, styles(i)._3))
    i+=1
  }

  bw.close()

  println("Finished!")

  def formatPlot(points:ArrayBuffer[(Int, Double, Double)], m:Int, L:Int, mark:Char, color:String, linesStyle:String): String = {
    /*

    \addplot[ mark = x, dashed, red ]
        coordinates {
          (1, 8.2)(2, 8.27)(3, 8.37)
        };
        \addlegendentry{M = 10, L = 2}

     */
    val sb = new StringBuilder
    sb.append("\\addplot[ mark = "+mark+", "+linesStyle+", "+color+" ]")
    sb.append("\n\tcoordinates {")
    sb.append("\n\t\t") // indenting cordinates
    for(p <- points) {
      sb.append("("+{
        if(p._1 > 0 && p._1 < 40000) {
          1
        } else if (p._1 > 40000 && p._1 < 90000) {
          2
        } else if (p._1 > 90000 && p._1 < 450000) {
          3
        } else if (p._1 > 450000 && p._1 < 800000) {
          4
        } else if (p._1 > 800000) {
          5
        }
      } +", "+p._2/10+")")
    }
    sb.append("\n\t};")
    sb.append("\n\t\\addlegendentry{M = "+m+", L = "+L+"}\n\n")

    sb.toString
  }

}

