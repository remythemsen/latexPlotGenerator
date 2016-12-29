import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Speed extends App {

  // Read in results
  //val filePath = "data/test_results_hyperplane.log"
  val filePath = "data/test_results_crosspolytope_for_plots.log"
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
    bw.write(formatPlot(buckets(b), b._1, b._2, {
      b._1 match {
          // Xpoly
        case 1 => "*"
        case 2 => "10-pointed star"
        case 3 => "oplus*"
          // Hyper
        case 10 => "*"
        case 11 => "10-pointed star"
        case 14 => "oplus*"
        case 15 => "square*"
        case 16 => "triangle*"
        case 17 => "diamond*"
        case 18 => "pentagon*"
        case 19 => "star"
      }
    }, {
      b._2 match {
        case 2 => "green"
        case 3 => "black"
        case _ => "orange"
      }
    }, "dashed"))
    i+=1
  }

  bw.close()

  println("Finished!")

  def formatPlot(points:ArrayBuffer[(Int, Double, Double)], m:Int, L:Int, mark:String, color:String, linesStyle:String): String = {
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
      } +", "+p._3*10+")")
    }
    sb.append("\n\t};")
    sb.append("\n\t\\addlegendentry{M = "+m+", L = "+L+"}\n\n")

    sb.toString
  }

}

