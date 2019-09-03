package basics
import swiftvis2.plotting._
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.SwingRenderer
import swiftvis2.plotting.ColorGradient

case class TempRow(day: Int, doy: Int, month: Int, year: Int, precip: Double,
    tave: Double, tmax: Double, tmin: Double)

object SATemps {

    def parseLine(line: String): TempRow = {
        val p = line.split(",")
        TempRow(p(0).toInt, p(1).toInt, p(2).toInt, p(4).toInt, p(5).toDouble, p(6).toDouble, p(7).toDouble, p(8).toDouble)
    }
    
    def main(args: Array[String]): Unit = {
        val source = scala.io.Source.fromFile("/users/mlewis/CSCI3395-F19/InClassBD/data/SanAntonioTemps.csv")
        val lines = source.getLines()
        val data = lines.drop(2).map(parseLine).toArray
        // data.take(5).foreach(println)

        println("Day with highest high temperature")
        val hotDay1 = data.maxBy(_.tmax)
        val hotDay2 = data.reduce((d1, d2) => if(d1.tmax > d2.tmax) d1 else d2)
        println(hotDay1)
        println(hotDay2)

        print("Day with most precipitation")
        val precipDay = data.maxBy(_.precip)
        println(precipDay)

        println("Fraction of days with more than 1 inch of precipitation")
        val precipDays = data.filter(_.precip >= 1.0)
        val fracPrecipDays = precipDays.length.toFloat / data.length.toFloat
        val (rainySum, rainyCount) = data.foldLeft((0.0, 0)){ case ((sum, cnt), day) => 
            if (day.precip >= 1) (sum + day.precip, cnt + 1) else (sum, cnt)
        }
        println(fracPrecipDays)
        println(rainySum/rainyCount)

        println("Average high temperature for rainy days")
        val avgHigh = precipDays.foldLeft(0.0)(_ + _.tmax) / precipDays.length.toFloat
        println(avgHigh)

        println("Average high temperature by month")
        val months = data.groupBy(_.month).toSeq.sortBy(_._1)
        val avgHighMonths = months.map{
            t => t._2.foldLeft(0.0)(_ + _.tmax) / t._2.length.toFloat
        }
        println(avgHighMonths)

        println("Average amount of precipitation by month")
        val avgPrecipMonths = months.map{
            t => t._2.foldLeft(0.0)(_ + _.precip) / t._2.length.toFloat
        }
        println(avgPrecipMonths)

        println("Median amount of precipitation by month")
        val medPrecipMonths = months.map(i => i._2.sortBy(_.precip)).map{
            i => if (i.length % 2 == 0)
                    i(i.length / 2).precip
                else
                    (i((i.length - 1)/ 2).precip + i(i.length / 2).precip).toFloat / 2.0
        }
        println(medPrecipMonths)

        val cg = ColorGradient(1946.0 -> RedARGB, 1975.0 -> BlueARGB, 2014.0 -> GreenARGB)
        val sizes = data.map(_.precip * 2 + 2)
        val tempByDayPlot = Plot.simple(
            ScatterStyle(data.map(_.doy), data.map(_.tave), symbolWidth = sizes, symbolHeight = sizes, colors = cg(data.map(_.year))),
            "SA Temps", "Day of Year", "Temp")
        SwingRenderer(tempByDayPlot, 800, 800, true)

    }
    
}

