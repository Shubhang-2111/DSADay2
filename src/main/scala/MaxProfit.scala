import scala.annotation.tailrec

object MaxProfit extends App {

  def maxProfit(prices:Array[Int]):Int = {

    @tailrec
    def maxProfitRec(index:Int, lastBuy:Int, profit:Int):Int = index match {

      case i if i>=prices.length => profit

      case i => {
        maxProfitRec(i+1,Math.min(lastBuy,prices(i)), Math.max(profit,prices(i)-lastBuy))
      }
    }

    maxProfitRec(0,Int.MaxValue,0)
  }

  val prices = Array(7,1,5,3,6,4)
  println(maxProfit(prices))
}
