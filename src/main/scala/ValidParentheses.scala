object ValidParentheses extends App {

  def validParentheses(n:Int):List[String] = {

    def validParenthesesRec(itr:Int,open:Int,close:Int,str:String):List[String] = itr match {

      case itr if itr == 2*n && open == close => {
        println(str)
        List(str)
      }

      case itr if itr == 2*n => List()

      case itr => {
        var ans = List()

        if(close < open){
          ans ++ validParenthesesRec(itr+1,open,close+1,str+")")
        }

        if(open < n){
          ans ++ validParenthesesRec(itr+1,open+1,close,str+"(")
        }

        ans
      }

    }

    validParenthesesRec(0,0,0,"")

  }

  println(validParentheses(3))
}
