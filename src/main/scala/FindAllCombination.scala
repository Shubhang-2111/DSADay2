object FindAllCombination extends App{

  val lst  =  Set(1,2,3,4,5)
  val targetSum = 8

  def findAllCombination(lst:List[Int],targetSum:Int):List[Set[Int]] = {

    if(lst.isEmpty){
      return List(Set())
    }

    def findAllCombinationRec(index:Int,sum:Int,currLst:List[Int]):List[Set[Int]] = (index,sum) match {

      case (i,0) => List(currLst.toSet)

      case (i,s) if s < 0 || i >= lst.size => List()

      case (i,s) => {
        val take = findAllCombinationRec(i+1,sum-lst(i),currLst:+lst(i))
        val notTake = findAllCombinationRec(i+1,sum,currLst)

        take ++ notTake
      }
    }

    findAllCombinationRec(0,targetSum,List())
  }

  println(findAllCombination(lst.toList,targetSum))
}
