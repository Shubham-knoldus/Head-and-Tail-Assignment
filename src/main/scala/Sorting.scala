import scala.annotation.tailrec

class Sorting {

  @tailrec
   final def insert (element : Int , myList : List[Int], accumulator : List[Int] = Nil ) : List[Int] =

    if ( myList.nonEmpty ) {
      val x1 :: xs1 = myList

      if ( element <= x1 ) accumulator.reverse :::( element )::x1::xs1
      else insert ( element,xs1,x1:: accumulator )
    }

    else ( element::accumulator ).reverse

  def isort ( xs: List[Int] ): List[Int] =
    if ( xs.isEmpty ) Nil
    else insert( xs.head, isort(xs.tail) )
}

object main extends App {
  val FirstList: List[Int] = List(9,3,4,8,7,2,6,1,5)
  val SecList: List[Int] = List(0,1,2,3,1,4,55,0)
  val unsortList = new Sorting
  println(unsortList.isort(FirstList))
  println(unsortList.isort(SecList))

}


