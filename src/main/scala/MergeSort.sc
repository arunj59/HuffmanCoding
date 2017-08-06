//import patmat.Huffman.Leaf
//val a1 = 'A'

val a2 = List('G','B','M','G','G','G','B','A')
a2.length
a2:::a2
//val a3 = List((1,1),(2,2),(3,3))
//
//List((null,null)) :: List("")
//
//val a4 = a3 ::: (4,4) :: Nil

//a2.head
//
//val s1 = List(10,-2,3)
//val s2 = List(-1,2,30)
//
//val s3 = 1::2::3::Nil
//
//s2.sortBy(i=>i)

//a2.sortBy(c => c)


def times(chars: List[Char]): List[(Char, Int)] = {

  def timesCalc(srtList: List[Char],l :List[(Char, Int)], ch : Char, count : Int ): List[(Char, Int)] = {
//    println(srtList + " : " + ch + " : " + count + " : " + l)
    if (srtList.isEmpty) l ::: (ch, count) :: Nil
    else if (ch != srtList.head) timesCalc(srtList.tail, l ::: (ch, count) :: Nil, srtList.head, 1)
    else timesCalc(srtList.tail, l , srtList.head, count+1)
  }

  if (chars.isEmpty) throw new NoSuchElementException("Empty List")
  else  {
    val srtList = chars.sortBy(c => c)
    timesCalc(srtList.tail,Nil,srtList.head,1)
  }
}

val x1 = times(a2)
val x2 = x1.sortBy(t => t._2)


//def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
//  def makeLeafList(freqs: List[(Char, Int)], ll : List[Leaf]):List[Leaf] ={
//    if (freqs.isEmpty) ll
//    else makeLeafList(freqs.tail,ll ::: Leaf(freqs.head._1, freqs.head._2) :: Nil)
//  }
//
//  if (freqs.isEmpty) throw new NoSuchElementException("Empty List")
//  else  {
//    val srtList = freqs.sortBy(t => t._2)
//    makeLeafList(freqs,Nil)
//  }
//}
//
//val x3 = makeOrderedLeafList(x1)
