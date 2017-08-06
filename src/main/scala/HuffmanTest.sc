

/**
  * Assignment 4: Huffman coding
  *
  */
object Huffman {

  /**
    * A huffman code is represented by a binary tree.
    *
    * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
    * The weight of a `Leaf` is the frequency of appearance of the character.
    *
    * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
    * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
    * leaves.
    */
  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree


  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, weight) => weight
    case Leaf(_, weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, chars, _) => chars
    case Leaf(char, _) => List(char)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  // Part 2: Generating Huffman trees

  /**
    * In this assignment, we are working with lists of characters. This function allows
    * you to easily create a character list from a given string.
    */
  def string2Chars(str: String): List[Char] = str.toList


  def times(chars: List[Char]): List[(Char, Int)] = {

    def timesCalc(srtList: List[Char],l :List[(Char, Int)], ch : Char, count : Int ): List[(Char, Int)] = {
      //        println(srtList + " : " + ch + " : " + count + " : " + l)
      if (srtList.isEmpty) l ::: (ch, count) :: Nil
      else if (ch != srtList.head) timesCalc(srtList.tail, l ::: (ch, count) :: Nil, srtList.head, 1)
      else timesCalc(srtList.tail, l , srtList.head, count+1)
    }

      val srtList = chars.sortBy(c => c)
      timesCalc(srtList.tail,Nil,srtList.head,1)
 }

  def singleton(trees: List[CodeTree]): Boolean = {
    if(trees.length == 1) true
    else false
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {

    def makeLeafList(freqs: List[(Char, Int)], ll : List[Leaf]):List[Leaf] ={
      if (freqs.isEmpty) ll
      else makeLeafList(freqs.tail,ll ::: Leaf(freqs.head._1, freqs.head._2)::Nil)
    }

      val srtList = freqs.sortBy(t => t._2)
      makeLeafList(srtList,Nil)
  }


  def combine(trees: List[CodeTree]): List[CodeTree] =  trees match {
    case Nil | _ :: Nil => trees
    case x :: y :: ts   => (makeCodeTree(x, y) :: ts).sortWith(weight(_) < weight(_))
  }

  def until(s: List[CodeTree] => Boolean, c: List[CodeTree] => List[CodeTree])(t: List[CodeTree]):List[CodeTree] = {
    if (s(t)) t
    else until(s,c)(c(t))
  }


  def createCodeTree(chars: List[Char]): CodeTree = {
    if (chars.isEmpty) throw new NoSuchElementException("Empty String")
    else {
          until(singleton,combine)(makeOrderedLeafList(times(chars))).head
     }
  }



  type Bit = Int

//  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
//    def charDecode(t: CodeTree, bits: List[Bit]): List[Char] = t match {
//      case Leaf(c, w) => if (bits.isEmpty) List(c) else c :: charDecode(tree, bits)
//      case Fork(l, r, c, w) => if (bits.head == 0) charDecode(l, bits.tail) else charDecode(r, bits.tail)
//    }
//    charDecode(tree,bits)
//  }

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def traverse(t: CodeTree, b: List[Bit]): List[Char] = t match {
      case Leaf(ch, _) => if (b.isEmpty) List(ch) else ch :: traverse(tree, b)
      case Fork(l, r, _, _) => if (b.head == 0) traverse(l, b.tail) else traverse(r, b.tail)
    }
    traverse(tree, bits)
  }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  def decodedSecret: List[Char] = decode(frenchCode,secret)

  decodedSecret


  val t1 = Leaf('Z',10)
  val t2 = Leaf('A',21)
  val t3 = makeCodeTree(t1,t2)
   t3.chars
  t3.weight

  val t4 = times(string2Chars("bggsegwesgdgfbvcbaaaaa"))
  val t5 = makeOrderedLeafList(t4)
  val t6 = times(string2Chars("m"))
  val t7 = makeOrderedLeafList(t6)
  singleton(t5)
  singleton(t7)

  combine(t5)

  val t8 = (until(singleton,combine)(t5)).head




}