val a = List(1,2,3)
val b = List(9,8,7)
val x = 1 to 10
val y = 1


a :: b
val d = a ::: b
10 :: a

val c = (a ++ b)
c.map(_*2)
//d.flatMap(_*2)

val fruit1 = Seq("apple", "banana", "orange")
val fruit2 = Seq("mango", "grapes")
val fruits = List(List("apple", "banana", "orange"), List("mango", "grapes"))
//fruits.map(s => s.toUpperCase)
fruits.flatMap(s => s.toUpperCase)

//c.flatMap(i => i+1)
//
//val z = List('a','b','c')
//
//z.flatMap{c => c}

def toInt(s: String): Option[Int] = {
  try {
    Some(Integer.parseInt(s.trim))
  } catch {
    // catch Exception to catch null 's'
    case e: Exception => None
  }
}
val strings = Seq("1", "2", "foo", "3", "bar")
strings.map(toInt)
strings.flatMap(toInt)