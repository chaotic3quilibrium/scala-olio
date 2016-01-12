import org.scalaolio.util.csv.{Composer,Parser}

val testRowsHardcoded: List[String] = {
  val superTrickyTestCase = {
    val dqx1 = '"'
    val dqx2 = dqx1.toString + dqx1.toString
    s"${dqx1}${dqx2}a${dqx2} , ${dqx2}1${dqx1} , ${dqx1}${dqx2}b${dqx2} , ${dqx2}2${dqx1} , ${dqx1}${dqx2}c${dqx2} , ${dqx2}3${dqx1}"
  }
  val nonTrickyTestCases =
"""
,,
a,b,c
a,,b,,c
 a, b, c
a ,b ,c
 a , b , c
"a,1","b,2","c,2"
"a"",""1","b"",""2","c"",""2"
 "a"" , ""1" , "b"" , ""2" , "c"",""2"
""".split("\n").tail.toList
 (superTrickyTestCase :: nonTrickyTestCases.reverse).reverse
}
val parsedLines =
  Parser.fromLines(testRowsHardcoded)
parsedLines.map(_.mkString("|")).mkString("\n")
val composedLines =
  Composer.toLines(parsedLines)
composedLines.mkString("\n")
val parsedLines2 =
  Parser.fromLines(composedLines)
parsedLines == parsedLines2
