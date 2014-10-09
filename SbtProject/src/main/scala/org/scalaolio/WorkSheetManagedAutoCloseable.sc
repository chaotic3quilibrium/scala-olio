import _root_.java.io._

import org.scalaolio.java.io.File_
val a = 1
val DEFAULT_BUFFER_SIZE = 65536
//
val fileIn = new File("""C:\Users\Jim\Desktop\folder placeholder(for git).txt""")
val fileOut = new File("""C:\Users\Jim\Desktop\folder placeholder(for git).txtout""")
val result3 = File_.pullString(fileIn)
val result4 = result3.get
val result5 = File_.pushString(fileOut, "testing to see if this go into the file\n2nd line")
