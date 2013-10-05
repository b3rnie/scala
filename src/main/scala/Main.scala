import scala.util.parsing.combinator._

object Main{
  def types(){
    val int    = 123
    val char   = 'x'
    val string = "str"
    val symbol = 'foo
    val tuple  = ('foo, 'bar)
    val list   = List("a", 'b, 2)
    val map    = Map("a" -> 1, "b" -> 2)
    val set    = Set(1, 2, 3)
  }
  def funs(){
    val fun = (x:Int) => x*2
    println(fun(10))
    println(funs2(fun))
    def fun2(l:List[Any]) = {
      println(l)
    }
    fun2(List(1, 2, "abc"))
  }
  def funs2(fun:(Int) => Int) : Int = {
    fun(10)
  }
  def classes{
    val x = new class1("param")
    println(x)
    println(x.x)
    println(x.z)
  }
  class class1(xx:String){
    val x = xx
    lazy val z = {println("init z") ; 1}
    override def toString():String = "blah"
  }
  def fold{
    (1 to 10).foldLeft(0)((a:Int, acc:Int) =>
      {println(a)
       acc
     })
  }
  def loops{
    val l = List('a, 'b, 'c)
    var i=0
    for(i<-0 until l.length){
      println(l(i))
    }
    for(x<-l){}
    l.reverse.foreach((x) => println(x))
  }
  def matching{
    'x match{
      case 'x => println("dsfdsf")
      case _  => println("other")
    }
    val x : Any = "123"
    x match{
      case _: String => println("string")
      case _: Symbol => println("symbol")
    }
    val y = List(1, 2, 3, 4, 5)
  }
  def exceptions{
    try{
      throw new Exception("oh no")
    }catch{
      case e:Exception => println("got exception")
    }
  }
  def oo{
    val x = new oo1(1111)
    val y = new oo1(2222)
    x and y
  }
  class oo1(vv:Int){
    val v = vv
    def and(other: oo1):oo1 = {
      println(other.v)
      other
    }
  }
  def dsl(){
    println
    val p = new ParserCombinators
    val s = """
      [rulename1] foo, bar
      [rulename2]
    """
    println(s)
    p.parseAll(p.rules, s)
      match{
        case p.Success(r,_) => println(r.toString)
        case x => println(x.toString)
      }
  }
  def main(args: Array[String]){
    types
    funs
    classes
    fold
    loops
    matching
    exceptions
    oo
    dsl
  }
}

class ParserCombinators extends RegexParsers{
  def rules : Parser[Any] = rep(rule)
  def rule  : Parser[Any] = "[" ~> ruleName <~ "]" ~ AList
  def AList  : Parser[Any] = repsep(member, ",")
  def member   = """(\w+)""".r
  def ruleName = """(\w+)""".r
}

