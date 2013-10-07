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
      [RULENAME1]
        on_entry: e1, e2
        on_exit:  e3, e4
        out1 -> RULENAME_BLAH
        out2 -> RULENAME_XXX
      [RULENAME2]
        on_exit: e1,e2
        on_entry: x,y
    """
    println(s)
    p.parseAll(p.rules, s)
      match{
        case p.Success(r,_) =>
        println(r.toString())
//r.foreach((e) => {
        //    println(e)
        //  })
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
  def rules:Parser[List[Rule]] = rep(rule)
  def rule                     = rule_name ~
                                 rep(rule_entry_exit) ~
                                 rep(rule_transition) ^^
    {case rulename~entry_exit~transitions =>{
      println(rulename)
      println(entry_exit)
      println(transitions)
      println("match!")
        //(rulename, entry_exit, transitions)
        Map('rulename -> rulename,
            'transitions -> transitions)
      var OnEntry = List[String]()
      var OnExit  = List[String]()
      entry_exit.foreach((x) =>{
        x match{
          case ("on_entry",l) => OnEntry = OnEntry ::: l
          case ("on_exit", l) => OnExit  = OnExit ::: l
          case e              => throw new Exception(e.toString)
        }
      })
      println(OnEntry)
      println(OnExit)
      new Rule(rulename, entry_exit, transitions)
    }
    }
  def rule_name       = "[" ~> rulename <~ "]"
  def rule_entry_exit = ("on_entry" | "on_exit") ~ ":" ~
                        repsep(name, ",") ^^ {
                          case what~":"~list => (what, list)
                        }
  def rule_transition = name ~ "->" ~ rulename ^^
      {case name~"->"~rulename => (name,rulename)}

  def name     = """[a-z][a-z0-9_]*""".r
  def rulename = """[A-Z][A-Z0-9_]*""".r
}


class Rule(r:String, ee:Any, t:List[Any]){
  val rulename    = r
  val entry_exit  = ee
  val transitions = t
}

