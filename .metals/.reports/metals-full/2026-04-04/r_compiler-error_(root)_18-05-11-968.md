error id: C44EFC1AA137FCFA2C07F44CD1E7B5DF
file://<WORKSPACE>/src/main/scala/example/MainProject.scala
### java.lang.AssertionError: assertion failed

occurred in the presentation compiler.



action parameters:
uri: file://<WORKSPACE>/src/main/scala/example/MainProject.scala
text:
```scala
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.io.StdIn._
import scala.collection.mutable
import scala.util.control.Breaks._
import Console._
import javax.crypto.KeyGenerator

// Unambiguous grammar
// S -> E$
// E -> T + E | T
// T -> Const | Var

// Easier to parse grammar
// S -> E$
// E -> Terminal E2
// E2 -> + E
// E2 -> NIL
// Terminal -> Const
// Terminal -> Var

abstract class S {
  def eval(env: Main.Environment): Int
}
sealed abstract class SetExpr
case class SetLiterals(elems: Set[Int]) extends SetExpr
case class SetRange(lo: Int, hi: Int) extends SetExpr
case class SetComp(output: E, clauses: List[Clause]) extends SetExpr

sealed abstract class Clause
case class Generator(identifier: String, source: SetExpr) extends Clause
case class Guard(l: E, op: CompareOp, r: E) extends Clause


enum CompareOp:
  case Eq, NEq, Lt, Gt

def evalSet(s: SetExpr, env: Main.Environment): Set[Int] = {
  s match {

    case SetLiterals(v) => v

    case SetRange(lo, hi) =>
      val set = scala.collection.mutable.Set[Int]()
      for (i <- lo to hi) {
        set += i
      }
      set.toSet

    case SetComp(output, clauses) =>
    var envs = ListBuffer(env)
    for (c <- clauses) {
      c match {
        case Generator(identifier, source) =>
          var newEnvs = ListBuffer[Main.Environment]()
          for (e <- envs; v <- evalSet(source, e)) {
            val newEnv: Main.Environment = 
              (x: String) =>
                  if (x == identifier) 
                  v 
                  else e(x)
            newEnvs += newEnv
          }
          envs = newEnvs
        case Guard(l, op, r) =>
          envs = envs.filter {e =>
            val left = l.eval(e)
            val right = r.eval(e)

            op match {
              case CompareOp.Eq  => left == right
              case CompareOp.NEq => left != right
              case CompareOp.Lt  => left < right
              case CompareOp.Gt  => left > right
            }
          } 
      }
    }
    val result = scala.collection.mutable.Set[Int]()
          for (e <- envs) {
            val value = output.eval(e)
            result += value
          }
          result.toSet
    } 
  
}
def symanticAnalyser (s : SetComp) : Unit = {
  val definedVars = scala.collection.mutable.Set[String]()
  for ( c <- s.clauses){
    c match {
      case Generator(identifier, source) => 
        if (identifier != null){
          definedVars += identifier
        }
        else 
          throw new Exception ("No Identifier Being defined")

      case Guard(l, op, r) => 
        check(l,definedVars)
        check(r,definedVars)
    }
    
  }
  check(s.output,definedVars)
}

def check (s : S , t : scala.collection.mutable.Set[String]) : Unit = {
  s match {
    case Var(n) => 
      if (!t.contains(n)){
        throw new Exception (s"Dont have $n")
      }
    case Const(n) => 
    
    case E(l, right) => 
      check(l,t)  
      right match {
        case Some(r) => check(r.l,t)
        case _ => 
      }

    case T(l, right) => 
      check(l,t) 
      right match {
        case Some(r) => check(r.l,t)
        case _ =>  
      }
      
  }
}



abstract class Terminal extends S
case class E(l: T, right: Option[E2]) extends S {
  def eval(env: Main.Environment): Int = {
    val a1: Int = l.eval(env)
    right match {
      case Some(r) => val a2 = r.eval(env)
        if (r.operator == '+') {
          a1 + a2
        } else {
          a1 - a2
        }
      case None => a1
    }
  }
}
case class T(l: S, right: Option[T2]) extends S {
  def eval(env: Main.Environment): Int = {
    val a1: Int = l.eval(env)
    right match {
      case Some(r) => 
        val a2 = r.eval(env)
        r.operator match {
          case '*' => a1 * a2
          case '%' => if (a2 == 0) 
            0 
            else a1 % a2 
          case _   => a1
        }

      case None => a1
    }
  }
}
case class T2(operator : Char , l : T) extends S{
  def eval(env: Main.Environment): Int = l.eval(env)
}
case class E2(operator :Char,l: E) extends S {
  def eval(env: Main.Environment): Int = l.eval(env)
}
case class Var(n: String) extends Terminal {
  def eval(env: Main.Environment): Int = env(n)
}
case class Const(v: Int) extends Terminal {
  def eval(env: Main.Environment): Int = v
}

class RecursiveDescent(input:String) {
  val constregex: Regex = "^[0-9]+".r
  val varregex: Regex = "^[A-Za-z]+".r

  var index = 0

  def skipWhitespace(): Unit = {
    while (index < input.length && input(index).isWhitespace) 
    index += 1
  }

  def parseS(): S = parseE()

  def parseSet(): SetExpr = {
    skipWhitespace()
    if (index < input.length && input(index) == '{'){
        index +=1
        var result = parseSetBody()
        skipWhitespace()
        if (index < input.length && input(index) == '}'){
            index +=1
        }  
        return result 
    }
    else throw new Exception("Not starting with {} ")
    
  }

  def parseSetBody(): SetExpr = {
    skipWhitespace()
    if (index < input.length && input(index) == '}') {
      return SetLiterals(Set.empty)
    }

    val startIndex = index
    val expr = parseE()
    skipWhitespace() 

    if (index < input.length && input(index) == '|') {
      index += 1
      skipWhitespace()
      val clauses = parseClauses()
      return SetComp(expr, clauses)
    }
    index = startIndex

    val first = parseF()
    val firstValue = first match {
      case Const(v) => v
      case _ => throw new Exception("Expected integer in set")
    }

    skipWhitespace()
    if (index + 4 < input.length && input.substring(index, index + 5) == ",...,") {
      index += 5
      val second = parseF()
      val secondValue = second match {
        case Const(v) => v
        case _ => throw new Exception("Expected integer in range")
      }
      return SetRange(firstValue, secondValue)
    }

    val set = scala.collection.mutable.Set[Int]()
    set.add(firstValue)

    skipWhitespace()
    while (index < input.length && input(index) == ',') {
      index += 1
      skipWhitespace()
      val next = parseF()
      val nextValue = next match {
        case Const(v) => v
        case _ => throw new Exception("Expected integer in set")
      }
      set.add(nextValue)
      skipWhitespace()
    }

    SetLiterals(set.toSet)
  }

  def parseClause(): Clause = {
    skipWhitespace()
    val startIndex = index
    val remaining = input.substring(index)
    val varMatch = varregex.findPrefixOf(remaining)
    var identifier = ""

    if (varMatch.isDefined) {
        identifier = varMatch.get
        index += identifier.length
    }
    skipWhitespace()
    if (index < input.length && input.substring(index).startsWith("in")){
        index += 2
        skipWhitespace()
        return Generator(identifier , parseSet())
    }
    else {
        index = startIndex
    }
    val left = parseE()
    skipWhitespace()

    val op =
    if (input.substring(index).startsWith("!=")) {
      index += 2
      CompareOp.NEq
    } else if (index < input.length && input(index) == '=') {
      index += 1 
      CompareOp.Eq
    } else if (index < input.length && input(index) == '<') {
      index += 1 
      CompareOp.Lt
    } else if (index < input.length && input(index) == '>') {
      index += 1 
      CompareOp.Gt
    } else {
      throw new Exception("Invalid comparison operator")
    }

    skipWhitespace()
    val right = parseE()
    Guard(left, op, right)
}

    def parseClauses() : List[Clause] = {
        var list = ListBuffer[Clause]()
        list += parseClause()
        skipWhitespace()
        while (index < input.length && input(index) == ',') {
          index += 1
          skipWhitespace()
          list += parseClause()
          skipWhitespace()
        }
        return list.toList
    }

  def parseE(): E = {
    skipWhitespace()
    E(parseT(), parseE2())
  }

  def parseE2(): Option[E2] = {
    skipWhitespace()
    if (index < input.length && input(index) == '+'){
      index+=1; 
      Some(E2('+',parseE()))
    }
    else if (index < input.length && input(index) == '-'){
      index+=1; 
      Some(E2('-',parseE()))
    }
    else None
  }

  def parseF(): S = {
    skipWhitespace()
    if (index < input.length && input(index) == '(') {
      index += 1
      val expr = parseE()
      skipWhitespace()
      index += 1
      expr
    } 
    else {
      val remaining = input.substring(index)

      val numMatch = constregex.findPrefixOf(remaining)
      if (numMatch.isDefined) {
        val numStr = numMatch.get
        index += numStr.length
        return Const(numStr.toInt)
      } 
      else {
        val varMatch = varregex.findPrefixOf(remaining)
        if (varMatch.isDefined) {
          val v = varMatch.get
          index += v.length
          return Var(v)
        } else {
          throw new Exception("Expected variable or number")
        }
      }
    }
  }

  def parseT(): T = {
    skipWhitespace()
    T(parseF(),parseT2())
  }

  def parseT2() : Option[T2]= {
    skipWhitespace()
    if (index < input.length && input(index) == '*'){
      index+=1;
      Some(T2('*',parseT()))
    }
    else if (index < input.length && input(index) == '%'){
      index+=1; 
      Some(T2('%',parseT()))
    }
    else None
  }

  def parseTerminal(): Terminal = {
    val currStr = input.substring(index)

    // Get either the const or var which is there.
    val consts = constregex.findAllIn(currStr)
    if (consts.hasNext){
      val const: String = consts.next()
      index += const.length()
      Const(const.toInt)
    }
    else {
      val vars = varregex.findAllIn(currStr)
      val varname = vars.next()
      index += varname.length()
      Var(varname)
    }
  }
    
  }
object Main {
  type Environment = String => Int
  def prettyPrint(s : String , e : Main.Environment): Unit = {
      if (s.trim.startsWith("{")) {
          val rd = new RecursiveDescent(s)
          val set = rd.parseSet()
          print("SET --->")
          println(set)
          set match {
              case s: SetComp => symanticAnalyser(s)
              case _ => 
            }
          print("Evaluated Set --->")
          println(evalSet(set, e).toList.sorted)
        } else {
          val rd = new RecursiveDescent(s)
          val exp = rd.parseS()
          print("Arithmetic --> ")
          println(exp)
          print("Simplified --> ")
          println(exp.eval(e))
        }
        }

  def main(args: Array[String]) = {
    val env: Environment = {
      case "x" => 5
      case "y" => 7
    }
    var running = true
    while (running) {
      println()
      println(s"${BOLD}${GREEN}==========================${RESET}")
      println()
      print(s"${BOLD}Expr --> ${RESET}")
      val expr = readLine()
      if (expr == "exit"){
        println(s"${BOLD}${BLUE}Bye !${RESET}")
        running = false
      }
      else {
          prettyPrint(expr, env)
        }
    }
  }
}
```


presentation compiler configuration:
Scala version: 3.3.1
Classpath:
<WORKSPACE>/.bloop/root/bloop-bsp-clients-classes/classes-Metals-VVrk5BjmRSG8zRrKWL-Zpw== [exists ], <HOME>/Library/Caches/bloop/semanticdb/com.sourcegraph.semanticdb-javac.0.11.2/semanticdb-javac-0.11.2.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.1/scala3-library_3-3.3.1.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.10/scala-library-2.13.10.jar [exists ]
Options:
-Xsemanticdb -sourceroot <WORKSPACE>




#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:11)
	dotty.tools.dotc.core.TypeOps$.dominators$1(TypeOps.scala:248)
	dotty.tools.dotc.core.TypeOps$.approximateOr$1(TypeOps.scala:382)
	dotty.tools.dotc.core.TypeOps$.orDominator(TypeOps.scala:395)
	dotty.tools.dotc.core.Types$OrType.join(Types.scala:3435)
	dotty.tools.dotc.core.Types$OrType.widenUnionWithoutNull(Types.scala:3451)
	dotty.tools.dotc.core.Types$Type.widenUnion(Types.scala:1296)
	dotty.tools.dotc.core.ConstraintHandling.widenOr$1(ConstraintHandling.scala:652)
	dotty.tools.dotc.core.ConstraintHandling.widenInferred(ConstraintHandling.scala:668)
	dotty.tools.dotc.core.ConstraintHandling.widenInferred$(ConstraintHandling.scala:29)
	dotty.tools.dotc.core.TypeComparer.widenInferred(TypeComparer.scala:30)
	dotty.tools.dotc.core.ConstraintHandling.instanceType(ConstraintHandling.scala:707)
	dotty.tools.dotc.core.ConstraintHandling.instanceType$(ConstraintHandling.scala:29)
	dotty.tools.dotc.core.TypeComparer.instanceType(TypeComparer.scala:30)
	dotty.tools.dotc.core.TypeComparer$.instanceType(TypeComparer.scala:3010)
	dotty.tools.dotc.core.Types$TypeVar.instantiate(Types.scala:4809)
	dotty.tools.dotc.typer.Inferencing.tryInstantiate$1(Inferencing.scala:738)
	dotty.tools.dotc.typer.Inferencing.doInstantiate$1(Inferencing.scala:741)
	dotty.tools.dotc.typer.Inferencing.interpolateTypeVars(Inferencing.scala:744)
	dotty.tools.dotc.typer.Inferencing.interpolateTypeVars$(Inferencing.scala:559)
	dotty.tools.dotc.typer.Typer.interpolateTypeVars(Typer.scala:116)
	dotty.tools.dotc.typer.Typer.simplify(Typer.scala:3128)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3114)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3096)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3112)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3184)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3188)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3237)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3256)
	dotty.tools.dotc.typer.Typer.typedBlockStats(Typer.scala:1159)
	dotty.tools.dotc.typer.Typer.typedBlock(Typer.scala:1163)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3056)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3112)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3184)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3188)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3300)
	dotty.tools.dotc.typer.Typer.$anonfun$57(Typer.scala:2486)
	dotty.tools.dotc.inlines.PrepareInlineable$.dropInlineIfError(PrepareInlineable.scala:243)
	dotty.tools.dotc.typer.Typer.typedDefDef(Typer.scala:2486)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3024)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3111)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3184)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3188)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3210)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3256)
	dotty.tools.dotc.typer.Typer.typedClassDef(Typer.scala:2669)
	dotty.tools.dotc.typer.Typer.typedTypeOrClassDef$1(Typer.scala:3036)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3040)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3111)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3184)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3188)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3210)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3256)
	dotty.tools.dotc.typer.Typer.typedPackageDef(Typer.scala:2812)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3081)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3112)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3184)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3188)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3300)
	dotty.tools.dotc.typer.TyperPhase.typeCheck$$anonfun$1(TyperPhase.scala:44)
	dotty.tools.dotc.typer.TyperPhase.typeCheck$$anonfun$adapted$1(TyperPhase.scala:54)
	scala.Function0.apply$mcV$sp(Function0.scala:42)
	dotty.tools.dotc.core.Phases$Phase.monitor(Phases.scala:440)
	dotty.tools.dotc.typer.TyperPhase.typeCheck(TyperPhase.scala:54)
	dotty.tools.dotc.typer.TyperPhase.runOn$$anonfun$3(TyperPhase.scala:88)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.typer.TyperPhase.runOn(TyperPhase.scala:88)
	dotty.tools.dotc.Run.runPhases$1$$anonfun$1(Run.scala:246)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.ArrayOps$.foreach$extension(ArrayOps.scala:1321)
	dotty.tools.dotc.Run.runPhases$1(Run.scala:262)
	dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:270)
	dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:279)
	dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:67)
	dotty.tools.dotc.Run.compileUnits(Run.scala:279)
	dotty.tools.dotc.Run.compileSources(Run.scala:194)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:165)
	scala.meta.internal.pc.MetalsDriver.run(MetalsDriver.scala:45)
	scala.meta.internal.pc.WithCompilationUnit.<init>(WithCompilationUnit.scala:28)
	scala.meta.internal.pc.SimpleCollector.<init>(PcCollector.scala:373)
	scala.meta.internal.pc.PcSemanticTokensProvider$Collector$.<init>(PcSemanticTokensProvider.scala:61)
	scala.meta.internal.pc.PcSemanticTokensProvider.Collector$lzyINIT1(PcSemanticTokensProvider.scala:61)
	scala.meta.internal.pc.PcSemanticTokensProvider.Collector(PcSemanticTokensProvider.scala:61)
	scala.meta.internal.pc.PcSemanticTokensProvider.provide(PcSemanticTokensProvider.scala:90)
	scala.meta.internal.pc.ScalaPresentationCompiler.semanticTokens$$anonfun$1(ScalaPresentationCompiler.scala:109)
```
#### Short summary: 

java.lang.AssertionError: assertion failed