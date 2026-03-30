error id: 53EDB61C3B06F5423423F6348E31589C
file://<WORKSPACE>/src/main/scala/example/MainProject.scala
### java.lang.AssertionError: assertion failed: position error, parent span does not contain child span
parent      = new RecursiveDescent(null: <notype>) # -1,
parent span = <8171..8275>,
child       = null # -1,
child span  = [8192..8279..8279]

occurred in the presentation compiler.



action parameters:
uri: file://<WORKSPACE>/src/main/scala/example/MainProject.scala
text:
```scala

import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.io.StdIn._

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
          val values = evalSet(source, env)
          var newEnvs = ListBuffer[Main.Environment]()
          for (e <- envs; v <- values) {
            val newEnv: Main.Environment = (x: String) =>
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
      case Some(r) => val a2 = r.eval(env)
        if (r.operator == '*') {
          a1 * a2
        } else {
          a1 % a2
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

  def parseS(): S = parseE()

  def parseSet(): SetExpr = {
    if (index < input.length && input(index) == '{'){
        index +=1
        var result = parseSetBody()
        if (index < input.length && input(index) == '}'){
            index +=1
        }  
        return result 
    }
    else throw new Exception("Not starting with {} ")
    
  }

  def parseSetBody(): SetExpr = {

  // Empty set {}
  if (index < input.length && input(index) == '}') {
    return SetLiterals(Set.empty)
  }
  val firstExpr = parseE()

  // Check for comprehension: '|'
  if (index < input.length && input(index) == '|') {
    index += 1 // skip '|'

    val clauses = parseClauses()
    return SetComp(firstExpr, clauses)
  }

  // From here on → literals or range → must be integers
  val first = parseF()
  val firstValue = first match {
    case Const(v) => v
    case _ => throw new Exception("Expected integer in set")
  }

  // Check for range: ",...,"
  if (index + 4 < input.length && input.substring(index, index + 5) == ",...,") {
    index += 5
    val second = parseF()
    val secondValue = second match {
      case Const(v) => v
      case _ => throw new Exception("Expected integer in range")
    }
    return SetRange(firstValue, secondValue)
  }

  // Otherwise → SetLiterals
  val set = scala.collection.mutable.Set[Int]()
  set.add(firstValue)

  while (index < input.length && input(index) == ',') {
    index += 1
    val next = parseF()
    val nextValue = next match {
      case Const(v) => v
      case _ => throw new Exception("Expected integer in set")
    }
    set.add(nextValue)
  }

  SetLiterals(set.toSet)
}

  def parseClause(): Clause = {
    val startIndex = index
    val remaining = input.substring(index)
    val varMatch = varregex.findPrefixOf(remaining)
    var identifier = ""

    if (varMatch.isDefined) {
        identifier = varMatch.get
        index += identifier.length
    }
    if (index < input.length && input.substring(index).startsWith("in")){
        index += 2
        return Generator(identifier , parseSet())
    }
    else {
        index = startIndex
    }
    val left = parseE()

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

    val right = parseE()
    Guard(left, op, right)
}

    def parseClauses() : List[Clause] = {
        var list = ListBuffer[Clause]()
        list += parseClause()
        while (index < input.length && input(index) == ',') {
        index += 1
        list += parseClause()
        }
        return list.toList
    }

  def parseE(): E = E(parseT(), parseE2())

  def parseE2(): Option[E2] = {
    if (index < input.length && input(index) == '+'){
      index+=1; // Advance past +
      Some(E2('+',parseE()))
    }
    else if (index < input.length && input(index) == '-'){
      index+=1; // Advance past -
      Some(E2('-',parseE()))
    }
    else None
  }
  def parseF(): S = {
  if (index < input.length && input(index) == '(') {
    index += 1 
    val expr = parseE()
    index += 1 
    expr
  } 
  else {
    val remaining = input.substring(index)
    val numMatch = constregex.findPrefixOf(remaining)
    if (numMatch.isDefined) {
      val numStr = numMatch.get
      index += numStr.length
      Const(numStr.toInt)
    } 
    else {
      val varMatch = varregex.findPrefixOf(remaining).get
      index += varMatch.length
      Var(varMatch)
    }
  }
}

  
  def parseT(): T = T(parseF(),parseT2())

  def parseT2() : Option[T2]= {
    if (index < input.length && input(index) == '*'){
      index+=1; // Advance past *
      Some(T2('*',parseT()))
    }
    else if (index < input.length && input(index) == '%'){
      index+=1; // Advance past %
      Some(T2('%',parseT()))
    }
    else None
  }

  def parseTerminal(): Terminal = {
    // Get the unparsed part of the string.
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

  def main(args: Array[String]) = {
    val env: Environment = { 
      case "x" => 5
      case "y" => 7 
      }
    println("Expr --> ")
    val expr = readLine() 
    val rd = new RecursiveDescent(")
    val exp2rd:S = rd.parseE()
    println(exp2rd)
    println(exp2rd.eval(env)) 
  }
}
```


presentation compiler configuration:
Scala version: 3.3.1
Classpath:
<WORKSPACE>/.bloop/root/bloop-bsp-clients-classes/classes-Metals-8cp6JnTiSACvuBetxyDb3g== [exists ], <HOME>/Library/Caches/bloop/semanticdb/com.sourcegraph.semanticdb-javac.0.11.2/semanticdb-javac-0.11.2.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.1/scala3-library_3-3.3.1.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.10/scala-library-2.13.10.jar [exists ]
Options:
-Xsemanticdb -sourceroot <WORKSPACE>




#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:8)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:175)
	dotty.tools.dotc.ast.Positioned.check$1$$anonfun$3(Positioned.scala:205)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:205)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.check$1$$anonfun$3(Positioned.scala:205)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:205)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.check$1$$anonfun$3(Positioned.scala:205)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:205)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.check$1$$anonfun$3(Positioned.scala:205)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:205)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.parsing.Parser.parse$$anonfun$1(ParserPhase.scala:38)
	dotty.tools.dotc.parsing.Parser.parse$$anonfun$adapted$1(ParserPhase.scala:39)
	scala.Function0.apply$mcV$sp(Function0.scala:42)
	dotty.tools.dotc.core.Phases$Phase.monitor(Phases.scala:440)
	dotty.tools.dotc.parsing.Parser.parse(ParserPhase.scala:39)
	dotty.tools.dotc.parsing.Parser.runOn$$anonfun$1(ParserPhase.scala:48)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.parsing.Parser.runOn(ParserPhase.scala:48)
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

java.lang.AssertionError: assertion failed: position error, parent span does not contain child span
parent      = new RecursiveDescent(null: <notype>) # -1,
parent span = <8171..8275>,
child       = null # -1,
child span  = [8192..8279..8279]