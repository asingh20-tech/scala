package example

import scala.util.matching.Regex
import scala.collection.mutable.Set

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
    else new Exception("Not starting with {} ")
    
  }

def parseSetBody(): SetExpr = {

  // Empty set {}
  if (index < input.length && input(index) == '}') {
    return SetLiterals(Set.empty)
  }

  // Parse first expression (IMPORTANT: now use parseE, not parseF)
  val firstExpr = parseE()

  // Check for comprehension: '|'
  if (index < input.length && input(index) == '|') {
    index += 1 // skip '|'

    val clauses = parseClauses()
    return SetComp(firstExpr, clauses)
  }

  // From here on → literals or range → must be integers
  val firstValue = firstExpr match {
    case c: Const => c.v
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

def parseClauses(): Clause = {
    val first = parseF()
    val identifier = first match{
        case Var(identifier) => identifier
        case _ => throw new Exception("Not a valid Identifier")
    }


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
    val env: Environment = { case "x" => 5 case "y" => 7 }

    val rd = new RecursiveDescent("x+x*7*y")
    val exp2rd:S = rd.parseE()
    println(exp2rd)
    println(exp2rd.eval(env)) 
  }
}