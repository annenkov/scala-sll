package sll.tests

import org.scalatest._
import sll.parsing.SllParser
import sll.syntax._
import sll.interpreter._

class ParsingTest extends FunSuite {
	test("Parsing identity function definition: f(x) = x") {
	  val res = SllParser.parseDefs("f(x) = x")
	  assert(res == List(FDef("f", List(Var("x")), Var("x"))))
	}
	
	test("Parsing constant function definition: f(x) = Z()") {
	  val res = SllParser.parseDefs("f(x) = Z()")
	  assert(res == List(FDef("f", List(Var("x")), Ctor("Z", List()))))
	}
	
	test("Parsing f-function calling another function: f(x) = g(Z())") {
	  val res = SllParser.parseDefs("f(x) = g(Z())")
	  assert(res == List(FDef("f", List(Var("x")), FCall("g", List(Ctor("Z", List()))))))
	}
	
	test("Parsing g-function definition") {
	  val prog = 
	    """
	    add(Z(), x) = x
	    add(S(x), y) = S(add(x,y))
	    """
	  val res = SllParser.parseDefs(prog)
	  assert(res == List(
	      GDef("add", Pat("Z", List()), List(Var("x")), Var("x")),
	      GDef("add", Pat("S", List(Var("x"))), List(Var("y")),
	          Ctor("S", List(FCall("add", List(Var("x"), Var("y"))))))))
	}
}

class SubstitutionTest extends FunSuite {
  test("Substitution x -> Z() in f(x)") {
    val ctx = Map("x" -> Ctor("Z", List()))
    val body = FCall("f", List(Var("x")))
    assert(SllEval.substitute(ctx)(body) == FCall("f",List(Ctor("Z",List()))))
  }
  
  test("Substitution: x -> Z() in f(x,y)") {
    val ctx = Map("x" -> Ctor("Z", List()))
    val body = FCall("f", List(Var("x"), Var("y")))
    assert(SllEval.substitute(ctx)(body) == FCall("f",List(Ctor("Z",List()), Var("y"))))
  }
  
  test("Substitution: x -> Z() in Z(f(x))") {
    val ctx = Map("x" -> Ctor("Z", List()))
    val body = Ctor("Z", List(FCall("f", List(Var("x")))))
    assert(SllEval.substitute(ctx)(body) == Ctor("Z", List(FCall("f",List(Ctor("Z",List()))))))
  }
  
  test("Substitution: x -> Z() in f(x,x)") {
    val ctx = Map("x" -> Ctor("Z", List()))
    val body = FCall("f", List(Var("x"), Var("x")))
    assert(SllEval.substitute(ctx)(body) == FCall("f",List(Ctor("Z",List()), Ctor("Z",List()))))
  }
  
  test("Unfold func call") {
    val prog = 
      """
      f(x) = x      
      """
    val expr = "f(Z())"
      assert(SllEval.unfold(SllParser.parseFCall(expr), SllParser.parseDefs(prog))
          == Ctor("Z", List()))
  }  
}

class EvalTests extends FunSuite {
  test("Eval f-functions") {
    val prog =
      """
      plusOne(x) = S(x)      
      """
    val expr = "plusOne(plusOne(Z()))"
    assert(SllEval.eval(SllParser.parseDefs(prog))(SllParser.parseFCall(expr))
      == SllParser.parseAll(SllParser.ctor, "S(S(Z()))"))
  }
  
  test("Eval nested (f-func in g-func) call") {
    val prog =
      """
      plusOne(x) = S(x)
      add(Z(), x) = x
	  add(S(x), y) = S(add(x,y))
      """
    val expr = "add(plusOne(Z()), S(Z()))"
    assert(SllEval.eval(SllParser.parseDefs(prog))(SllParser.parseFCall(expr))
      == SllParser.parseAll(SllParser.ctor, "S(S(Z()))"))
  }
  
  test("Laziness test") {
    val prog =
      """
      head(Cons(x, xs)) = x
      tail(Cons(x, xs)) = xs
      zeros() = Cons(Z(), zeros())
      """
    val expr = "head(zeros())"
    assert(SllEval.eval(SllParser.parseDefs(prog))(SllParser.parseFCall(expr))
      == SllParser.parseAll(SllParser.ctor, "Z()"))
  }
}