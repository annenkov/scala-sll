package sll.tests

import org.scalatest._
import sll.parsing.SllParser
import sll.syntax._

class ParsingTest extends FunSuite {
	test("Parsing identity function definition: f(x) = x") {
	  val res = SllParser.parseDefs("f(x) = x")
	  assert(res == List(FDef("f", List(Var("x")), Var("x"))))
	}
	
	test("Parsing constant function definition: f(x) = Z()") {
	  val res = SllParser.parseDefs("f(x) = Z()")
	  assert(res == List(FDef("f", List(Var("x")), Ctor("Z", List()))))
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