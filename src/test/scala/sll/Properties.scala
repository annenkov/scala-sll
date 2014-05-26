package sll.tests

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalatest.prop.Checkers._
import org.scalatest.FunSuite
import org.scalacheck.{Prop, Arbitrary, Properties}
import sll.syntax._
import sll.parsing.TypedSllParser
import sll.io.PP
import sll.tests.gen.ASTGen._
 

class PropertiesTest extends FunSuite {
  test("Parse(PrettyPrint(AST) == AST) for expressions") {
    check(Prop.forAll {e: Expr => TypedSllParser.parseExpr(PP.showExpr(e)) == e})
    }
  
  test("Parse(PrettyPrint(AST) == AST) for function definitions") {
    check(Prop.forAll {e: FunDef => TypedSllParser.parseDefs(PP.showDef(e)).head == e})
    }
  
  test("Parse(PrettyPrint(AST) == AST) for type definitions") {
    check(Prop.forAll {e: TypeDef => TypedSllParser.parseDefs(PP.showDef(e)).head == e})
    }
}