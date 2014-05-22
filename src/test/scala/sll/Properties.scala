package sll.tests

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalatest.prop.Checkers._
import org.scalatest.FunSuite
import org.scalacheck.Prop
import org.scalacheck.Arbitrary
import sll.syntax._
import sll.parsing.TypedSllParser
import sll.io.PP
import sll.tests.gen.ASTGen._
 

class PropertiesTest extends FunSuite {
  test("Parse(PrettyPrint(AST) == AST)") {
    check(Prop.forAll {fCall: FCall => TypedSllParser.parseFCall(PP.showExpr(fCall)) == fCall})
    }
}