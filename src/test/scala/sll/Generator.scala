package sll.tests.gen

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import sll.syntax._

object ASTGen {
    val names = Gen.resize(5, Gen.alphaStr) suchThat (!_.isEmpty())
	val funcNames = names map {_.toLowerCase()}
    val ctorNames = names map {_.capitalize}
	
    val fCalls: Gen[FCall] = for {
	  n <- funcNames
	  argsCount <- Gen.choose(0, 2)
	  fArgs <- Gen.listOfN(argsCount, Gen.oneOf(fCalls, ctors))
	  } yield FCall(n, fArgs)
	
	val ctors: Gen[Ctor] = for {
	  n <- ctorNames
	  argsCount <- Gen.choose(0, 2)
	  fArgs <- Gen.listOfN(argsCount, Gen.oneOf(fCalls, ctors))
	} yield Ctor(n, fArgs)
	
	val expr: Gen[Expr] = Gen.oneOf(fCalls, ctors)
    
	implicit val arbFCall: Arbitrary[Expr] = Arbitrary(expr)
} 