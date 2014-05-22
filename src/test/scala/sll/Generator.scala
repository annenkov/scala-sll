package sll.tests.gen

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import sll.syntax._

object ASTGen {
	val funcNames = Gen.resize(5, Gen.alphaStr) suchThat (!_.isEmpty()) map {v => v.toLowerCase()}
	val fCalls: Gen[FCall] = for {
	  n <- funcNames
	  argsCount <- Gen.choose(0, 2)
	  fArgs <- Gen.listOfN(argsCount, fCalls)
	  } yield FCall(n, fArgs)
	  
    implicit val arbFCall: Arbitrary[FCall] = Arbitrary(ASTGen.fCalls)
} 