package sll.tests.gen

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import sll.syntax._

object ASTGen {
    val names = Gen.resize(5, Gen.alphaStr) suchThat (!_.isEmpty())
	val funcNames = names map {_.toLowerCase()}
    val ctorNames = names map {_.capitalize}
    val varNames = funcNames
    val typeNames = ctorNames
    
    val vars = varNames map {Var(_)}
    
    val typeCtors = for {
      name <- ctorNames
	  argsCount <- Gen.choose(0, 2)
	  tArgs <- Gen.listOfN(argsCount, typeNames)
    } yield TypeCtor(name, tArgs)
    
    val types = for {
      name <- typeNames
      argsCount <- Gen.choose(1, 3)
      ctors <- Gen.listOfN(argsCount, typeCtors)
    } yield SllType(name, ctors)
    
    val funTypes = for {
      name <- funcNames
      argsCount <- Gen.choose(1, 3)
	  tArgs <- Gen.listOfN(argsCount, typeNames)
	  retType <- typeNames
    } yield FunType(name, tArgs, retType)
	
    val fCalls: Gen[FCall] = for {
	  n <- funcNames
	  argsCount <- Gen.choose(0, 2)
	  fArgs <- Gen.listOfN(argsCount, Gen.oneOf(fCalls, ctors, vars))
	  } yield FCall(n, fArgs)
	
	val typeDefs = Gen.oneOf(types, funTypes)
	
	val ctors: Gen[Ctor] = for {
	  n <- ctorNames
	  argsCount <- Gen.choose(0, 2)
	  fArgs <- Gen.listOfN(argsCount, Gen.oneOf(fCalls, ctors, vars))
	} yield Ctor(n, fArgs)
	
	val expr: Gen[Expr] = Gen.oneOf(fCalls, ctors)
	
	val pats: Gen[Pat] = for {
	  patVarsCount <- Gen.choose(0, 2)
	  patVars <- Gen.listOfN(patVarsCount, vars)
	  name <- ctorNames
	} yield Pat(name, patVars)
	
	val fDefs: Gen[FunDef] = for {
	  fName <- funcNames
	  varsCount <- Gen.choose(0, 2)
	  vars <- Gen.listOfN(varsCount, vars)
	  body <- expr
	} yield FDef(fName, vars, body)
	
	val gDefs: Gen[FunDef] = for {	
	  fName <- funcNames
	  varsCount <- Gen.choose(0, 2)
	  vars <- Gen.listOfN(varsCount, vars)
	  pat <- pats
	  body <- expr
	} yield GDef(fName, pat, vars, body)
	
	val defs = Gen.oneOf(fDefs, gDefs)
    
	implicit val arbFCall: Arbitrary[Expr] = Arbitrary(expr)
	implicit val arbFunDef: Arbitrary[FunDef] = Arbitrary(defs)
	implicit val arbTypeDef: Arbitrary[TypeDef] = Arbitrary(typeDefs)
} 