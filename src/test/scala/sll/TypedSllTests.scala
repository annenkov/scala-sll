package sll.tests

import org.scalatest._
import sll.parsing.TypedSllParser
import sll.syntax._
import sll.typed._

class TypedSllTests  extends FunSuite with Matchers {
	test("Inconsistent types declaration") {
	  val prog =
      """
      data Nat = Z() | S(ZZZ)      
      """
	  an [NoSuchTypeException] should be thrownBy EnvUtils.makeTypeEnv(TypedSllParser.parseDefs(prog))
	}	
  
	test("Well-typed f-function") {
    val prog =
      """
      data Nat = Z() | S(Nat)
      
      plusOne: Nat -> Nat
      plusOne(x) = S(x) 
      """    
    noException should be thrownBy TypeChecker.checkProgram(prog)    
	}
	
	test("Ill-typed f-function") {
    val prog =
      """
      data Nat = Z() | S(Nat)
      data Bool = True() | False()
      
      plusOne: Nat -> Bool
      plusOne(x) = S(x) 
      """    
    val thrown = the [TypeMismatchException] thrownBy TypeChecker.checkProgram(prog)
    thrown.getMessage() should equal ("Type mismatch: expected Bool, given Nat")
	}
	
	test("Well-typed g-function") {
    val prog =
      """
      data Nat = Z() | S(Nat)
      
      add: (Nat, Nat) -> Nat
      add(Z(), x) = x
      add(S(x), y) = S(add(x,y))
      """    
    noException should be thrownBy TypeChecker.checkProgram(prog)    
	}
	
	test("Ill-typed g-function (wrong pattern)") {
    val prog =
      """
      data Nat = Z() | S(Nat)
      data Bool = True() | False()
      
      add: (Nat, Nat) -> Nat
      add(True(), x) = x
      add(S(x), y) = S(add(x,y))
      """
    val thrown = the [TypeMismatchException] thrownBy TypeChecker.checkProgram(prog)
    thrown.getMessage() should equal ("Type mismatch: expected Nat, given Bool")
	}
	
	test("Ill-typed g-function") {
    val prog =
      """
      data Nat = Z() | S(Nat)
      data Bool = True() | False()
      
      add: (Nat, Bool) -> Nat
      add(Z(), x) = x
      add(S(x), y) = S(add(x,y))
      """
    val thrown = the [TypeMismatchException] thrownBy TypeChecker.checkProgram(prog)
    thrown.getMessage() should equal ("Type mismatch: expected Bool, given Nat")
	}
	
	test("g-function with wrong arity") {
    val prog =
      """
      data Nat = Z() | S(Nat)
      
      add: Nat -> Nat
      add(Z(), x) = x
      add(S(x), y) = S(add(x,y))
      """
    val thrown = the [ArityMismatchException] thrownBy TypeChecker.checkProgram(prog)
    thrown.getMessage() should equal ("Arity mismatch for add. In type declaration: 1 arguments, in function declaration: 2")
	}
}