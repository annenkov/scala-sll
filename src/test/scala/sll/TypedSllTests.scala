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
	  an [NoSuchTypeException] should be thrownBy TypeChecker.makeTypeEnv(prog)
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
      
      plusOne: Nat -> Bool
      plusOne(x) = S(x) 
      """    
    an [TypeMismatchException] should be thrownBy TypeChecker.checkProgram(prog)    
	}
}