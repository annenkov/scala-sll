package sll.parsing

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.token.StdTokens
import sll.syntax._

object SllParser extends StandardTokenParsers with ImplicitConversions {
  lexical.delimiters += (",", "=", "(", ")")
  lexical.reserved += ("program")
  
  def start: Parser[List[Definition]] = rep(definition)
  
  def definition = fDef | gDef
  
  def fDef: Parser[Definition] =
    funcName ~ ("(" ~> repsep(variable, ",") <~ ")") ~ ("=" ~> expr) ^^ FDef
  
  def gDef: Parser[Definition] =
    funcName ~ ("(" ~> pat) ~ (gDefVars <~ ")") ~ ("=" ~> expr) ^^ GDef 
  
  def gDefVars = opt("," ~> repsep(variable, ",")) ^^ {
    case Some(v) => v
    case None => List[Var]()
  }
    
  def ctorName = 
    elem("Constructor name", x => x.isInstanceOf[lexical.Identifier] && x.chars.head.isUpper) ^^
    {_.chars}
  
  def funcName = 
    elem("Function name", x => x.isInstanceOf[lexical.Identifier] && x.chars.head.isLower) ^^
    {_.chars}
  
  def expr = fCall | ctor | variable
  
  def pat = ctorName ~ ("(" ~> repsep(variable, ",") <~ ")") ^^ Pat
    
  def variable: Parser[Var] = ident ^^ Var
  
  def fCall: Parser[Expr] = funcName ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ FCall
  
  def ctor: Parser[Expr] = ctorName ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ Ctor
  
  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = {
    phrase(p)(new lexical.Scanner(in))
  }
  
  def parseDefs(p: String) = parseAll(start, p) match {
     case t if t.successful => t.get
     case t => error(t.toString)
     }
}