package sll.parsing

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.token.StdTokens
import sll.syntax._

abstract class SllParser extends StandardTokenParsers with ImplicitConversions {
  lexical.delimiters += (",", "=", "(", ")")
  lexical.reserved += ("program")
  
  def start: Parser[List[Definition]]
  
  def definition: Parser[Definition]
  
  def fDef: Parser[FunDef] =
    funcName ~ ("(" ~> repsep(variable, ",") <~ ")") ~ ("=" ~> expr) ^^ FDef
  
  def gDef: Parser[FunDef] =
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
  
  def fCall: Parser[FCall] = funcName ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ FCall
  
  def ctor: Parser[Expr] = ctorName ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ Ctor
  
  def parseAll[T](p: Parser[T], in: String) = {
    phrase(p)(new lexical.Scanner(in)) match {
     case t if t.successful => t.get
     case t => error(t.toString)
     }
  }
  
  def parseDefs(p: String): List[Definition]
  def parseFCall(p: String): FCall = parseAll(fCall, p)
}

object UntypedSllParser extends SllParser {
  def start: Parser[List[FunDef]] = rep(definition)
  def definition = gDef | fDef
  def parseDefs(p: String): List[FunDef] = parseAll(start, p)  
}

object TypedSllParser extends SllParser {
  lexical.delimiters += ("|", ":", "->")
  lexical.reserved += ("data")
  
  def typeName = 
    elem("Type name", x => x.isInstanceOf[lexical.Identifier] && x.chars.head.isUpper) ^^
    {_.chars}
  
  def start: Parser[List[Definition]] = rep(definition)
  
  def definition = gDef | fDef | typeDef | funType
  
  def parseDefs(p: String): List[Definition] = parseAll(start, p)  
  
  def typeDef: Parser[TypeDef] = ("data" ~> typeName) ~ ("=" ~> repsep(typeCtor, "|")) ^^ SllType 
  
  def typeCtor: Parser[TypeCtor] = ctorName ~ ("(" ~> repsep(typeName, ",") <~ ")") ^^ TypeCtor
  
  def localVarTypes: Parser[List[String]] = ("(" ~> repsep(typeName, ",") <~ ")") | oneVarType
  def oneVarType: Parser[List[String]] = typeName ^^ (e => List(e))
  
  def funType: Parser[FunType] = (funcName <~ ":") ~ (localVarTypes <~ "->") ~ typeName ^^ FunType 
}