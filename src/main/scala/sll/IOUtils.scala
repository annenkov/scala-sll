package sll.io

import sll.syntax._

object PP {
  import PPCombinators._
	def showExpr(t: Expr): String = t match {
	  case Var(v) => v
	  case FCall(name, args) => name ++ argsInParens(showExpr, args)
	  case Ctor(name, args) => name ++ argsInParens(showExpr, args)
	}
	
	def showDef(d: Definition): String = d match {
	  case FDef(name, params, body) => s"${name}${argsInParens(showExpr, params)} = ${showExpr(body)}"
	  case GDef(name, pat, List(), body) => s"${name}(${showPat(pat)}) = ${showExpr(body)}"
	  case GDef(name, pat, params, body) =>
	    s"${name}${argsInParens(args=List(showPat(pat), argsList(showExpr, params)))}) = ${showExpr(body)}"
	  case SllType(name, ctors) => s"${name} = ${repsep(showTypeCtor, "|")(ctors)}"
	  case FunType(name, dom, retType) => s"${name} = ${argsInParens(args=dom)} -> ${retType}"
	}
	
	def showTypeCtor(tc: TypeCtor) = tc.name ++ argsInParens(args=tc.typeArgs)
	
	def showPat(p: Pattern): String = p match {
	  case Pat(n, params) => s"${n}(${repsep(showExpr, ", ")(params)})"
	}
	
	def showDefs(defs: List[Definition]): String = repsep(showDef, "\n")(defs)
	
	def argsInParens[A](pp: A => String = identity(_:String), args: List[A]) = parens(argsList(pp, args))
	def argsList[A](pp: A => String = identity(_:String), args: List[A]) = repsep(pp, ", ")(args)
}

object PPCombinators {
    def repsep[A](f: A => String , sep: String)(x: List[A])= x.map(f).mkString(sep)
    
    def enclose(left: String, s: String, right: String) = s"${left}${s}${right}"
    
    def parens(s: String) = enclose("(", s, ")")
} 