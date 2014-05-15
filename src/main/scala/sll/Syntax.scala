package sll.syntax

sealed abstract class Expr
sealed abstract class TypeExpr
sealed abstract class Definition
sealed abstract class FunDef extends Definition
sealed abstract class TypeDef extends Definition
sealed abstract class Pattern

case class Var(name: String) extends Expr
case class Ctor(name: String, args: List[Expr]) extends Expr
case class FCall(name: String, args: List[Expr]) extends Expr
case class Pat(name: String, args: List[Var]) extends Pattern 
case class FDef(name: String, params: List[Var], body: Expr) extends FunDef
case class GDef(name: String, pat: Pat, params: List[Var], body: Expr) extends FunDef
case class Program(defs: List[FunDef])

case class TypeCtor(name: String, typeArgs: List[String])
case class SllType(name: String, ctors: List[TypeCtor]) extends TypeDef
case class FunType(funcName: String, localVarTypes: List[String], returnType: String) extends TypeDef
