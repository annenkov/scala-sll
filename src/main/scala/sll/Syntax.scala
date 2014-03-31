package sll.syntax

sealed abstract class Expr
sealed abstract class Definition
sealed abstract class Pattern

case class Var(name: String) extends Expr
case class Ctor(name: String, args: List[Expr]) extends Expr
case class FCall(name: String, args: List[Expr]) extends Expr
case class Pat(name: String, args: List[Var]) extends Pattern 
case class FDef(name: String, params: List[Var], body: Expr) extends Definition
case class GDef(name: String, pat: Pat, params: List[Var], body: Expr) extends Definition
case class Program(defs: List[Definition])
