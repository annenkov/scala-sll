package sll.interpreter

import sll.syntax._
import PartialFunction._

object SllEval {
  
  def eval(p: List[Definition])(t: Expr): Expr =  t match {
    case fcall @ FCall(name, args) => eval(p)(unfold(fcall, p))
    case Ctor(name, args) => Ctor(name, args.map(eval(p)))
    case Var(_) => error("Variables are not allowed in tasks")
  }

  def unfold(funCall: FCall, p: List[Definition]): Expr =
       getFuncDef(p, funCall.name, funCall.args) match {
        case Some(FDef(name, params, body)) =>
          substitute(Map(params.map(_.name).zip(funCall.args): _*))(body)
        case Some(GDef(name, pat, params, body)) => {
        		val ctor@Ctor(_,cArgs) :: _args = funCall.args
                substitute(Map((pat.args ++ params).map(_.name).zip(cArgs ++ _args): _*))(body)
              }        
        case None => funCall.args match {
                case (fcall @ FCall(_, _)) :: _args => FCall(funCall.name, unfold(fcall, p) :: _args)
                case _ => error(s"Unknown function: ${funCall.name}")}                              
        }
  
  def substitute(ctx: Map[String, Expr])(t: Expr): Expr = t match {
    case FCall(name, body) => FCall(name, body map substitute(ctx))
    case Ctor(name, body) => Ctor(name, body map substitute(ctx))
    case Var(x) if (ctx contains x) => ctx(x)
    case v @ Var(_) => v
  }
  
  def getFuncDef(p: List[Definition], name: String, args: List[Expr]): Option[Definition] = 
    getFFuncDef(p, name).orElse(getGFuncDef(p, name, args)) 

  def getFFuncDef(p: List[Definition], name: String): Option[Definition] =
    p.find(d => cond(d){case FDef(_name, params, body) => name == _name})

  def getGFuncDef(p: List[Definition], name: String, args: List[Expr]): Option[Definition] = {
    args match {
      case Ctor(cName, cArgs)::_ => p.find(d => cond(d) {
        case GDef(_name, Pat(pName, pArgs), params, body) => 
        name == _name && cName == pName && cArgs.length == pArgs.length
        })
      case _ => None
    }    
  }
}