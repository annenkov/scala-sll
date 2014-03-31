package sll.interpreter

import sll.syntax._
import PartialFunction._

object SllEval {
  
  def eval(p: List[Definition])(t: Expr): Expr = t match {
    case fcall @ FCall(name, args) => eval(p)(unfold(fcall, p))
    case Ctor(name, args) => Ctor(name, args.map(eval(p)))
  }

  def unfold(t: Expr, p: List[Definition]): Expr =
    t match {
      case FCall(name, args) => getFunDef(p, name) match {
        case Some(FDef(name, params, body)) =>
          substitute(Map(params.map(_.name).zip(args): _*))(body)
        case None => t match {
          case FCall(name, (ctor @ Ctor(cName, cArgs)) :: args) =>
            getGFuncDef(p, name, ctor) match {             
              case Some(GDef(name, pat, params, body)) => {
                println(params)
                substitute(Map((pat.args ++ params).map(_.name).zip(cArgs ++ args): _*))(body)
              }              
              case None => error(s"Unknown function: ${name}")
            }            
          case FCall(name, (fcall @ FCall(_, _)) :: args) => FCall(name, unfold(fcall, p) :: args)
        }
      }
    }

  def substitute(ctx: Map[String, Expr])(t: Expr): Expr = t match {
    case FCall(name, body) => FCall(name, body map substitute(ctx))
    case Ctor(name, body) => Ctor(name, body map substitute(ctx))
    case Var(x) if (ctx contains x) => ctx(x)
    case v @ Var(_) => v
  }

  def getFunDef(p: List[Definition], name: String) =
    p.find(d => cond(d){case FDef(_name, params, body) => name == _name})

  def getGFuncDef(p: List[Definition], name: String, ctor: Ctor): Option[Definition] = {
    p.find(d => cond(d) {
      case GDef(_name, Pat(cName, args), params, body) => 
        name == _name && ctor.name == cName && ctor.args.length == args.length
    })
  }
}