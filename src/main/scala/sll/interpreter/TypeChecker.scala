package sll.typed

import sll.syntax._
import sll.parsing._
import PartialFunction.cond

case class FunTypeEnv(dom: List[String], returnType: String)

object TypeChecker {
  
  def typeDefs(p: String): List[SllType] = TypedSllParser.parseDefs(p).collect({case d:SllType => d })
  def funTypes(p: String): List[FunType] = TypedSllParser.parseDefs(p).collect({case d:FunType => d })
  def fFunVars(p: String, name: String) = {
    val funDefs = TypedSllParser.parseDefs(p)
    funDefs.collect({case FDef(_name, params, _) if _name == name => params }).head
    }
  
  def makeTypeEnv(p: String): Map[String, FunTypeEnv] = {
    val tDefs = typeDefs(p)
    checkTypeDefs(tDefs)
    val ctorEnv = (for (SllType(n, ctors) <- tDefs) yield ctorEnvEntries(n, ctors, tDefs)).flatten
    val ts = funTypes(p)
    checkFunTypes(tDefs, ts)
    val funEnv = ts.map({case FunType(name, dom, retType) =>    
      (name, FunTypeEnv(dom, retType))
      })
    Map(funEnv ++ ctorEnv: _*)
    }
  
  def ctorEnvEntries(t: String, cs:List[TypeCtor], tDefs: List[SllType]) =
    cs.map(c => (c.name, FunTypeEnv(c.typeArgs, t)))
  
  def resolveType(tDefs: List[SllType])(typeName: String) = {
    if (!tDefs.exists(_.name == typeName)) throw new NoSuchTypeException(typeName)
    }
  
  def typeDefsToMap(ds: List[SllType]) = Map(ds.map({case SllType(name, ctors) => (name, ctors)}): _*)
  
  def checkTypeDefs(ds: List[SllType]) = {
    ds.foreach({case SllType(name, ctors) => ctors.map(c => c.typeArgs.map(resolveType(ds)))}) 
    }
  
  def checkFunTypes(typeDefs: List[SllType], fts: List[FunType]) = 
    fts.map(ft => ft.localVarTypes.map(resolveType(typeDefs)))
  
  def extractTypeDefs(p: List[Definition]) = p.filter(d => d.isInstanceOf[FunType] || d.isInstanceOf[SllType])
  
  def typeOf(e: Expr, typeEnv: Map[String, FunTypeEnv]): String = e match {
    case FCall(fName, fArgs) => {
      fArgs.zip(typeEnv(fName).dom).foreach({case (a, t) => checkType(a, t, typeEnv) })
      typeEnv(fName).returnType
    }        
    case Ctor(cName, cArgs) => { 
      cArgs.zip(typeEnv(cName).dom).foreach({case (a, t) => checkType(a, t, typeEnv) })
      typeEnv(cName).returnType
      }
    case Var(vName) => typeEnv(vName).returnType
  } 
  
  def checkType(e: Expr, expectedType: String, typeEnv: Map[String, FunTypeEnv]) = {
    val actualType = typeOf(e, typeEnv) 
	if (!(actualType == expectedType)) throw new TypeMismatchException(expectedType, actualType) 
  }
  
  def checkFunDef(d: FunDef, typeEnv: Map[String, FunTypeEnv]) = d match {
    case FDef(fName, params, body) => {
      val locals = params.zip(typeEnv(fName).dom).map({case (v, t) => (v.name, FunTypeEnv(List(), t))})
      val actualFunType = typeOf(body, typeEnv ++ locals) 
      val expectedFunType = typeEnv(fName).returnType
      if (actualFunType != expectedFunType) throw new TypeMismatchException(actualFunType, expectedFunType)
      } 
  }
  
  def checkProgram(p: String) = {
    val defs = TypedSllParser.parseDefs(p).collect({case d:FDef => d; case d:GDef => d})
    defs.foreach(d => checkFunDef(d, makeTypeEnv(p)))
  }
}

class NoSuchTypeException(t: String) extends Exception(s"Undefined type ${t}")
class TypeMismatchException(t1: String, t2: String) extends Exception(s"Type mismatch: expected ${t1}, given ${t2}")