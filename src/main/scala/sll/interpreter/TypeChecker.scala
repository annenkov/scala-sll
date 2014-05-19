package sll.typed

import sll.syntax._
import sll.parsing._
import PartialFunction.cond


case class TypeEnvEntry(dom: List[String], returnType: String) {
  
  def this(t: String) {
    this(List(), t)
  }
  
  override def toString() = dom match {
    case _ if dom.length == 0 => returnType
    case _ if dom.length == 1 => s"${dom.head} -> ${returnType}"
    case _ => s"(${dom.mkString(", ")}) -> ${returnType}"
  } 
}

object TypeEnvEntry {
  def apply(t: String) = new TypeEnvEntry(t)
}

object TypeChecker {
  def checkProgram(p: String) = {
    val defs = TypedSllParser.parseDefs(p)
    defs.collect({case d:FDef => d; case d:GDef => d})
        .foreach(d => checkFunDef(d, EnvUtils.makeTypeEnv(defs)))
  }
  
  def checkFunDef(d: FunDef, typeEnv: Map[String, TypeEnvEntry]) = d match {
    case FDef(fName, params, body) => {
      if (params.length != typeEnv(fName).dom.length) 
        throw new ArityMismatchException(fName, typeEnv(fName).dom.length, params.length)
      val locals = params.zip(typeEnv(fName).dom).map({case (v, t) => (v.name, TypeEnvEntry(t))})
      val actualFunType = typeOf(body, typeEnv ++ locals) 
      val expectedFunType = typeEnv(fName).returnType
      if (actualFunType != expectedFunType) throw new TypeMismatchException(expectedFunType, actualFunType)
      }
    
    case GDef(gName, pat, params, body) => {
      if (params.length +1 != typeEnv(gName).dom.length) 
        throw new ArityMismatchException(gName, typeEnv(gName).dom.length, params.length + 1)
      
      val actualPatType = typeEnv(pat.name).returnType
      val expectedPatType = typeEnv(gName).dom.head
      if (actualPatType != expectedPatType) throw new TypeMismatchException(expectedPatType, actualPatType)
      
      val variables = pat.args ++ params
      val types = typeEnv(pat.name).dom ++ typeEnv(gName).dom
      val locals = variables.zip(types).map({case (v, t) => (v.name, TypeEnvEntry(t))})

      val actualFunType = typeOf(body, typeEnv ++ locals)
      val expectedFunType = typeEnv(gName).returnType
      if (actualFunType != expectedFunType) throw new TypeMismatchException(expectedFunType, actualFunType)
      }
  }
  
  def typeOf(e: Expr, typeEnv: Map[String, TypeEnvEntry]): String = e match {
    case FCall(fName, fArgs) => {
      fArgs.zip(typeEnv(fName).dom).foreach((checkType(typeEnv) _).tupled)
      typeEnv(fName).returnType
    }        
    case Ctor(cName, cArgs) => { 
      cArgs.zip(typeEnv(cName).dom).foreach((checkType(typeEnv) _).tupled)
      typeEnv(cName).returnType
      }
    case Var(vName) => typeEnv(vName).returnType
  }
  
  def checkType(typeEnv: Map[String, TypeEnvEntry])(e: Expr, expectedType: String) = {
    val actualType = typeOf(e, typeEnv) 
	if (actualType != expectedType) throw new TypeMismatchException(expectedType, actualType) 
  }  
}

object EnvUtils {
  
  def makeTypeEnv(defs: List[Definition]): Map[String, TypeEnvEntry] = {
    val tDefs = defs.collect({case d:SllType => d })
    checkTypeDefs(tDefs)
    val ctorEnv = (for (SllType(n, ctors) <- tDefs) yield ctorEnvEntries(n, ctors, tDefs)).flatten
    val funTypes = defs.collect({case d:FunType => d })
    checkFunTypes(tDefs, funTypes)
    val funEnv = funTypes.map({case FunType(name, dom, retType) => (name, TypeEnvEntry(dom, retType))})
    Map(funEnv ++ ctorEnv: _*)
    }
  
  def ctorEnvEntries(t: String, cs:List[TypeCtor], tDefs: List[SllType]) =
    cs.map(c => (c.name, TypeEnvEntry(c.typeArgs, t)))
  
  def checkTypeDefs(ds: List[SllType]) = {
    ds.foreach({case SllType(name, ctors) => ctors.map(c => c.typeArgs.map(resolveType(ds)))}) 
    }
  
  def resolveType(tDefs: List[SllType])(typeName: String) = {
    if (!tDefs.exists(_.name == typeName)) throw new NoSuchTypeException(typeName)
    }
  
  def checkFunTypes(typeDefs: List[SllType], fts: List[FunType]) = 
    fts.foreach(ft => {
      ft.localVarTypes.foreach(resolveType(typeDefs))
      resolveType(typeDefs)(ft.returnType)
      })
}

class NoSuchTypeException(t: String) extends Exception(s"Undefined type ${t}")
class TypeMismatchException(expected: String, actual: String)
       extends Exception(s"Type mismatch: expected ${expected}, given ${actual}")
class ArityMismatchException(name: String, inTypeDecl: Int, inFunDecl: Int)
       extends Exception(s"Arity mismatch for ${name}. In type declaration: ${inTypeDecl} arguments, in function declaration: ${inFunDecl}")