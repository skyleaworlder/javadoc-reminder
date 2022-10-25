package edu.fudan.selab
package dto

import edu.fudan.selab.config.Global
import edu.fudan.selab.util.{JDTUtil, MethodTransfer, MethodUtil}
import org.eclipse.jdt.core.dom.{ChildPropertyDescriptor, CompilationUnit, Javadoc, MethodDeclaration, SingleVariableDeclaration}
import soot.SootMethod
import soot.jimple.toolkits.callgraph.Edge

import scala.jdk.CollectionConverters.*

/**
 * jdt decl & soot method abstraction
 * @param methodDecl
 * @param sootMethod shouldn't be null in principle.
 *                   if null, caller and callee must be empty
 */
class JRMethod(
                val methodDecl: MethodDeclaration,
                val sootMethod: SootMethod
              ) extends JRModel(methodDecl) {
  // soot
  // caller: who call this method;
  // callee: which is called by this method.
  var caller: Array[SootMethod] = Global.NEW_CG.edgesOutOf(sootMethod).asScala.toArray
    .filter(e => e.src() == sootMethod)
    .map(e => e.tgt())
  var callee: Array[SootMethod] = Global.NEW_CG.edgesInto(sootMethod).asScala.toArray
    .filter(e => e.tgt() == sootMethod)
    .map(e => e.src())

  /**
   * format: package.class.method(params)
   * @return
   */
  override def getName: String =
    MethodTransfer.getNameWithParams(sootMethod)

  /**
   * get parameter's names
   * @return
   */
  def getParamNames: Array[String] =
    var names: Array[String] = Array.empty
    val params = methodDecl.parameters()
    val iter = params.iterator()
    while (iter.hasNext) {
      val param = iter.next().asInstanceOf[SingleVariableDeclaration]
      val paramName = param.getName.toString
      names = names :+ paramName
    }
    names

  /**
   * get line number of method decl
   * @return
   */
  override def getLineNo: Int = JDTUtil.getMethodDeclLineNo(methodDecl)
}
