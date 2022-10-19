package edu.fudan.selab
package dto

import edu.fudan.selab.config.Global
import org.eclipse.jdt.core.dom.{ChildPropertyDescriptor, Javadoc, MethodDeclaration}
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
              ) {
  // jdt
  // javadoc
  val javadoc: Javadoc = methodDecl.getJavadoc
  val javadocProps: ChildPropertyDescriptor = methodDecl.getJavadocProperty

  // soot
  // caller: who call this method;
  // callee: which is called by this method.
  var caller: Array[SootMethod] = Global.NEW_CG.edgesOutOf(sootMethod).asScala.toArray
    .filter(e => e.src() == sootMethod)
    .map(e => e.tgt())
  var callee: Array[SootMethod] = Global.NEW_CG.edgesInto(sootMethod).asScala.toArray
    .filter(e => e.tgt() == sootMethod)
    .map(e => e.src())
}
