package edu.fudan.selab
package dto

import edu.fudan.selab.config.Global
import edu.fudan.selab.util.{MethodTransfer, MethodUtil}
import org.eclipse.jdt.core.dom.{CompilationUnit, MethodDeclaration, TypeDeclaration}
import soot.{SootClass, SootMethod}

import java.util
import scala.jdk.CollectionConverters.*

/**
 * jdt decl & soot class abstraction
 * @param classDecl
 * @param sootClass shouldn't be null, type decl here only refers to Class!
 */
class JRClass(
               val classDecl: TypeDeclaration,
               val sootClass: SootClass
             ) extends JRModel(classDecl) {
  val className: String = classDecl.getName.toString
  val parent: SootClass = sootClass.getSuperclass
  val children: Array[SootClass] = Global.NEW_HIERARCHY.getSubclassesOf(sootClass).asScala.toArray

  // check validity of method decls and soot methods
  private var methodsDecl: Array[MethodDeclaration] = classDecl.getMethods
  private var methodsSoot: Array[SootMethod] = sootClass.getMethods.asScala.toArray
  methodsDecl = methodsDecl.filter(md => {
    // check if method decl in Global.METHOD_DECLS
    val res = Global.METHOD_DECLS.contains(md)
    if !res then Global.LOG.warn(
      s"JRClass.init: method(${md.getName}) in td(${classDecl.getName}) not exists in Global.METHOD_DECLS")
    res
  })
  methodsSoot = methodsSoot.filter(sm => {
    // check if method decl in Global.SOOT_METHODS
    val res = Global.SOOT_METHODS.contains(sm)
    if !res then Global.LOG.warn(
      s"JRClass.init: method(${sm.getName}) in sc(${sootClass.getName}) not exists in Global.SOOT_METHODS")
    res
  })

  // simpleName means: no package, no class, only method name and params (<init> fixed, no <clinit>)
  private val simpleNameMdMap: Map[String, MethodDeclaration] = methodsDecl
    .map(md => MethodUtil.fixInit(className, MethodUtil.getShortMethodSig(md)) -> md).toMap
  private val simpleNameSmMap: Map[String, SootMethod] = methodsSoot
    .map(sm => MethodTransfer.getNameWithParams(sm) -> sm).toMap

  // generate JRMethod for each method in class (class decl && soot class)
  val methods: Array[JRMethod] = simpleNameMdMap.map(elem => {
    val (name, md) = elem
    new JRMethod(md, simpleNameSmMap(name))
  }).toArray

  // checker if method decl number equals constructed JRMethod number
  if methods.length != simpleNameMdMap.size then
    Global.LOG.warn("JRClass.init: filtered md's number not equal JRMethod's number")

  /**
   * get name, using SootMethod getName
   * @return
   */
  override def getName: String = sootClass.getName

  /**
   * get line number of class decl
   * @return
   */
  override def getLineNo: Int =
    val cu = classDecl.getRoot.asInstanceOf[CompilationUnit]
    cu.getLineNumber(classDecl.getStartPosition) - 1
}
