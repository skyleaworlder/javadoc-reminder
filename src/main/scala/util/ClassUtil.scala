package edu.fudan.selab
package util

import org.eclipse.jdt.core.dom.{ASTNode, AbstractTypeDeclaration, MethodDeclaration}
import soot.SootClass

import java.util
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

/**
 * thanks Dr.Huang
 */
object ClassUtil {
  def getFullyClassName(
                         packageName: String,
                         className: String
                       ): String = s"${packageName}.${className}"

  /**
   * get class name by td (not including packageName)
   * @param typeDeclaration
   * @return
   */
  def getClassName(typeDeclaration: AbstractTypeDeclaration): String =
    var sb = new StringBuilder()
    var td = typeDeclaration
    while true do
      val className = td.getName.toString()
      sb.insert(0, className + "$")
      if td.isPackageMemberTypeDeclaration then
        sb = new StringBuilder(sb.substring(0, sb.length() - 1))
        return sb.toString()
      end if
      if !td.getParent.isInstanceOf[AbstractTypeDeclaration] then return null
      td = td.getParent.asInstanceOf[AbstractTypeDeclaration]
    end while
    sb.toString()

  /**
   * get a type decl through a given method
   * if not found, return null
   *
   * infinite loop warning
   * @param md
   * @return
   */
  def getTypeDecl(md: MethodDeclaration): AbstractTypeDeclaration =
    var current: ASTNode = md
    while true do
      current = current.getParent
      if current == null then
        return null
      if current.isInstanceOf[AbstractTypeDeclaration] then
        return current.asInstanceOf[AbstractTypeDeclaration]
    end while
    current.asInstanceOf[AbstractTypeDeclaration] // fake exit stmt

  /**
   * use methods (package+class+fully method) as input,
   * return a map (key: class name; val: package+class+fully method)
   * @param methods
   * @return a map: class -> method
   */
  def makeClassMethodMap(methods: Array[String]): mutable.Map[String, util.List[String]] =
    val m = new mutable.HashMap[String, util.List[String]]()
    for method <- methods do
      breakable {
        if !method.contains('(') then break
        val end = method.indexOf('(')
        val nameWithoutParams = method.substring(0, end)
        val beg = nameWithoutParams.lastIndexOf('.')
        val className = nameWithoutParams.substring(0, beg)
        if !m.contains(className) then
          m.put(className, new util.ArrayList[String]())
        m.get(className) match
          case Some(elem: util.List[String]) => elem.add(method)
          case None => { }
      }
    end for
    m
}
