package edu.fudan.selab
package util.format

import util.ClassUtil

import org.eclipse.jdt.core.dom.{CompilationUnit, MethodDeclaration, SingleVariableDeclaration}

import java.util.Stack
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

/**
 * thanks Dr.Huang
 */
object JDTMethodFormatter {
  /**
   * packageName.className.methodName(params)
   *
   * @param md
   * @return
   */
  def getFullyMethodSig(md: MethodDeclaration): String =
    val cu = md.getRoot.asInstanceOf[CompilationUnit]
    val packageName = cu.getPackage.getName.toString
    val className = ClassUtil.getClassName(ClassUtil.getTypeDecl(md))
    val shortMethodSig = getShortMethodSig(md)
    getFullyMethodSig(packageName, className, shortMethodSig)

  def getFullyMethodName(
                          packageName: String,
                          className: String,
                          methodName: String
                        ) = s"${packageName}.${className}.${methodName}"

  def getFullyMethodSig(
                         packageName: String,
                         className: String,
                         shortMethodSig: String
                       ) = s"${packageName}.${className}.${shortMethodSig}"

  def getShortMethodSig(m: MethodDeclaration): String =
    val name = m.getName.toString
    val params = getParamsList(m)
    removeBracket(name + "(" + params + ")")

  def getParamsList(m: MethodDeclaration): String =
    val sb = new mutable.StringBuilder()
    val params = m.parameters()
    val iter = params.iterator()
    while (iter.hasNext) {
      val param = iter.next().asInstanceOf[SingleVariableDeclaration]
      val paramFullyType = param.getType.toString
      val paramType = paramFullyType.substring(paramFullyType.lastIndexOf(".") + 1)
      // TODO: now ignore "..."
      sb.append(paramType)
      if iter.hasNext then sb.append(",")
    }
    sb.toString()

  /**
   * replace A.B.C.C(...) => A.B.C.<init>(...)
   *
   * @param methodName package.class.method(params)
   * @return if (class == method) package.class.<init>(params)
   */
  def fixInit(methodName: String): String =
    val parts = methodName.split("\\(")
    if parts.size < 2 then return methodName
    val packageClassMethod = parts.head
    val pcmParts = packageClassMethod.split("\\.")
    if pcmParts.size < 2 then methodName
    else
      val className = pcmParts.apply(pcmParts.size - 2)
      val shortMethodName = pcmParts.last
      if className.equals(shortMethodName) then methodName.replace(s"$shortMethodName(", "<init>(")
      else methodName

  /**
   * replace C(...) => C.<init>(...), if and only if className == C
   * used in JRClass / JRMethod generation. (because method decl doesn't obtain class information)
   *
   * @param className
   * @param methodName
   * @return
   */
  def fixInit(className: String, methodName: String): String =
    val parts = methodName.split("\\(")
    if parts.size < 2 then return methodName
    val shortMethodName = parts.head
    if className.equals(shortMethodName) then methodName.replace(s"$shortMethodName(", "<init>(")
    else methodName

  /**
   * get information about method overload
   *
   * @param methodNames array of method name
   * @return key: only method name, no param; val: fully method sig
   */
  def getOverloadMethodMap(methodNames: Array[String]): Map[String, Array[String]] =
    var m = Map.empty[String, mutable.ArrayBuffer[String]]
    methodNames.foreach(name => {
      val end = name.indexOf("(")
      if end != -1 then
        // get package.class.method name
        val pcm = name.substring(0, end)
        if !m.contains(pcm) then
          m = m.updated(pcm, new ArrayBuffer[String]())
        m(pcm) += name
    })
    m.map(tuple => tuple._1 -> tuple._2.toArray)

  /**
   * remove generic "<" and ">"
   *
   * @param s
   * @return
   */
  def removeBracket(s: String): String =
    val stack = new Stack[Int]()
    val pair = new mutable.HashMap[Int, Int]()

    for i <- 0 until s.length do
      breakable {
        if s.charAt(i) == '<' then
          stack.push(i)
        if s.charAt(i) == '>' then
          if stack.size >= 2 then
            stack.pop()
            break
          if stack.isEmpty then break
          pair.put(stack.peek(), i)
          stack.pop()
        end if
      }
    end for

    val sb = new mutable.StringBuilder()
    var isIgnore = false
    var endIgnoreI = -1
    for i <- 0 until s.length do
      breakable {
        if pair.contains(i) then
          isIgnore = true
          pair.get(i) match
            case Some(res: Int) => endIgnoreI = res
            case None => {}
        end if
        if i == endIgnoreI then
          isIgnore = false
          break
        if isIgnore then break
        sb.append(s.charAt(i))
      }
    end for
    sb.toString()
}
