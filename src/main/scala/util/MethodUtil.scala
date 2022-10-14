package edu.fudan.selab
package util

import org.eclipse.jdt.core.dom.{MethodDeclaration, SingleVariableDeclaration}

import java.util.Stack
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

/**
 * thanks Dr.Huang
 */
object MethodUtil {
  def getFullyMethodSig(
                         packageName: String,
                         className: String,
                         methodName: String
                       ) = packageName + "." + className + "." + methodName

  def getShortMethodSig(m: MethodDeclaration): String =
    val name = m.getName.toString()
    val params = getParamsList(m)
    removeBracket(name + "(" + params + ")")

  def getParamsList(m: MethodDeclaration): String =
    val sb = new StringBuilder()
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
   * remove generic "<" and ">"
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
