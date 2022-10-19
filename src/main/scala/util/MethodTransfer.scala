package edu.fudan.selab
package util

import soot.{SootMethod, Type}

import scala.jdk.CollectionConverters.*

object MethodTransfer {
  /**
   * get name by SootMethod
   * @param m
   * @return
   */
  def getNameWithParams(m: SootMethod): String =
    val simpleName = m.getName
    val params = getParams(m)
    s"$simpleName($params)"

  /**
   * get params part by SootMethod
   * @param m
   * @return => "String,Integer,String[]"
   */
  def getParams(m: SootMethod): String =
    if m.getParameterCount == 0 then return ""
    m.getParameterTypes.asScala
      .map(getNameOfType)
      .reduce((prev: String, curr: String) => s"$prev,$curr")

  /**
   * get the class name (last part, excluding package name)
   *
   * @param t soot.Type
   * @return edu.fdu.se.callgraph.Main => Main
   *         java.lang.String[] => String[]
   */
  def getNameOfType(t: Type): String =
    val fullyTypeName = t.toString
    val beg = fullyTypeName.lastIndexOf(".")
    fullyTypeName.substring(beg + 1)
}
