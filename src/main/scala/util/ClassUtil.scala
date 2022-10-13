package edu.fudan.selab
package util

import org.eclipse.jdt.core.dom.AbstractTypeDeclaration

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

/**
 * thanks Dr.Huang
 */
object ClassUtil {
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
}
