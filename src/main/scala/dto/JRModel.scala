package edu.fudan.selab
package dto

import org.eclipse.jdt.core.dom.{BodyDeclaration, ChildPropertyDescriptor, Javadoc}

abstract class JRModel(
                        val decl: BodyDeclaration
                      ) {
  var records: Array[Record] = Array.empty
  val javadoc: Javadoc = decl.getJavadoc

  /**
   * get model's name
   * @return
   */
  def getName: String = ""

  /**
   * get line number of model
   * @return
   */
  def getLineNo: Int
}
