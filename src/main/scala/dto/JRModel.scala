package edu.fudan.selab
package dto

import org.eclipse.jdt.core.dom.{BodyDeclaration, ChildPropertyDescriptor, Javadoc}

class JRModel(
              val decl: BodyDeclaration
             ) {
  var records: Array[Record] = Array.empty
  val javadoc: Javadoc = decl.getJavadoc
  val javadocProps: ChildPropertyDescriptor = decl.getJavadocProperty
}
