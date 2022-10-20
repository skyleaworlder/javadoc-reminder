package edu.fudan.selab
package service.checker

import dto.{Record, RecordType}

import edu.fudan.selab.visitor.JavadocVisitor
import org.eclipse.jdt.core.dom.{Javadoc, MethodDeclaration, TypeDeclaration}

class JavadocChecker extends Checker {
  val classJavadocMap = Map.empty[String, Javadoc]
  val methodJavadocMap = Map.empty[String, Javadoc]

  override def process(stream: CheckerStream): Array[Record] =
    val allTd: Array[TypeDeclaration] = stream.classDeclMap.values.toArray

    allTd.foreach(td => {
      val visitor = new JavadocVisitor()
      td.accept(visitor)
    })


    Array.empty[Record]
}

object JavadocChecker {
  val T: RecordType = RecordType.JAVADOC
}
