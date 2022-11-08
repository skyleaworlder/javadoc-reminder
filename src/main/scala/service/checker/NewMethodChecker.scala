package edu.fudan.selab
package service.checker
import dto.{JavadocNewMethodRecord, Record}

import edu.fudan.selab.service.checker.NewMethodChecker.makeRecord
import edu.fudan.selab.util.JDTUtil
import edu.fudan.selab.util.format.JDTMethodFormatter
import org.eclipse.jdt.core.dom.MethodDeclaration

class NewMethodChecker extends Checker {
  override def process(stream: CheckerStream): Array[Record] =
    stream.diffStore.insertMethodMap.map(elem => {
      val shortSig = JDTMethodFormatter.getShortMethodSig(elem._2)
      makeRecord(elem._2, s"$shortSig is a new method, should pay more attention to it")
    }).toArray
}

object NewMethodChecker {
  def makeRecord(md: MethodDeclaration, msg: String): Record =
    new JavadocNewMethodRecord(JDTMethodFormatter.getFullyMethodSig(md), JDTUtil.getMethodDeclLineNo(md), msg)
}
