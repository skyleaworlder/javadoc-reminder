package edu.fudan.selab
package service.checker

import dto.{JRMethod, JavadocCallGraphImpactRecord, Record}

import edu.fudan.selab.config.Global
import edu.fudan.selab.service.checker.CallGraphChecker.makeRecord
import edu.fudan.selab.util.format.{JDTMethodFormatter, SootMethodFormatter}
import edu.fudan.selab.util.JDTUtil
import org.eclipse.jdt.core.dom.MethodDeclaration
import soot.SootMethod

import scala.collection.immutable.HashSet

class CallGraphChecker extends Checker {
  override def process(stream: CheckerStream): Array[Record] =
    var records: Array[Record] = Array.empty
    val diff = stream.diffStore.fileMap
    val jrms = stream.jrMethods

    var checkedMethodName: Set[String] = new HashSet()
    diff.foreach(diffEntry => {
      val fileName = diffEntry._1
      val diffMethodNames = diffEntry._2.getAllMethod
      diffMethodNames.foreach(diffMethodName => {
        jrms.get(diffMethodName) match
          // None means method not existed in CallGraph, a new method or one method's signature has been changed
          // IGNORE IT!!! (because new method gonna be called, then modified check part works,
          //               if not, it is a dead method)
          case None => {}
          case Some(jrm) =>
            val diffMethodName = SootMethodFormatter.getNameWithParams(jrm.sootMethod)
            records :+= makeRecord(jrm.sootMethod, s"method ($diffMethodName) has modified, javadoc might need to update")
            checkedMethodName += diffMethodName
            jrm.caller.foreach(sm => {
              val callerName = SootMethodFormatter.getNameWithParams(sm)
              if !checkedMethodName.contains(callerName) then
                records :+= makeRecord(sm, s"method ($callerName) calls $diffMethodName, javadoc might need to update")
                checkedMethodName += callerName
              end if
            })
      })
    })
    records
}

object CallGraphChecker {
  /**
   * make a Record
   * @param m
   * @param msg
   * @return
   */
  def makeRecord(sm: SootMethod, msg: String): Record =
    new JavadocCallGraphImpactRecord(sm.getSignature, sm.getJavaSourceStartLineNumber, msg)

  //def makeRecord(md: MethodDeclaration, msg: String): Record =
  //  new JavadocCallGraphImpactRecord(MethodUtil.getFullyMethodSig(md), JDTUtil.getMethodDeclLineNo(md), msg)
}
