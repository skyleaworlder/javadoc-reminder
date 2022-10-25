package edu.fudan.selab
package service.checker

import dto.{BodyType, JRClass, JRMethod, Record, RecordType, WorkingDirDiffStore}

import org.eclipse.jdt.core.dom.{MethodDeclaration, TypeDeclaration}
import soot.SootMethod
import soot.jimple.toolkits.callgraph.CallGraph

import scala.collection.mutable

class CheckerStream(
                    val diffStore: WorkingDirDiffStore,
                    val jrClasses: Map[String, JRClass],
                    val jrMethods: Map[String, JRMethod],
                    val callgraph: CallGraph
                   ) {
  // contains checkers' result
  var classRecords = mutable.Map.empty[String, mutable.Map[RecordType, Record]]
  var methodRecords = mutable.Map.empty[String, mutable.Map[RecordType, Record]]

  // checkers will be used
  var checkers: Array[Checker] = Array.empty

  /**
   * register checker
   * @param checker
   * @return
   */
  def use(checker: Checker): CheckerStream =
    checkers = checkers.appended(checker)
    this

  /**
   *  use a checker to append record on method decl
   */
  def perform(): Unit =
    checkers.foreach(checker => {
      val records: Array[Record] = checker.check(this)
      records.foreach(record => {
        record.bt match
          case BodyType.CLASS =>
            if !classRecords.contains(record.sig) then
              classRecords.put(record.sig, new mutable.HashMap[RecordType, Record]())
            end if
            classRecords(record.sig).addOne(record.t, record)
          case BodyType.METHOD =>
            if !methodRecords.contains(record.sig) then
              methodRecords.put(record.sig, new mutable.HashMap[RecordType, Record]())
            end if
            methodRecords(record.sig).addOne(record.t, record)
      })
    })
}
