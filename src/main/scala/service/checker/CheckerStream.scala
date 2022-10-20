package edu.fudan.selab
package service.checker

import dto.{BodyType, Record, RecordType, WorkingDirDiffStore}

import org.eclipse.jdt.core.dom.{MethodDeclaration, TypeDeclaration}
import soot.SootMethod
import soot.jimple.toolkits.callgraph.CallGraph

import scala.collection.mutable

class CheckerStream(
                    val diffStore: WorkingDirDiffStore,
                    val methodOverloadMap: Map[String, Array[String]],
                    val classDeclMap: Map[String, TypeDeclaration],
                    val methodDeclMap: Map[String, MethodDeclaration],
                    val sootMethodMap: Map[String, SootMethod],
                    val callgraph: CallGraph
                   ) {
  // contains checkers' result
  var tdRecords = mutable.Map.empty[String, mutable.Map[RecordType, Record]]
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
            tdRecords(record.sig).addOne(record.t, record)
          case BodyType.METHOD =>
            methodRecords(record.sig).addOne(record.t, record)
      })
    })
}
