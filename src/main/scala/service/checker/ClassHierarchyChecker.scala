package edu.fudan.selab
package service.checker

import dto.{ClassHierarchyImpactRecord, JRClass, Record}

import edu.fudan.selab.service.checker.ClassHierarchyChecker.makeRecord

import scala.jdk.CollectionConverters.*
import scala.language.postfixOps
import soot.{Scene, SootClass}

class ClassHierarchyChecker extends Checker {
  /**
   * get all diff classes and their sub-classes
   * @param stream
   * @return
   */
  override def process(stream: CheckerStream): Array[Record] =
    val sc2jrc: Map[SootClass, JRClass] = stream.jrClasses
      .map(elem => elem._2.sootClass -> elem._2)

    val diffClassesName: Array[String] = stream.diffStore.fileMap
      .map(diffEntry => diffEntry._2.getAllClass)
      .reduce((prev, curr) => prev.appendedAll(curr))
      .distinct

    val diffClasses: Array[SootClass] = diffClassesName
      .map(name => Scene.v().getSootClass(name))
      .filter(!_.isPhantomClass)
      .distinct

    val allSubclasses: Array[SootClass] = diffClasses
      .map(sc => stream.hierarchy.getSubclassesOf(sc).asScala.toArray)
      .filter(_.nonEmpty)
      .reduceOption((prev, curr) => prev.appendedAll(curr))
      .getOrElse(Array.empty[SootClass])
      .distinct

    val allClassJrc: Array[JRClass] = diffClasses
      .map(sc2jrc.get)
      .filter(None !=).map(oJrc => oJrc.get)

    val allSubclassJrc: Array[JRClass] = allSubclasses
      .map(sc2jrc.get)
      .filter(None !=).map(oJrc => oJrc.get)

    val baseClassRecords = allClassJrc.map(jrc => makeRecord(jrc.sootClass,
      s"method in class(${jrc.getName}) has been changed, javadoc might need updated"))
    val subclassRecords = allSubclassJrc.map(jrc => makeRecord(jrc.sootClass,
      s"base class change might affect subclasses(${jrc.getName})"))

    baseClassRecords.appendedAll(subclassRecords)
}

object ClassHierarchyChecker {
  def makeRecord(sc: SootClass, msg: String): Record =
    new ClassHierarchyImpactRecord(sc.getName, sc.getJavaSourceStartLineNumber, msg)
}
