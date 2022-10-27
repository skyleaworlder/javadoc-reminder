package edu.fudan.selab
package service.checker

import dto.{BodyType, JRMethod, JavadocIncompletenessRecord, Record, RecordType}

import edu.fudan.selab.service.checker.JavadocChecker.{getAllParamTagElements, getFragments, getValidTags, makeRecord}
import org.eclipse.jdt.core.dom.{Javadoc, MethodDeclaration, SimpleName, SingleVariableDeclaration, TagElement, TextElement, TypeDeclaration}
import soot.SootMethod

import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

class JavadocChecker extends Checker {
  val classJavadocMap = Map.empty[String, Javadoc]
  val methodJavadocMap = Map.empty[String, Javadoc]

  override def process(stream: CheckerStream): Array[Record] =
    val noJavadocCheckRecords = noJavadocCheck(stream)
    val paramCompletenessCheckRecords = paramCompletenessCheck(stream)
    Array.empty[Record]
      .appendedAll(noJavadocCheck(stream))
      .appendedAll(paramCompletenessCheck(stream))

  /**
   * check all methods in stream if it doesn't have javadoc
   * @param stream
   * @return records
   */
  private def noJavadocCheck(stream: CheckerStream): Array[Record] =
    stream.jrMethods
      .filter(elem => elem._2.javadoc == null)
      .map(elem => makeRecord(elem._2, "no javadoc"))
      .toArray

  /**
   * check parameters' javadoc description completeness of JRMethod
   * @param stream
   * @return
   */
  private def paramCompletenessCheck(stream: CheckerStream): Array[Record] =
    stream.jrMethods
      .filter(elem => elem._2.javadoc != null)
      .map(elem => paramCompletenessSingleCheck(elem._2))
      .reduce((prev, curr) => prev.appendedAll(curr))

  /**
   * check parameters' javadoc description completeness of JRMethod
   * @param m
   * @return record whose description is not completed
   */
  private def paramCompletenessSingleCheck(m: JRMethod): Array[Record] =
    val paramNames = m.getParamNames
    val validTags = getValidTags(getAllParamTagElements(m.javadoc))
    // "@param node" => tagNames contains "node"
    val tagNames: Array[String] = validTags.map(tag => tag.fragments().get(0).asInstanceOf[SimpleName].toString)
    paramNames
      .filter(paramName => !tagNames.contains(paramName))
      .map(paramName => makeRecord(m, s"param $paramName has no description"))
}

object JavadocChecker {
  /**
   * make a Record using JRMethod
   * @param m
   * @param msg
   * @return
   */
  def makeRecord(m: JRMethod, msg: String): Record =
    new JavadocIncompletenessRecord(m.sootMethod.getSignature, m.getLineNo, msg)

  /**
   * get all tags in javadoc
   * @param javadoc
   * @return
   */
  def getAllParamTagElements(javadoc: Javadoc): Array[TagElement] =
    var tagElements: Array[TagElement] = Array.empty
    val tags = javadoc.tags()
    val iter = tags.iterator()
    while iter.hasNext do
      val elem = iter.next().asInstanceOf[TagElement]
      tagElements +:= elem
    end while
    tagElements

  /**
   * get all fragments (text element) of a tag
   * @param tag
   * @return
   */
  def getFragments(tag: TagElement): Array[TextElement] =
    var textElements: Array[TextElement] = Array.empty
    val fragments = tag.fragments()
    val iter = fragments.iterator()
    while iter.hasNext do
      val fragment = iter.next().asInstanceOf[TextElement]
      textElements +:= fragment
    end while
    textElements

  /**
   * remove all tagelement without “ootionalTagName”
   * tag fragments size must be larger than 1 (SimpleName and TextElement)
   *
   * @param tags
   * @return
   */
  def getValidTags(tags: Array[TagElement]): Array[TagElement] =
    tags
      .filter(tag => tag.getTagName != null)
      .filter(tag => tag.fragments().size() > 1)
      .filter(tag => tag.fragments().get(0).isInstanceOf[SimpleName])
}
