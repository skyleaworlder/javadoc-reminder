package edu.fudan.selab
package util.format

import com.github.gumtreediff.tree.Tree
import edu.fudan.selab.config.Global
import edu.fudan.selab.util.GumTreeUtil.{ARRAY_TYPE, CLASS_OR_INTERFACE_TYPE, PARAMETER, PRIMITIVE_TYPE, getFullyClassName, getMethodDeclNodeFromDown, getMethodName, getPackageName}

import scala.jdk.CollectionConverters.*

object GumTreeMethodFormatter {
  /**
   * format primitive type
   *
   * @param tree
   * @return label of SimpleType is its param name
   */
  def formatPrimitiveType(tree: Tree): Option[String] =
    if tree == null then None
    else Some(tree.getLabel)

  /**
   * format class or interface type
   * 1. List<SootMethod> => List
   * 2. SootMethod => SootMethod
   *
   * @param tree
   * @return label of SimpleType is its param name
   */
  def formatClassOrInterfaceType(tree: Tree): Option[String] =
    val children = tree.getChildren.asScala.toArray
    // TODO: need to check if generic is always SimpleName
    // when children's size >= 2, it means generic parameter, like List<SootMethod>, return List
    val child = children(0)
    Some(child.getLabel)

  /**
   * format ArrayType
   * 1. ArrayType(ArrayType(ClassOrInterfaceType like String)) => String[][]
   * 2. ArrayType(Primitive like int) => int[][]
   *
   * @param tree
   * @return
   */
  def formatArrayType(tree: Tree): Option[String] =
    val children = tree.getChildren.asScala.toArray
    if children.length != 1 then
      Global.LOG.warn(s"GumTreeUtil.formatArrayType error: $tree ArrayType should only have 1 child")
      return None
    end if
    val child = children(0)
    child.getType.name match
      case ARRAY_TYPE => GumTreeMethodFormatter.formatArrayType(child) match
        case Some(arrayType) => Some(s"$arrayType[]")
        case None => None
      case CLASS_OR_INTERFACE_TYPE => GumTreeMethodFormatter.formatClassOrInterfaceType(child) match
        case Some(classOrInterfaceType) => Some(s"$classOrInterfaceType[]")
        case None => None
      case PRIMITIVE_TYPE => GumTreeMethodFormatter.formatPrimitiveType(child) match
        case Some(primitiveType) => Some(s"$primitiveType[]")
        case None => None

  /**
   * format a parameter (tree.getType.name is Parameter):
   * 1. String type => String
   * 2. int[][] a => int[][]
   *
   * @param tree
   * @return
   */
  def formatParam(tree: Tree): Option[String] =
    val children = tree.getChildren.asScala.toArray
    val typeNodes = children.filter(node => {
      val t = node.getType.name
      t.equals(ARRAY_TYPE) || t.equals(CLASS_OR_INTERFACE_TYPE) || t.equals(PRIMITIVE_TYPE)
    })
    if typeNodes.length != 1 then
      Global.LOG.warn(s"GumTreeUtil.formatParam error: $tree has ${typeNodes.length} children instead of only 1")
      return None
    end if

    val paramType: Tree = typeNodes(0)
    paramType.getType.name match
      case ARRAY_TYPE => GumTreeMethodFormatter.formatArrayType(paramType)
      case CLASS_OR_INTERFACE_TYPE => GumTreeMethodFormatter.formatClassOrInterfaceType(paramType)
      case PRIMITIVE_TYPE => GumTreeMethodFormatter.formatPrimitiveType(paramType)
      case _ => None

  /**
   * get params by method decl node
   *
   * @param tree
   * @return format as "Type,Type,Type"
   */
  def getParams(tree: Tree): Option[String] =
    val formatParams = tree.getChildren.asScala
      .filter(child => child.getType.name.equals(PARAMETER))
      .map(paramNode => GumTreeMethodFormatter.formatParam(paramNode))
      .toArray
    if formatParams.isEmpty then Some("")
    else if formatParams.contains(None) then None
    else Some(formatParams
      .map(oFormatParam => oFormatParam.get)
      .reduce((prev, curr) => s"$prev,$curr"))

  /**
   * get a packageName.className.methodName of a given node
   *
   * @param tree
   * @return Some => node in a method; None => node isn't in any method
   */
  def getFullyQualifiedMethodName(tree: Tree): Option[String] =
    val methodName = getMethodDeclNodeFromDown(tree) match
      case Some(methodDeclNode) => getMethodName(methodDeclNode)
      case None => null
    val className = getFullyClassName(tree).orNull
    val packageName = getPackageName(tree).orNull
    val params = GumTreeMethodFormatter.getParams(tree).orNull
    if methodName != null && className != null && packageName != null && params != null then
      Some(s"$packageName.$className.$methodName($params)")
    else None

  /**
   * get packageName.className of a given node
   * @param tree
   * @return
   */
  def getPackageClassName(tree: Tree): Option[String] =
    val packageName = getPackageName(tree).orNull
    val className = getFullyClassName(tree).orNull
    if className != null && packageName != null then
      Some(s"$packageName.$className")
    else None
}
