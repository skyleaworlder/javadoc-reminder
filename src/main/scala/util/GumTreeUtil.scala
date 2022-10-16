package edu.fudan.selab
package util

import com.github.gumtreediff.tree.Tree
import edu.fudan.selab.config.Global

import scala.jdk.CollectionConverters.*

object GumTreeUtil {
  val CLASS_OR_INTERFACE_DECLARATION = "ClassOrInterfaceDeclaration"
  val METHOD_DECLARATION = "MethodDeclaration"
  val PACKAGE_DECLARATION = "PackageDeclaration"
  val QUALIFIED_NAME = "QualifiedName"
  val SIMPLE_NAME = "SimpleName"

  /**
   * get a packageName.className.methodName of a given node
   * @param tree
   * @return Some => node in a method; None => node isn't in any method
   */
  def getFullyQualifiedMethodName(tree: Tree): Option[String] =
    val methodName = getMethodDeclNodeFromDown(tree) match
      case Some(methodDeclNode) => getMethodName(methodDeclNode)
      case None => null
    val className = getFullyClassName(tree).orNull
    val packageName = getPackageName(tree).orNull
    if methodName != null && className != null && packageName != null then
      Some(s"$packageName.$className.$methodName")
    else None

  /**
   * get root node of a given tree node
   * @param tree
   * @return
   */
  def getRoot(tree: Tree): Option[Tree] = tree.getParents.asScala.lastOption

  /**
   * get package qualified name by any tree node
   * @param tree
   * @return Some => package A.B.C; None => this file has no package decl
   */
  def getPackageName(tree: Tree): Option[String] =
    val pkgDecls = getRoot(tree) match
      case Some(root) => root.getDescendants.asScala.toArray
        .filter(node => node.getType.name.equals(PACKAGE_DECLARATION))
      case None => Array.empty[Tree]
    if pkgDecls.nonEmpty then getQualifiedNameOfPackageDecl(pkgDecls.head)
    else None

  /**
   * get package name by package decl
   * @param packageDecl
   * @return
   */
  def getQualifiedNameOfPackageDecl(packageDecl: Tree): Option[String] =
    val nameNode = packageDecl.getChildren.asScala.toArray
      .filter(node => node.getType.equals(QUALIFIED_NAME))
    if nameNode.nonEmpty then Some(nameNode.head.getLabel)
    else None

  /**
   * get a fully class name of given node
   * e.g. class A { class B { C } }: getFullyClassName(C) => A.B
   * @param tree
   * @return
   */
  def getFullyClassName(tree: Tree): Option[String] =
    val outerClasses = tree.getParents.asScala
      .filter(node => node.getType.name.equals(CLASS_OR_INTERFACE_DECLARATION))
      .reverse
    if outerClasses.nonEmpty then
      Some(outerClasses.map(getClassName)
        .reduce((prev: String, curr: String) => s"$prev$$$curr")
        + (if !tree.getType.name.equals(CLASS_OR_INTERFACE_DECLARATION) then ""
          else  "." + getClassName(tree)))
    else None

  /**
   * get all method declaration node under root node
   * @param root
   * @return
   */
  def getAllMethodUnderRoot(root: Tree): Array[Tree] =
    root.getDescendants.asScala.toArray
      .filter(node => node.getType.name.equals(METHOD_DECLARATION))

  /**
   * get name of class (SimpleName of ClassOrInterfaceDeclaration)
   * @param clsDeclNode
   * @return
   */
  def getClassName(clsDeclNode: Tree): String =
    if !clsDeclNode.getType.name.equals(CLASS_OR_INTERFACE_DECLARATION) then
      Global.LOG.warn("GumTreeUtil.getClassName param is not ClassOrInterfaceDeclaration node")
      return null
    val nameNode = clsDeclNode.getChild(SIMPLE_NAME)
    if nameNode != null then nameNode.getLabel
    else null

  /**
   * get name of method (SimpleName of MethodDeclaration)
   * @param mDeclNode
   * @return
   */
  def getMethodName(mDeclNode: Tree): String =
    if !mDeclNode.getType.name.equals(METHOD_DECLARATION) then
      Global.LOG.warn("GumTreeUtil.getMethodName param is not MethodDeclaration node")
      return null
    val nameNode = mDeclNode.getChildren.asScala
      .filter(node => node.getType.name.equals(SIMPLE_NAME))
    if nameNode.nonEmpty then nameNode.head.getLabel
    else null

  /**
   * get method decl node of a node:
   * 1. param is already a method decl node
   * 2. param is a node under a method decl node
   * 3. param is not under any method decl node
   * @param tree
   * @return Some(Tree) => method decl; None => tree isn't in any method block
   */
  def getMethodDeclNodeFromDown(tree: Tree): Option[Tree] =
    if tree.getType.name.equals(METHOD_DECLARATION) then return Some(tree)
    val parents = tree.getParents
    val methodDeclNodes = parents.asScala.filter(node => node.getType.name.equals(METHOD_DECLARATION))
    methodDeclNodes.headOption
}
