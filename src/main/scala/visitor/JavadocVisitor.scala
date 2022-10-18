package edu.fudan.selab
package visitor

import org.eclipse.jdt.core.dom.{ASTVisitor, Javadoc, MethodDeclaration, SingleVariableDeclaration}

import java.util
import scala.jdk.CollectionConverters.*

/**
 * search cu, accept this visitor to find method declaration
 * get method decl -> method params name
 *     method decl -> Javadoc
 */
class JavadocVisitor extends ASTVisitor {
  var methodParams: Map[MethodDeclaration, Array[String]] = Map.empty
  var methodJavadocs: Map[MethodDeclaration, Javadoc] = Map.empty

  override def visit(node: MethodDeclaration): Boolean =
    // the type of params under MethodDeclaration is always SingleVariableDeclaration
    val params = node.parameters().asInstanceOf[util.List[SingleVariableDeclaration]]
      .asScala.toArray
    methodParams += (node -> params.map(svd => svd.getName.toString))
    methodJavadocs += (node -> node.getJavadoc)
    super.visit(node)
}
