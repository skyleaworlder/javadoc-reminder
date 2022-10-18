package edu.fudan.selab
package visitor

import org.eclipse.jdt.core.dom.{ASTVisitor, CompilationUnit, EnumDeclaration, TypeDeclaration}

class DeclarationVisitor extends ASTVisitor {
  var cu: CompilationUnit = null
  var types = Array.empty[TypeDeclaration]
  var enums = Array.empty[EnumDeclaration]


  override def visit(node: CompilationUnit): Boolean =
    cu = node
    super.visit(node)

  override def visit(node: TypeDeclaration): Boolean =
    if node.isMemberTypeDeclaration || node.isPackageMemberTypeDeclaration then
      types = types :+ node
    end if
    super.visit(node)

  override def visit(node: EnumDeclaration): Boolean =
    if node.isMemberTypeDeclaration || node.isPackageMemberTypeDeclaration then
      enums = enums :+ node
    end if
    super.visit(node)
}
