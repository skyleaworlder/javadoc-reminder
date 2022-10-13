package edu.fudan.selab
package util

import org.eclipse.jdt.core.dom.{ASTVisitor, EnumDeclaration, TypeDeclaration}

class DeclarationVisitor extends ASTVisitor {
  var types = Array.empty[TypeDeclaration]
  var enums = Array.empty[EnumDeclaration]

  override def visit(node: TypeDeclaration): Boolean =
    if node.isMemberTypeDeclaration || node.isPackageMemberTypeDeclaration then
      types = types :+ node
    end if
    true

  override def visit(node: EnumDeclaration): Boolean =
    if node.isMemberTypeDeclaration || node.isPackageMemberTypeDeclaration then
      enums = enums :+ node
    end if
    true
}
