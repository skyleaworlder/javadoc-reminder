package edu.fudan.selab
package util

import edu.fudan.selab.config.Global
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.core.dom.{AST, ASTParser, CompilationUnit}

import java.io.{BufferedReader, File, FileInputStream, FileReader}
import scala.io.Source
import scala.util.Using

object JDTUtil {
  /**
   * get cu by file path
   * @param filePath
   * @return
   */
  def getCompilationUnit(filePath: String): CompilationUnit = {
    getCompilationUnit(new File(filePath))
  }

  /**
   * get cu by File
   * @param file
   * @return
   */
  def getCompilationUnit(file: File): CompilationUnit = {
    var cu: CompilationUnit = null
    Using(Source.fromFile(file)) { fd =>
      val source = fd.mkString
      cu = getCompilationUnit(source.toCharArray)
    }
    cu
  }

  /**
   * get cu
   * @param source
   * @return
   */
  private def getCompilationUnit(source: Array[Char]): CompilationUnit = {
    val astParser = ASTParser.newParser(AST.JLS8)
    astParser.setKind(ASTParser.K_COMPILATION_UNIT)

    val options = JavaCore.getOptions
    options.put(JavaCore.COMPILER_COMPLIANCE, JavaCore.VERSION_1_8)
    options.put(JavaCore.COMPILER_CODEGEN_TARGET_PLATFORM, JavaCore.VERSION_1_8)
    options.put(JavaCore.COMPILER_SOURCE, JavaCore.VERSION_1_8)
    options.put(JavaCore.COMPILER_DOC_COMMENT_SUPPORT, JavaCore.ENABLED)

    astParser.setCompilerOptions(options)
    astParser.setSource(source)
    astParser.createAST(null).asInstanceOf[CompilationUnit]
  }

  /**
   * judge if a compilation unit has package definition
   * (some java source file don't have "package A.B.C;")
   * @param cu
   * @return true: has; false: doesn't have
   */
  def isCuHasPackageDecl(cu: CompilationUnit): Boolean =
    if cu.getPackage != null then true
    else { Global.LOG.warn("cu has no package name"); false }

  /**
   * visit cu to get enough metadata of source file
   * including types and enums (now only types used)
   * @param cu
   * @return visitor contains types and enums
   */
  def visitCu(cu: CompilationUnit): DeclarationVisitor =
    val visitor = new DeclarationVisitor()
    cu.accept(visitor)
    visitor

  /**
   * judge if a java sourcec file has type declaration
   * (some source file only define annotation)
   * TODO: Interface need taken into account
   * @param visitor
   * @return true: has; false: doesn't have
   */
  def isCuHasTypeDecl(visitor: DeclarationVisitor): Boolean =
    if !visitor.types.isEmpty then true
    else { Global.LOG.warn("cu has 0 types. (maybe annotation declaration)"); false }

  /**
   * get all non-private methods' name as entry points of call-graph
   * @param cu
   * @return
   */
  def getNonPrivateMethodName(cu: CompilationUnit): Array[String] = {
    val packageDecl = cu.getPackage
    if packageDecl == null then
      Global.LOG.warn("cu has no package name")
      return Array.empty[String]

    val packageName = packageDecl.getName.toString
    val visitor = new DeclarationVisitor()
    cu.accept(visitor)

    if visitor.types.isEmpty then
      Global.LOG.warn("cu has 0 types. (maybe annotation declaration)")
      return Array.empty[String]

    val r1 = visitor.types.map(t => {
      val className = ClassUtil.getClassName(t)
      t.getMethods.map(m => {
        val methodName = MethodUtil.getShortMethodSig(m)
        MethodUtil.getFullyMethodSig(packageName, className, methodName)
      })
    }).filter(elem => !elem.isEmpty)

    if r1.isEmpty then
      Global.LOG.warn("type declarations in cu have no method")
      return Array.empty[String]

    val r2 = r1.reduce((prev: Array[String], curr: Array[String]) => prev.appendedAll(curr))
    r2
  }
}
