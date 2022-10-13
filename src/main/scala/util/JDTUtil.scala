package edu.fudan.selab
package util

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
   * get all non-private methods' name as entry points of call-graph
   * @param cu
   * @return
   */
  def getNonPrivateMethodName(cu: CompilationUnit): Array[String] = {
    val packageName = cu.getPackage.getName.toString
    val visitor = new DeclarationVisitor()
    cu.accept(visitor)

    visitor.types.map(t => {
      val className = ClassUtil.getClassName(t)
      t.getMethods.map(m => {
        val methodName = MethodUtil.getShortMethodSig(m)
        MethodUtil.getFullyMethodSig(packageName, className, methodName)
      })
    }).reduce((prev: Array[String], curr: Array[String]) => prev.appendedAll(curr))
  }
}