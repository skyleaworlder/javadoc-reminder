package edu.fudan.selab
package util.file

import util.JDTUtil

import org.eclipse.jdt.core.dom.CompilationUnit
import org.eclipse.jgit.util.StringUtils

import java.io.File
import scala.util.matching.Regex
import scala.language.postfixOps
import scalaz.Scalaz.*

object RetrieveUtil {
  // about entrypoint
  /**
   * get all entry points(non-private method) of a given folder
   * @param path
   * @return
   */
  def getAllEntryPointsOfProject(path: String, excludeTest: Boolean = true): Array[String] =
    getAllEntryPointsOfProject(new File(path), excludeTest)

  /**
   * get all entry points(non-private method) of a given folder
   * @param path
   * @return
   */
  def getAllEntryPointsOfProject(path: File, excludeTest: Boolean): Array[String] =
    val neededJavaFiles = {
      if excludeTest then getAllJavaFiles(path.getPath).filter(pruneJavaTestFile)
      else getAllJavaFiles(path.getPath)
    }
    neededJavaFiles |> getAllEntryPoints

  /**
   * get all entry points(non-private method) of given files
   * @param files java files array
   * @return
   */
  def getAllEntryPoints(files: Array[File]): Array[String] =
    files.map(JDTUtil.getCompilationUnit)
      .map(JDTUtil.getNonPrivateMethodName)
      .reduce((prev: Array[String], curr: Array[String]) => prev.appendedAll(curr))

  // load file to get cu
  /**
   * get all java source files under path
   * ^     \\w    \\.java  $
   * beg any char  . java end
   * @param path
   * @return
   */
  def getAllJavaFiles(path: String, excludeTest: Boolean = true): Array[File] =
    getAllJavaFiles(new File(path), excludeTest)

  /**
   * get all java source files under path
   * ^     \\w    \\.java  $
   * beg any char  . java end
   * @param path
   * @return
   */
  def getAllJavaFiles(path: File, excludeTest: Boolean): Array[File] =
    val res = getAllFilesMatchPattern(path, regex = "^\\w+\\.java$".r)
    if excludeTest then res.filter(pruneJavaTestFile) else res

  /**
   * if file name contains "Test.java" => true
   * else if file is under "src/test" => true
   * e.g. GlobalTest.java
   * e.g. src/test/java/edu/fdu/se/callgraph/entryScan/FileUtils.java
   * @param javaFilePath
   * @return
   */
  def pruneJavaTestFile(javaFilePath: String): Boolean =
    new File(javaFilePath) |> pruneJavaTestFile

  /**
   * Usage: .filter(pruneJavaTestFile)
   *
   * if file name contains "Test.java" => false
   * else if file is under "src/test" => false
   * e.g. GlobalTest.java
   * e.g. src/test/java/edu/fdu/se/callgraph/entryScan/FileUtils.java
   * @param javaFile
   * @return
   */
  def pruneJavaTestFile(javaFile: File): Boolean =
    val fileName: String = javaFile.getName
    if fileName.contains("Test.java") then
      return false
    val absName: String = javaFile.getAbsolutePath
    !absName.contains(s"src${File.separator}test")

  /**
   * get all files satisfy given regex
   * @param path
   * @param regex
   * @return
   */
  def getAllFilesMatchPattern(path: File, regex: Regex): Array[File] =
    if path.isFile then return Array.empty[File]
    var qualifiedFiles = Array.empty[File]
    path.listFiles().foreach(file => {
      if file.isFile then
        regex.findFirstMatchIn(file.getName) match
          case Some(f) => qualifiedFiles = qualifiedFiles :+ file
          case None => {}
      else if file.isDirectory then
        qualifiedFiles = qualifiedFiles.appendedAll(getAllFilesMatchPattern(file, regex))
    })
    qualifiedFiles

  /**
   * get all cu of a given folder or a file
   * File => Array with 1 element
   * Dir => Array with all cu under this Dir
   * @param path
   * @return
   */
  def getAllCompilationUnit(path: File): Array[CompilationUnit] =
    if path.isFile then Array[CompilationUnit](JDTUtil.getCompilationUnit(path))
    else getAllJavaFiles(path.getPath).map(JDTUtil.getCompilationUnit)

}
