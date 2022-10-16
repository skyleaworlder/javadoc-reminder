package edu.fudan.selab
package util.file

import util.JDTUtil

import org.eclipse.jdt.core.dom.CompilationUnit

import java.io.File
import scala.util.matching.Regex
import scala.language.postfixOps
import scalaz.Scalaz.*

object RetrieveUtil {
  /**
   * get all entry points(non-private method) of a given folder
   * @param path
   * @return
   */
  def getAllEntryPointsOfProject(path: String): Array[String] =
    new File(path) |> getAllEntryPointsOfProject

  /**
   * get all entry points(non-private method) of a given folder
   * @param path
   * @return
   */
  def getAllEntryPointsOfProject(path: File): Array[String] =
    path |> getAllJavaFiles |> getAllEntryPoints

  /**
   * get all entry points(non-private method) of given files
   * @param files java files array
   * @return
   */
  def getAllEntryPoints(files: Array[File]): Array[String] =
    files.map(JDTUtil.getCompilationUnit)
      .map(JDTUtil.getNonPrivateMethodName)
      .reduce((prev: Array[String], curr: Array[String]) => prev.appendedAll(curr))

  /**
   * get all java source files under path
   * ^     \\w    \\.java  $
   * beg any char  . java end
   * @param path
   * @return
   */
  def getAllJavaFiles(path: String): Array[File] =
    new File(path) |> getAllJavaFiles

  /**
   * get all java source files under path
   * ^     \\w    \\.java  $
   * beg any char  . java end
   * @param path
   * @return
   */
  def getAllJavaFiles(path: File): Array[File] =
    getAllFilesMatchPattern(path, regex = "^\\w+\\.java$".r)

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
    else getAllJavaFiles(path).map(JDTUtil.getCompilationUnit)

}