package edu.fudan.selab
package util

import java.io.File

import scala.util.matching.Regex

object FileUtil {
  /**
   * get all entry points(non-private method) of a given folder
   * @param path
   * @return
   */
  def getAllEntryPointsOfProject(path: String): Array[String] =
    getAllEntryPointsOfProject(new File(path))

  /**
   * get all entry points(non-private method) of a given folder
   * @param path
   * @return
   */
  def getAllEntryPointsOfProject(path: File): Array[String] =
    val files = getAllJavaFiles(path)
    files.map(file => {
      val cu = JDTUtil.getCompilationUnit(file)
      val nonPrivateMethodName = JDTUtil.getNonPrivateMethodName(cu)
      nonPrivateMethodName
    }).reduce((prev: Array[String], curr: Array[String]) => prev.appendedAll(curr))

  /**
   * get all java source files under path
   * ^     \\w    \\.java  $
   * beg any char  . java end
   * @param path
   * @return
   */
  def getAllJavaFiles(path: File): Array[File] =
    getAllFiles(path, regex = "^\\w+\\.java$".r)

  def getAllFiles(path: File, regex: Regex): Array[File] =
    var qualifiedFiles = Array.empty[File]
    if path.isFile then return Array.empty[File]

    path.listFiles().foreach(file => {
      if file.isFile then
        regex.findFirstMatchIn(file.getName) match
          case Some(f) => qualifiedFiles = qualifiedFiles :+ file
          case None => {}
      else if file.isDirectory then
        qualifiedFiles = qualifiedFiles.appendedAll(getAllFiles(file, regex))
    })
    qualifiedFiles
}
