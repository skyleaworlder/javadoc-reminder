package edu.fudan.selab
package util.file

import config.Global

object PathUtil {
  def winToUnix(path: String): String = path.replace("\\", "/")

  def toRelative(path: String, repoPath: String): String = path.stripPrefix(repoPath)

  def absPathToPathInGit(path: String, repoPath: String): String =
    if path.startsWith(repoPath) then
      // <proj-path>\\src\\main\\java\\Main.java => src/main/java/Main.java
      winToUnix(toRelative(path, repoPath).stripPrefix("\\"))
    else
      Global.LOG.warn(s"PathUtil.absPathToPathInGit error: " +
        s"$path cannot be translated to relative path in git")
      ""
}
