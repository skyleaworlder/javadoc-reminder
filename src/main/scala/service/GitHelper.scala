package edu.fudan.selab
package service

import edu.fudan.selab.config.Global
import edu.fudan.selab.util.GitUtil
import edu.fudan.selab.util.file.{PathUtil, RWUtil}
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.blame.BlameResult
import org.eclipse.jgit.diff.DiffEntry
import org.eclipse.jgit.errors.{IncorrectObjectTypeException, MissingObjectException}
import org.eclipse.jgit.lib.{ObjectId, Ref, Repository, RepositoryBuilder}
import org.eclipse.jgit.revwalk.{RevCommit, RevTree, RevWalk}
import org.eclipse.jgit.treewalk.TreeWalk
import org.eclipse.osgi.framework.util.FilePath

import java.io.{File, IOException, InputStream}
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.language.postfixOps
import scala.util.Using

class GitHelper(
                val repoPath: String,
                private val tmpFileDir: String
               ) {
  val repository: Repository = RepositoryBuilder()
    .setGitDir(new File(repoPath + File.separator + ".git"))
    .readEnvironment()
    .findGitDir()
    .build()

  val git = new Git(repository)
  val util = new GitUtil(git)

  /**
   * init environment for non-commit <-> HEAD compare
   */
  def initNonCommittedEnv(): Map[Path, Path] =
    val HEAD = util.getHeadRef
    util.javaSourceDiff.map(entry => {
      val oldPath = entry.getOldPath
      entry.getChangeType match
        // MODIFY | DELETE: copy old version file to target path
        // newPath == oldPath for MODIFY
        case DiffEntry.ChangeType.MODIFY | DiffEntry.ChangeType.DELETE =>
          val oldAbsPath: Path = Paths.get(repoPath + File.separator + oldPath)
          val newAbsPath: Path = extractFileToPath(filePath = oldPath, revId = HEAD.getObjectId.getName,
            targetPath = s"$tmpFileDir/${HEAD.getObjectId.getName}/$oldPath")
          (oldAbsPath, newAbsPath)
        // ADD: don't need to copy old version file (no old version)
        // just analyze new version content
        // use .filter(null !=) to remove ADD impact
        case _ => null
    }).filter(null !=)
      .toMap[Path, Path]

  /**
   * init environment for 2 commits compare (perhaps never use)
   * @param oldCommitId
   * @param newCommitId
   */
  def initTwoCommitEnv(oldCommitId: String, newCommitId: String): Unit =
    val oldCommit = getRevCommit(getObjectId(oldCommitId))
    val newCommit = getRevCommit(getObjectId(newCommitId))
    util.diffTwoCommit(oldCommit, newCommit).foreach(entry => {
      val oldPath = entry.getOldPath
      val newPath = entry.getNewPath
      entry.getChangeType match
        case DiffEntry.ChangeType.MODIFY =>
          extractFileToPath(filePath = oldPath, revId = oldCommitId,
            targetPath = s"$tmpFileDir/$oldCommitId/$oldPath")
          extractFileToPath(filePath = newPath, revId = newCommitId,
            targetPath = s"$tmpFileDir/$newCommitId/$newPath")
        case DiffEntry.ChangeType.DELETE =>
          extractFileToPath(filePath = oldPath, revId = oldCommitId,
            targetPath = s"$tmpFileDir/$oldCommitId/$oldPath")
        case DiffEntry.ChangeType.ADD =>
          extractFileToPath(filePath = newPath, revId = newCommitId,
            targetPath = s"$tmpFileDir/$newCommitId/$newPath")
        case _ => {}
    })

  /**
   * extract file to a given path
   * @param filePath file path in project(relative)
   * @param targetPath given path(absolute), where extracted file gonna be copied
   * @param revId
   * @return
   */
  def extractFileToPath(filePath: String, targetPath: String, revId: String): Path =
    val content: Array[Byte] = extractFileContent(filePath, revId)
    val path: Path = Paths.get(targetPath)
    if !path.toFile.exists() then
      val absPath = path.toFile.getAbsolutePath
      val dirEndPos = absPath.lastIndexOf(File.separator)
      val dirPath = absPath.substring(0, dirEndPos)
      Files.createDirectories(Paths.get(dirPath))
    Files.write(path, content)
    path

  def extractFileContent(filePath: String, revId: String): Array[Byte] =
    val objId = getObjectId(revId)
    if objId == null then
      Global.LOG.warn(s"objectId with revId ${revId} not existed")
      return Array.empty[Byte]
    extractFileContent(filePath, objId)

  def extractFileContent(filePath: String, objId: ObjectId): Array[Byte] =
    val commit = getRevCommit(objId)
    extractFileContent(filePath, commit)

  def extractFileContent(filePath: String, commit: RevCommit): Array[Byte] =
    val tree: RevTree = commit.getTree
    val treeWalk: TreeWalk = TreeWalk.forPath(repository, filePath, tree)
    val id: ObjectId = treeWalk.getObjectId(0)
    val ins: InputStream = RWUtil.open(blobId = id, repository)
    RWUtil.getContent(ins)

  def getObjectId(objIdStr: String): ObjectId =
    repository.resolve(objIdStr)

  /**
   * parse ObjectId to RevCommit using RevWalk
   * MissingObjectException => object not existed
   * IncorrectObjectTypeException => object isn't commit object (maybe a file object)
   * @param objId
   * @return
   */
  def getRevCommit(objId: ObjectId): RevCommit =
    var walk: RevWalk = null
    var commit: RevCommit = null
    try {
      walk = new RevWalk(repository)
      commit = walk.parseCommit(objId)
      if commit == null then Global.LOG.warn(s"commit with object ${objId.getName} not existed")
    } catch {
      case e: MissingObjectException =>
        Global.LOG.warn(s"GitHelper.extractFileContent ${objId.getName} failed: object missing")
        e.printStackTrace()
      case e: IncorrectObjectTypeException =>
        Global.LOG.warn(s"GitHelper.extractFileContent ${objId.getName} failed: object isn't commit")
        e.printStackTrace()
      case e: IOException =>
        e.printStackTrace()
    } finally { if walk != null then walk.close() }
    commit

  /**
   * get blame result by absolute path
   * @param filePath abs path
   * @return
   */
  def blameAbsPath(filePath: String): BlameResult =
    val relativePath = PathUtil.absPathToPathInGit(filePath, repoPath)
    util.blame(relativePath)
}
