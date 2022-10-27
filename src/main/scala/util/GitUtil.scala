package edu.fudan.selab
package util

import edu.fudan.selab.util.file.PathUtil
import org.eclipse.jgit.api.{BlameCommand, Git}
import org.eclipse.jgit.blame.BlameResult
import org.eclipse.jgit.diff.{DiffEntry, DiffFormatter, RawTextComparator}
import org.eclipse.jgit.diff.DiffEntry.ChangeType
import org.eclipse.jgit.lib.{ObjectReader, Ref, Repository}
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.treewalk.CanonicalTreeParser
import org.eclipse.jgit.util.io.DisabledOutputStream

import java.util
import scala.jdk.CollectionConverters.*

class GitUtil(private val git: Git) {
  /**
   * get HEAD ref, might be null?
   * @return
   */
  def getHeadRef: Ref = git.getRepository.findRef("HEAD")

  def diff: Array[DiffEntry] = git.diff().call().asScala.toArray

  def javaSourceDiff: Array[DiffEntry] =
    diff.filter(filterJavaSource)

  def filterJavaSource(entry: DiffEntry): Boolean =
    val oldPath = entry.getOldPath
    val newPath = entry.getNewPath
    entry.getChangeType match
      case ChangeType.ADD => newPath.endsWith(".java")
      case ChangeType.DELETE => oldPath.endsWith(".java")
      case ChangeType.MODIFY =>
        newPath.endsWith(".java") && oldPath.endsWith(".java")
      case _ => false

  /**
   * pass in 2 commit, get all diff entries
   * @param oldCommit
   * @param newCommit
   * @return
   */
  def diffTwoCommit(oldCommit: RevCommit, newCommit: RevCommit): Array[DiffEntry] =
    val reader: ObjectReader = git.getRepository.newObjectReader()
    val oldId = oldCommit.getTree.getId
    val newId = newCommit.getTree.getId
    val oldTreeIter = new CanonicalTreeParser()
    val newTreeIter = new CanonicalTreeParser()

    val diffFormatter = new DiffFormatter(DisabledOutputStream.INSTANCE)
    diffFormatter.setRepository(git.getRepository)
    oldTreeIter.reset(reader, oldId)
    newTreeIter.reset(reader, newId)
    diffFormatter.scan(oldTreeIter, newTreeIter).asScala.toArray

  /**
   * git blame
   * @param relativePath
   * @return
   */
  def blame(relativePath: String): BlameResult =
    git.blame()
      .setTextComparator(RawTextComparator.WS_IGNORE_ALL)
      .setFilePath(relativePath)
      .setStartCommit(git.getRepository.resolve("HEAD"))
      .call()
}
