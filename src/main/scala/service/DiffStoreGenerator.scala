package edu.fudan.selab
package service

import com.github.gumtreediff.actions.model.Action
import com.github.gumtreediff.tree.Tree
import edu.fudan.selab.dto.{FileDiffMetadata, FileDiffType, TwoCommitDiffStore, WorkingDirDiffStore}

import java.io.File
import scala.jdk.CollectionConverters.*

object DiffStoreGenerator {
  def makeWorkingDirDiffStore(
                              workingDir: String,
                              tmpDir: String,
                              newToOldPath: Map[String, String]
                             ): WorkingDirDiffStore =
    val store = makeTwoCommitDiffStore(workingDir, tmpDir, newToOldPath)
    new WorkingDirDiffStore(
      store.newDir, store.oldDir, store.oldToNew, store.newToOld, store.fileMap)

  def makeTwoCommitDiffStore(
                              newDir: String,
                              oldDir: String,
                              newToOldPath: Map[String, String]
                            ): TwoCommitDiffStore =
    // create old file path <-> new file path
    var oldToNew: Map[String, String] = Map.empty
    var newToOld: Map[String, String] = Map.empty
    newToOldPath.foreach((newPath, oldPath) => {
      oldToNew += (oldPath -> newPath)
      newToOld += (newPath -> oldPath)
    })
    // make file diff metadata
    var fileMap: Map[String, FileDiffMetadata] = Map.empty
    oldToNew.foreach((oldFilePath, newFilePath) => {
      val differ = new GumTreeDiffer(oldFilePath, newFilePath)
      val oldTreeActions = differ.actions.asScala.toArray.filter(differ.belongToOldTree)
      val newTreeActions = differ.actions.asScala.toArray.filter(differ.belongToNewTree)
      val oldMetadata = makeFileDiffMetadata(
        FileDiffType.OLD, oldFilePath, differ.oldTree, oldTreeActions)
      val newMetadata = makeFileDiffMetadata(
        FileDiffType.NEW, newFilePath, differ.newTree, newTreeActions)
      fileMap += (oldFilePath -> oldMetadata)
      fileMap += (newFilePath -> newMetadata)
    })
    new TwoCommitDiffStore(oldDir, newDir, oldToNew, newToOld, fileMap)

  def makeFileDiffMetadata(
                            t: FileDiffType,
                            filePath: String,
                            root: Tree,
                            actions: Array[Action]
                          ): FileDiffMetadata =
    new FileDiffMetadata(t, new File(filePath), root, actions)
}
