package edu.fudan.selab
package dto

import com.github.gumtreediff.actions.model.Action
import com.github.gumtreediff.tree.Tree
import edu.fudan.selab.util.GumTreeUtil

import java.io.File
import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

abstract class DiffStore(
  // old files and new files path
  var oldToNew: Map[String, String] = Map.empty,
  var newToOld: Map[String, String] = Map.empty,

  // file path -> File
  var fileMap: Map[String, FileDiffMetadata] = Map.empty
)


/**
 * OLD: old version / HEAD
 * NEW: new version / Working Directory
 */
enum FileDiffType {
  case OLD
  case NEW
}


/**
 * if file is oldFile; actions: {Move, Update, Delete, DeleteTree}
 * if file is newFile; actions: {Insert, InsertTree}
 * @param file
 * @param root
 * @param actions
 */
class FileDiffMetadata(
                        val t: FileDiffType,
                        val file: File,
                        val root: Tree,
                        val actions: Array[Action]
                      ) {
  def getAllMethod: Array[String] =
    actions.filter(action => GumTreeUtil.isUnderAnyMethodDecl(action.getNode))
      .map(action => GumTreeUtil.getMethodDeclNodeFromDown(action.getNode))
      .filter(None !=).map(oMethodDecl => oMethodDecl.get)
      .map(methodDecl => GumTreeUtil.getFullyQualifiedMethodName(methodDecl))
      .filter(None !=).map(oName => oName.get)
      .distinct
}


/**
 * git diff [old-version] [new-version]
 * @param oldDir RWUtil created
 * @param newDir RWUtil created
 */
class TwoCommitDiffStore(
                          val oldDir: String,
                          val newDir: String,
                          oldToNew: Map[String, String],
                          newToOld: Map[String, String],
                          fileMap: Map[String, FileDiffMetadata]
                        ) extends DiffStore(oldToNew, newToOld, fileMap)


/**
 * git diff
 *
 * WorkingDirDiffStore is just an alias of TwoCommitDiffStore
 * @param workingDir working directory, contains un-staging change
 * @param tmpDir RWUtil created, to store HEAD version files
 */
class WorkingDirDiffStore(
                          val workingDir: String,
                          val tmpDir: String,
                          oldToNew: Map[String, String],
                          newToOld: Map[String, String],
                          fileMap: Map[String, FileDiffMetadata]
                         ) extends TwoCommitDiffStore(workingDir, tmpDir, oldToNew, newToOld, fileMap)
