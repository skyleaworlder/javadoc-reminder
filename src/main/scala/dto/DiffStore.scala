package edu.fudan.selab
package dto

import com.github.gumtreediff.actions.model.Action
import com.github.gumtreediff.tree.Tree
import edu.fudan.selab.util.{GumTreeUtil, JDTUtil}
import edu.fudan.selab.util.format.{GumTreeMethodFormatter, JDTMethodFormatter}
import edu.fudan.selab.visitor.DeclarationVisitor
import org.eclipse.jdt.core.dom.{CompilationUnit, MethodDeclaration, TypeDeclaration}

import java.io.File
import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

abstract class DiffStore(
  // old files and new files path
  var oldToNew: Map[String, String] = Map.empty,
  var newToOld: Map[String, String] = Map.empty,

  // file path -> File
  var fileMap: Map[String, FileDiffMetadata] = Map.empty
) {
  private val oldFileMap = fileMap.filter(elem => oldToNew.keySet.contains(elem._1))
  private val newFileMap = fileMap.filter(elem => newToOld.keySet.contains(elem._1))

  // old file method name -> decl map
  val oldFileMethodMap: Map[String, MethodDeclaration] = oldFileMap.values
    .map(diffData => diffData.mds)
    .reduce((prev, curr) => prev.appendedAll(curr))
    .map(md => JDTMethodFormatter.getFullyMethodSig(md) -> md)
    .toMap
  // new file method name -> decl map
  val newFileMethodMap: Map[String, MethodDeclaration] = newFileMap.values
    .map(diffData => diffData.mds)
    .reduce((prev, curr) => prev.appendedAll(curr))
    .map(md => JDTMethodFormatter.getFullyMethodSig(md) -> md)
    .toMap

  // created method -> decl map
  val insertMethodMap: Map[String, MethodDeclaration] = newFileMethodMap
    .filter(elem => newFileMethodMap.contains(elem._1) && !oldFileMethodMap.contains(elem._1))
  // deleted method -> decl map
  val deleteMethodMap: Map[String, MethodDeclaration] = oldFileMethodMap
    .filter(elem => oldFileMethodMap.contains(elem._1) && !newFileMethodMap.contains(elem._1))
}


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
  val cu: CompilationUnit = JDTUtil.getCompilationUnit(file)
  private val visitor = new DeclarationVisitor()
  cu.accept(visitor)

  val tds: Array[TypeDeclaration] = visitor.types
  val mds: Array[MethodDeclaration] = tds
    .map(td => td.getMethods)
    .reduce((prev, curr) => prev.appendedAll(curr))

  def getAllClass: Array[String] =
    actions.filter(action => GumTreeUtil.isUnderAnyClassOrInterfaceDecl(action.getNode))
      .map(action => GumTreeMethodFormatter.getPackageClassName(action.getNode))
      .filter(None !=).map(oName => oName.get)
      .distinct

  def getAllMethod: Array[String] =
    actions.filter(action => GumTreeUtil.isUnderAnyMethodDecl(action.getNode))
      .map(action => GumTreeUtil.getMethodDeclNodeFromDown(action.getNode))
      .filter(None !=).map(oMethodDecl => oMethodDecl.get)
      .map(methodDecl => GumTreeMethodFormatter.getFullyQualifiedMethodName(methodDecl))
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
