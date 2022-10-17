package edu.fudan.selab
package service

import com.github.gumtreediff.actions.{EditScript, EditScriptGenerator, SimplifiedChawatheScriptGenerator}
import com.github.gumtreediff.actions.model.Action
import com.github.gumtreediff.client.Run
import com.github.gumtreediff.gen.TreeGenerators
import com.github.gumtreediff.matchers.{MappingStore, Matcher, Matchers}
import com.github.gumtreediff.tree.{Tree, TreeContext}
import edu.fudan.selab.util.GumTreeUtil

import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

/**
 * differ
 * @param oldFilePath src in mappings
 *                    (因为是看 old 的代码如何变成当前状态，
 *                    看的是过去到当前的变化，看的是当前相较于过去做了什么修改，old (或 HEAD) 如何变成当前 new (或 working) 状态
 *                    因此过去为 src，当前为 dst）
 * @param newFilePath dst in mappings
 */
class GumTreeDiffer(
                    private val oldFilePath: String,
                    private val newFilePath: String
                   ) {
  Run.initGenerators()
  val oldTC: TreeContext = TreeGenerators.getInstance().getTree(oldFilePath)
  val newTC: TreeContext = TreeGenerators.getInstance().getTree(newFilePath)
  val oldTree: Tree = oldTC.getRoot
  val newTree: Tree = newTC.getRoot

  val matcher: Matcher = Matchers.getInstance().getMatcher
  val mappings: MappingStore = matcher.`match`(oldTree, newTree)

  val esGenerator: EditScriptGenerator = new SimplifiedChawatheScriptGenerator()
  val actions: EditScript = esGenerator.computeActions(mappings)

  def belongToOldTree(action: Action): Boolean = GumTreeUtil.isChildOf(oldTree, action.getNode)

  def belongToNewTree(action: Action): Boolean = GumTreeUtil.isChildOf(newTree, action.getNode)
}
