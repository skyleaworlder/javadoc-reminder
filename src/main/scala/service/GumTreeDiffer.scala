package edu.fudan.selab
package service

import com.github.gumtreediff.actions.{EditScript, EditScriptGenerator, SimplifiedChawatheScriptGenerator}
import com.github.gumtreediff.client.Run
import com.github.gumtreediff.gen.TreeGenerators
import com.github.gumtreediff.matchers.{MappingStore, Matcher, Matchers}
import com.github.gumtreediff.tree.{Tree, TreeContext}

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
}
