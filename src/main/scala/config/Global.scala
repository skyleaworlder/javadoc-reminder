package edu.fudan.selab
package config

import edu.fudan.selab.util.{ClassUtil, FileUtil, SootUtil}
import org.slf4j.{Logger, LoggerFactory}
import soot.{Hierarchy, Scene}
import soot.jimple.toolkits.callgraph.CallGraph

import java.util
import scala.jdk.CollectionConverters.*

object Global {
  val LOG: Logger = LoggerFactory.getLogger("javadoc-reminder")

  // class hierarchy, super-sub structure
  var NEW_HIERARCHY: Hierarchy = null

  // class -> methods
  var NEW_CLASSMETHOD_MAP: Map[String, util.List[String]] = null

  // call-graph
  var NEW_CG: CallGraph = null

  def init(
            jarPath: Array[String],
            projPath: String
          ): Unit =
    // class -> methods map
    val methods = FileUtil.getAllEntryPointsOfProject(projPath)
    val classMethodMap = ClassUtil.makeClassMethodMap(methods)
    setClassMethodMap(m = classMethodMap.toMap)
    // call-graph
    SootUtil.prepare(jarPath)
    NEW_CG = SootUtil.getCallGraph(methods)
    // hierarchy
    // must be called after Soot prepare
    NEW_HIERARCHY = Scene.v().getActiveHierarchy

  private def setClassMethodMap(m: Map[String, util.List[String]]): Unit =
    NEW_CLASSMETHOD_MAP = Map.from(m)
}
