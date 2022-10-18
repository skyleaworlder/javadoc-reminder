package edu.fudan.selab
package config

import edu.fudan.selab.service.GitHelper
import edu.fudan.selab.util.file.RetrieveUtil
import edu.fudan.selab.util.{ClassUtil, JDTUtil, MethodUtil, SootUtil}
import org.eclipse.jdt.core.dom.{MethodDeclaration, TypeDeclaration}
import org.slf4j.{Logger, LoggerFactory}
import soot.{Hierarchy, Scene, SootClass, SootMethod}
import soot.jimple.toolkits.callgraph.CallGraph

import java.io.File
import java.nio.file.Path
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

  // class name -> SootClass
  var SOOT_CLASS_MAP: Map[String, SootClass] = Map.empty

  // method name -> SootMethod
  var SOOT_METHOD_MAP: Map[String, SootMethod] = Map.empty

  // class name -> td
  var CLASS_DECL_MAP: Map[String, TypeDeclaration] = Map.empty

  // method sig -> md
  var METHOD_DECL_MAP: Map[String, MethodDeclaration] = Map.empty

  // old version files Path which contain diff
  var NEW_OLD_FILES_MAP: Map[Path, Path] = Map.empty

  def init(
            jarPath: Array[String],
            projPath: String,
            tmpPath: String
          ): Unit =
    // class name -> td
    // method name -> td
    RetrieveUtil.getAllJavaFiles(projPath)
      .map(JDTUtil.getCompilationUnit)
      .filter(JDTUtil.isCuHasPackageDecl)
      .map(JDTUtil.visitCu)
      .filter(JDTUtil.isCuHasTypeDecl)
      .foreach(visitor => { visitor.types.foreach(td => {
        val packageName = visitor.cu.getPackage.getName.toString
        val className = ClassUtil.getClassName(td)
        val fullyClassName = ClassUtil.getFullyClassName(packageName, className)

        // TODO: 这样好吗，目前没有考虑 “没有方法的类”
        // TODO: 目前生成 cg 靠的是 CLASS_DECL_MAP，这会有隐患吗？cg 需要的 classes 和这里想表示的是同一个含义吗？
        //  而且目前也是依靠 cg 读取的是 CLASS_DECL_MAP 而非 CLASS_METHOD_MAP，
        //  才使得 METHOD_DECL_MAP 能够使用 fully name 而非 sig 来做 key
        if td.getMethods.length > 0 then putClassDeclMap(fullyClassName, td)
        else Global.LOG.warn(s"${fullyClassName} has 0 method")

        td.getMethods.foreach(md => {
          val fullyMethodName = MethodUtil.getFullyMethodName(
            packageName, className, md.getName.toString)
          putMethodDeclMap(fullyMethodName, md)
        })
      }) })

    // class -> methods map
    val methods = METHOD_DECL_MAP.keySet.toArray
    val classMethodMap = ClassUtil.makeClassMethodMap(methods)
    setClassMethodMap(m = classMethodMap.toMap)
    // old version files contain diff with new version
    val helper = new GitHelper(projPath, tmpPath)
    val res = helper.initNonCommittedEnv()
    res.foreach((newPath, oldPath) => NEW_OLD_FILES_MAP += (newPath -> oldPath))
    // call-graph
    SootUtil.prepare(jarPath)
    NEW_CG = SootUtil.getCallGraph(CLASS_DECL_MAP.keySet.toArray)
    // hierarchy
    // must be called after Soot prepare
    NEW_HIERARCHY = Scene.v().getActiveHierarchy
    println("Initialization finished")

  private def setClassMethodMap(m: Map[String, util.List[String]]): Unit =
    NEW_CLASSMETHOD_MAP = Map.from(m)

  private def putClassDeclMap(fullyClassName: String, td: TypeDeclaration): Unit =
    CLASS_DECL_MAP += (fullyClassName, td)

  private def putMethodDeclMap(methodFullySig: String, md: MethodDeclaration): Unit =
    METHOD_DECL_MAP += (methodFullySig, md)
}
