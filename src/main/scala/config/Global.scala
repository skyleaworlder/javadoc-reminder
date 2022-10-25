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

  // method name -> overload methods
  var NEW_OVERLOAD_MAP: Map[String, Array[String]] = null

  // call-graph
  var OLD_CG: CallGraph = null

  // class name -> SootClass
  var SOOT_CLASS_MAP: Map[String, SootClass] = Map.empty

  // array version
  var SOOT_CLASSES: Array[SootClass] = Array.empty

  // method name -> SootMethod
  var SOOT_METHOD_MAP: Map[String, SootMethod] = Map.empty

  // array version
  var SOOT_METHODS: Array[SootMethod] = Array.empty

  // class name -> td
  var CLASS_DECL_MAP: Map[String, TypeDeclaration] = Map.empty

  // array version
  var CLASS_DECLS: Array[TypeDeclaration] = Array.empty

  // method sig -> md
  var METHOD_DECL_MAP: Map[String, MethodDeclaration] = Map.empty

  // array version
  var METHOD_DECLS: Array[MethodDeclaration] = Array.empty

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

        if td.getMethods.length > 0 then putClassDeclMap(fullyClassName, td)
        else Global.LOG.warn(s"${fullyClassName} has 0 method")

        td.getMethods.foreach(md => {
          val fullyMethodName = MethodUtil.getFullyMethodSig(
            packageName, className, MethodUtil.getShortMethodSig(md))
          putMethodDeclMap(MethodUtil.fixInit(fullyMethodName), md)
        })
      }) })

    // class -> methods map
    val methods = METHOD_DECL_MAP.keySet.toArray
    val classMethodMap = ClassUtil.makeClassMethodMap(methods)
    setClassMethodMap(m = classMethodMap.toMap)

    // method overload information
    NEW_OVERLOAD_MAP = MethodUtil.getOverloadMethodMap(METHOD_DECL_MAP.keySet.toArray)

    // old version files contain diff with new version
    val helper = new GitHelper(projPath, tmpPath)
    NEW_OLD_FILES_MAP = helper.initNonCommittedEnv()

    // call-graph
    SootUtil.prepare(jarPath)
    OLD_CG = SootUtil.getCallGraph(CLASS_DECL_MAP.keySet.toArray)
    // hierarchy
    // must be called after Soot prepare
    NEW_HIERARCHY = Scene.v().getActiveHierarchy

    // array version
    SOOT_CLASSES = SOOT_CLASS_MAP.values.toArray
    SOOT_METHODS = SOOT_METHOD_MAP.values.toArray
    CLASS_DECLS = CLASS_DECL_MAP.values.toArray
    METHOD_DECLS = METHOD_DECL_MAP.values.toArray

    println("Initialization finished")

  private def setClassMethodMap(m: Map[String, util.List[String]]): Unit =
    NEW_CLASSMETHOD_MAP = Map.from(m)

  private def putClassDeclMap(fullyClassName: String, td: TypeDeclaration): Unit =
    CLASS_DECL_MAP += (fullyClassName, td)

  private def putMethodDeclMap(methodFullySig: String, md: MethodDeclaration): Unit =
    METHOD_DECL_MAP += (methodFullySig, md)
}
