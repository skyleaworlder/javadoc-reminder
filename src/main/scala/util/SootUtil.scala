package edu.fudan.selab
package util

import edu.fudan.selab.config.Global
import edu.fudan.selab.util.format.SootMethodFormatter
import soot.{G, Scene, SootClass, SootMethod}
import soot.jimple.toolkits.callgraph.{CHATransformer, CallGraph}
import soot.options.Options

import java.util
import scala.collection.mutable
import scala.collection.mutable.Map
import scala.jdk.CollectionConverters.*

object SootUtil {
  val libs: String = System.getProperty("user.dir") + "\\lib\\rt.jar"

  def prepare(jarPaths: Array[String]): Unit = {
    G.reset()

    Options.v().set_soot_classpath(libs)
    Options.v().set_process_dir(jarPaths.toList.asJava)
    Options.v().set_src_prec(Options.src_prec_java)
    Options.v().set_whole_program(true) // inter-procedural analysis
    Options.v().set_allow_phantom_refs(true)
    Options.v().set_verbose(true)
    Options.v().set_keep_line_number(true)
    Options.v().setPhaseOption("jb", "use-original-names:true")
    Options.v().setPhaseOption("cg", "all-reachable:true")
    Options.v().set_no_bodies_for_excluded(true)
    Options.v().set_app(true)
  }

  /**
   * getCallGraph must be called after prepare invoked !!!
   * @param classes
   * @return
   */
  def getCallGraph(classes: Array[String]): CallGraph =
    assert(Options.v().soot_classpath() != "")
    assert(Options.v().process_dir() != null)
    assert(Options.v().whole_program())
    assert(Options.v().allow_phantom_refs())
    assert(Options.v().verbose())

    val entryPoints = loadEntryPoints(classes)
    //println(entryPoints.mkString("Array(", ", ", ")"))
    Scene.v().setEntryPoints(entryPoints.toList.asJava)
    Scene.v().loadNecessaryClasses()
    Scene.v().loadBasicClasses()
    CHATransformer.v().transform()
    Scene.v().getCallGraph

  /**
   * get all public methods of given classes
   * @param classNames
   * @return all public methods as entries
   */
  def loadEntryPoints(classNames: Array[String]): Array[SootMethod] =
    classNames
      .map(loadEntryPoints)
      .reduce((prev: Array[SootMethod], curr: Array[SootMethod]) => prev.appendedAll(curr))

  /**
   * get all public methods of given class
   * @param className
   * @return all methods
   */
  private def loadEntryPoints(className: String): Array[SootMethod] = {
    var entryPoints = Array[SootMethod]()

    val sootClass = Scene.v().loadClassAndSupport(className)
    sootClass.setApplicationClass()
    Scene.v().loadNecessaryClasses()
    Global.SOOT_CLASS_MAP += (sootClass.getName, sootClass)

    sootClass.getMethods.asScala.foreach((method: SootMethod) => {
      Global.LOG.info(s"${SootMethodFormatter.getNameWithParams(method)} -> $method")
      Global.SOOT_METHOD_MAP += (s"${className}.${SootMethodFormatter.getNameWithParams(method)}" -> method)
      if !method.isPrivate then
        try method.retrieveActiveBody()
        catch
          case _: RuntimeException =>
            Global.LOG.info(s"${className}.${method.getName} is an abstract method with empty body")
        entryPoints = entryPoints :+ method
      end if
    })
    entryPoints
  }

  //def recoverGenerics(sootClass: SootClass): SootClass =

}
