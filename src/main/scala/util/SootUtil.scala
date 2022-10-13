package edu.fudan.selab
package util

import soot.{G, Scene, SootClass, SootMethod}
import soot.jimple.toolkits.callgraph.{CHATransformer, CallGraph}
import soot.options.Options

import java.util.ArrayList
import scala.collection.mutable
import scala.collection.mutable.Map
import scala.jdk.CollectionConverters._

object SootUtil {
  val libs = "./lib/rt.jar"

  def prepare(jarPaths: Array[String]) = {
    G.reset()
    var argList = Array[String](
      "-allow-phantom-refs", "-w",
      "-keep-line-number", "-enabled",
      "-cp", libs
    )
    jarPaths.foreach(jarPath => { argList = argList :+ "-process-dir" :+ jarPath })
    argList = argList.appendedAll(Array[String]("-p", "jb", "use-original-names:true"))

    Options.v().parse(argList)
    Options.v().set_src_prec(Options.src_prec_java)
    Options.v().set_whole_program(true) // inter-procedural analysis
    Options.v().set_allow_phantom_refs(true)
    Options.v().set_keep_line_number(true)
    Options.v().set_verbose(true)
    Options.v().setPhaseOption("cg", "all-reachable:true")
    Options.v().set_no_bodies_for_excluded(true)
    Options.v().set_app(true)
  }

  def getCallGraph(jarPath: Array[String], classNames: Array[String]): CallGraph = {
    prepare(jarPath)

    val entryPoints = loadEntryPoints(classNames)
    Scene.v().setEntryPoints(entryPoints.toList.asJava)
    Scene.v().loadNecessaryClasses()
    Scene.v().loadBasicClasses()
    CHATransformer.v().transform()
    Scene.v().getCallGraph
  }

  /**
   * get all public methods of given classes
   * @param classNames
   * @return all public methods as entries
   */
  def loadEntryPoints(classNames: Array[String]): Array[SootMethod] = {
    var entryPoints = Array[SootMethod]()
    classNames.foreach(className => entryPoints = entryPoints.appendedAll(loadEntryPoints(className)))
    entryPoints
  }

  /**
   * get all public methods of given class
   * @param className
   * @return all methods
   */
  private def loadEntryPoints(className: String): Array[SootMethod] = {
    var entryPoints = Array[SootMethod]()

    val sootClass = Scene.v().loadClassAndSupport(className)
    Scene.v().getClasses.forEach(clazz => { println("class is :" + clazz) })
    sootClass.setApplicationClass()
    Scene.v().loadNecessaryClasses()

    val methodsMap = getMethodsMap(sootClass)
    methodsMap.foreach((_, method: SootMethod) => {
      if !method.isPrivate then
        try method.retrieveActiveBody()
        catch
          case re: RuntimeException => print("fuck!"); re.printStackTrace()
        entryPoints = entryPoints :+ method
      end if
    })
    entryPoints
  }

  /**
   * @param className
   * @return key: method's signature; val: SootMethod
   */
  def getMethodsMap(sootClass: SootClass): Map[String, SootMethod] = {
    val m = mutable.Map[String, SootMethod]()
    print(sootClass.getMethods.size())
    sootClass.getMethods.forEach(method => m.addOne(method.getName, method))
    m
  }
}
