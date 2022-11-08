import edu.fudan.selab.config.Global
import edu.fudan.selab.dto.JRClass
import edu.fudan.selab.service.DiffStoreGenerator
import edu.fudan.selab.service.checker.{CallGraphChecker, Checker, CheckerStream, ClassHierarchyChecker, JavadocChecker, NewMethodChecker}
import edu.fudan.selab.util.format.SootMethodFormatter

@main
def main(projPath: String, tmpPath: String, jarPath: String*): Unit = {
  Global.init(jarPath.toArray, projPath, tmpPath)

  val diffStore = DiffStoreGenerator.makeWorkingDirDiffStore(projPath, tmpPath,
    Global.NEW_OLD_FILES_MAP.map((n, o) => (n.toString, o.toString)))

  val jrClasses = Global.CLASS_DECL_MAP.map(elem => {
    val (sig, td) = elem
    val sm = Global.SOOT_CLASS_MAP(sig)
    new JRClass(td, sm)
  }).map(jrc => jrc.getName -> jrc).toMap

  val jrMethods = jrClasses.map(elem => {
    val className = elem._1
    val methods = elem._2.methods
    methods.map(jrm => {
      SootMethodFormatter.getFullyQualifiedName(className, jrm.sootMethod) -> jrm
    })
  }).reduce((prev, curr) => prev.appendedAll(curr))
    .toMap

  val stream = new CheckerStream(
    diffStore, jrClasses, jrMethods, Global.OLD_CG, Global.NEW_HIERARCHY)

  stream
    //.use(new JavadocChecker())
    .use(new CallGraphChecker())
    .use(new NewMethodChecker())
    .use(new ClassHierarchyChecker())

  stream.perform()

  stream.classRecords.foreach(r => {
    r._2.foreach(e => println(e._2))
  })
  stream.methodRecords.foreach(r => {
    r._2.foreach(e => println(e._2))
  })
}
