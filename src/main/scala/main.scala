import edu.fudan.selab.config.Global
import edu.fudan.selab.dto.JRClass
import edu.fudan.selab.service.DiffStoreGenerator
import edu.fudan.selab.service.checker.{Checker, CheckerStream, JavadocChecker}

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

  val jrMethods = jrClasses.map(elem => elem._2.methods)
    .reduce((prev, curr) => prev.appendedAll(curr))
    .map(jrm => jrm.getName -> jrm).toMap

  val stream = new CheckerStream(
    diffStore, jrClasses, jrMethods, Global.OLD_CG)

  stream
    .use(new JavadocChecker())

  stream.perform()
  println(stream.methodRecords)
}
