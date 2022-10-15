package edu.fudan.selab
package util.file

import edu.fudan.selab.config.Global
import edu.fudan.selab.util.GitUtil
import org.apache.commons.io.output.ByteArrayOutputStream
import org.eclipse.jgit.errors.MissingObjectException
import org.eclipse.jgit.lib.{Constants, CoreConfig, ObjectId, Repository}
import org.eclipse.jgit.treewalk.WorkingTreeOptions
import org.eclipse.jgit.util.io.AutoCRLFInputStream

import java.io.{ByteArrayInputStream, InputStream, OutputStream}
import java.nio.file.{Path, Paths}
import scala.language.postfixOps

/**
 * Thanks Dr.Huang
 */
object RWUtil {
  /**
   * open a git object, wrap it with InputStream
   * if object not existed, return null
   * @param blobId
   * @param repo
   * @return
   */
  def open(blobId: ObjectId, repo: Repository): InputStream =
    if blobId == null then return new ByteArrayInputStream(Array.empty[Byte])
    try repo.open(blobId, Constants.OBJ_BLOB).openStream()
    catch
      case e: MissingObjectException =>
        Global.LOG.warn(s"RWUtil.open ${blobId.getName} failed: object missing")
        e.printStackTrace()
        null

  /**
   * put all content of ins into Array[Byte]
   * @param ins
   * @return
   */
  def getContent(ins: InputStream): Array[Byte] =
    val bos = new ByteArrayOutputStream()
    copy(ins, bos)
    bos.toByteArray

  /**
   * copy data from InputStream to OutputStream
   * @param ins
   * @param os
   * @return
   * @see https://stackoverflow.com/questions/6927873/how-can-i-read-a-file-to-an-inputstream-then-write-it-into-an-outputstream-in-sc
   */
  private def copy(ins: InputStream, os: OutputStream): Unit =
    Iterator.continually(ins.read)
      .takeWhile(-1 !=)
      .foreach(os.write)
}
