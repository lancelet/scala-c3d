package c3d.io

import java.io.File
import scala.collection.immutable._
import org.scalatest.Assertions

trait C3DFileSource { self: Assertions =>

  private def cancelWithName(fileName: String): Nothing = {
    cancel(s"${fileName} not found.  You must run the fetch-c3d-example-files SBT task.")
  }

  private def fetchOrCancel(fileName: String): IndexedSeq[Byte] =
    FileUtils.fileToIndexedSeq(new File(fileName)) getOrElse { cancelWithName(fileName) }

  object Sample08 {
    def EB015PI: IndexedSeq[Byte] = fetchOrCancel("./c3d.org-example-files/sample08/EB015PI.c3d")
  }

}
