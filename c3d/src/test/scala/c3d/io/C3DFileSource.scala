package c3d.io

import java.io.File
import scala.collection.immutable._
import org.scalatest.Assertions
import c3d.io.collection.ImmutableArray

trait C3DFileSource { self: Assertions =>

  private def cancelWithName(fileName: String): Nothing = {
    cancel(s"${fileName} not found.  You must run the fetch-c3d-example-files SBT task.")
  }

  private def fetchOrCancel(fileName: String): ImmutableArray[Byte] =
    FileUtils.fileToImmutableArray(new File(fileName)) getOrElse { cancelWithName(fileName) }

  object Sample01 {
    def EB015PI: ImmutableArray[Byte] = fetchOrCancel("./c3d.org-example-files/sample01/Eb015pi.c3d")
    def EB015PR: ImmutableArray[Byte] = fetchOrCancel("./c3d.org-example-files/sample01/Eb015pr.c3d")
    def EB015SI: ImmutableArray[Byte] = fetchOrCancel("./c3d.org-example-files/sample01/Eb015si.c3d")
    def EB015SR: ImmutableArray[Byte] = fetchOrCancel("./c3d.org-example-files/sample01/Eb015sr.c3d")
    def EB015VI: ImmutableArray[Byte] = fetchOrCancel("./c3d.org-example-files/sample01/Eb015vi.c3d")
    def EB015VR: ImmutableArray[Byte] = fetchOrCancel("./c3d.org-example-files/sample01/Eb015vr.c3d")
  }
  
  object Sample08 {
    def EB015PI: ImmutableArray[Byte] = fetchOrCancel("./c3d.org-example-files/sample08/EB015PI.c3d")
  }

}
