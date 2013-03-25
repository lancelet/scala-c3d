package c3d.iobase

import java.io.File

/**
 * C3D example files from c3d.org.
 *
 * These files should be downloaded using the fetch-c3d-example-files sbt task.
 */
object C3DExampleFiles {

  private def getAndRequireFileExists(fileName: String): File = {
    val file = new File(fileName)
    require(file.exists(), s"File '${fileName}' does not exist.  Did you run the fetch-c3d-example-files sbt task?")
    file
  }

  object Sample08 {
    def eb015pi: File = getAndRequireFileExists("resources/sample08/EB015PI.c3d")
  }

}
