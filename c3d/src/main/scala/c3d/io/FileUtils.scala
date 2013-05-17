package c3d.io

import java.io.{DataInputStream, File, FileInputStream, IOException}
import scala.collection.immutable._
import scala.util.{Failure, Success, Try}

object FileUtils {

  /** Reads a `File` into an `IndexedSeq[Byte]`.
    * 
    * The method attempts to read `inFile` into an `IndexedSeq[Byte]`.  The attempt is wrapped in a `Try`, so that any
    * thrown exceptions will be recorded.  The method will fail if the file length exceeds `Int.MaxValue`, because in
    * that case, the file length exceeds the maximum length of a backing array which will be created to reference the
    * file.
    * 
    * TODO: This method may be too large and/or complicated.  I'm not sure if `Try` is really being used effectively
    * here.  Alternatives to this method in external libraries include Apache Commons IO and Guava.
    * 
    * @param inFile input file
    * @return Attempt to read the file to an `IndexedSeq[Byte]`.
    */
  def fileToIndexedSeq(inFile: File): Try[IndexedSeq[Byte]] = {
    Try {
      if (inFile.length > Int.MaxValue)
        throw new IOException(s"File length exceeds ${Int.MaxValue} - too large to read.")
    } flatMap { _ =>
      val byteArray = Array.ofDim[Byte](inFile.length.toInt)
      val dataInputStream = new DataInputStream(new FileInputStream(inFile))
      Try {
        dataInputStream.readFully(byteArray)
      } recover {
        case ioException: IOException => {
          dataInputStream.close()
          throw ioException
        }
      } map { _ =>
        dataInputStream.close()
        WrappedArrayIndexedSeq(byteArray)
      }
    }
  }

}
