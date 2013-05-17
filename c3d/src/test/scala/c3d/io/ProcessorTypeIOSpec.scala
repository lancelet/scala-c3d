package c3d.io

import org.scalatest.FunSpec
import c3d.ProcessorType
import Util.b

class ProcessorTypeIOSpec extends FunSpec {

  describe("ProcessorTypeIO") {

    import ProcessorTypeIO._

    it("should convert Bytes to ProcessorTypes") {
      assert(byteToProcessorType(b(84)) === Some(ProcessorType.Intel))
      assert(byteToProcessorType(b(85)) === Some(ProcessorType.DEC))
      assert(byteToProcessorType(b(86)) === Some(ProcessorType.SGIMIPS))
      assert(byteToProcessorType(b(87)) === None)
    }

    it("should convert ProcessorTypes to Bytes") {
      assert(processorTypeToByte(ProcessorType.Intel)   === b(84))
      assert(processorTypeToByte(ProcessorType.DEC)     === b(85))
      assert(processorTypeToByte(ProcessorType.SGIMIPS) === b(86))
    }

  }

}
