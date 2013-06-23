package c3d.io

import c3d.Parameter
import scala.collection.immutable._
import org.scalatest.FunSpec
import Util.b

class UIntParameterSpec extends FunSpec {

  private val testIntParam: Parameter[Int] = new Parameter[Int] with ParameterTemplate[Int] {
    val name: String = "TEST"
    val description: String = "Test integer parameter"
    val isLocked: Boolean = false
    val dimensions: IndexedSeq[Int] = IndexedSeq(2)
    val data: IndexedSeq[Int] = IndexedSeq(30000, -96)
    val parameterType: Parameter.Type = Parameter.Type.Integer
  }
  private val uip: Parameter[Int] = new UIntParameter(testIntParam)

  describe("UIntParameter") {

    it("should have the correct name") { assert(uip.name === testIntParam.name) }
    it("should have the correct description") { assert(uip.description === testIntParam.description) }
    it("should have correct isLocked") { assert(uip.isLocked === testIntParam.isLocked) }
    it("should have correct dimensions") { assert(uip.dimensions === testIntParam.dimensions) }
    it("should have correct parameterType") { assert(uip.parameterType === Parameter.Type.Integer) }

    it("should correctly read values below 32767 (value 30000)") {
      assert(testIntParam(0) === 30000)
      assert(uip(0) === 30000)
    }

    it("should correctly read values above 32767 (value 65440)") {
      assert(testIntParam(1) === -96)
      assert(uip(1) === 65440)
    }

  }

}
