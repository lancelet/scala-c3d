package org.c3d

import org.scalatest.FeatureSpec
import c3d.c3d2xml.C3D2XML.c3d2xmlString
import c3d.GenerateReference.{fetchC3DFile, fetchXMLFile, readFileToString}
import org.custommonkey.xmlunit.Diff

class Sample08Spec extends FeatureSpec {

  feature("Sample08 files should be read correctly") {

    scenario("EB015PI.c3d") {
      val readXML: String = c3d2xmlString(fetchC3DFile("sample08/EB015PI.c3d")).getOrElse(fail())
      val refXML: String = readFileToString(fetchXMLFile("sample08/sample08.xml"))
      val diff: Diff = new Diff(refXML, readXML)
      assert(diff.similar())
    }

    scenario("TESTAPI.c3d") (pending)

    scenario("TESTBPI.c3d") (pending)

    scenario("TESTCPI.c3d") (pending)

    scenario("TESTDPI.c3d") (pending)

  }

}
