package org.c3d

import org.scalatest.FeatureSpec
import c3d.c3d2xml.C3D2XML.c3d2xmlString
import c3d.GenerateReference.{fetchC3DFile, fetchXMLFile, readFileToString}
import org.custommonkey.xmlunit.Diff

class Sample08Spec extends FeatureSpec {

  feature("Sample08 files should be read correctly") {

    lazy val refXML: String = readFileToString(fetchXMLFile("sample08/sample08.xml"))

    scenario("EB015PI.c3d") {
      val readXML: String = c3d2xmlString(fetchC3DFile("sample08/EB015PI.c3d")).getOrElse(fail())
      val diff: Diff = new Diff(refXML, readXML)
      assert(diff.similar())
    }

    scenario("TESTAPI.c3d") {
      val readXML: String = c3d2xmlString(fetchC3DFile("sample08/TESTAPI.c3d")).getOrElse(fail())
      val diff: Diff = new Diff(refXML, readXML)
      assert(diff.similar())
    }

    scenario("TESTBPI.c3d") {
      val readXML: String = c3d2xmlString(fetchC3DFile("sample08/TESTBPI.c3d")).getOrElse(fail())
      val diff: Diff = new Diff(refXML, readXML)
      assert(diff.similar())
    }

    scenario("TESTCPI.c3d") {
      val readXML: String = c3d2xmlString(fetchC3DFile("sample08/TESTCPI.c3d")).getOrElse(fail())
      val diff: Diff = new Diff(refXML, readXML)
      assert(diff.similar())
    }

    scenario("TESTDPI.c3d") {
      val readXML: String = c3d2xmlString(fetchC3DFile("sample08/TESTDPI.c3d")).getOrElse(fail())
      val diff: Diff = new Diff(refXML, readXML)
      assert(diff.similar())
    }

  }

}
