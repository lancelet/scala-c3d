package org.c3d

import org.scalatest.FeatureSpec
import c3d.c3d2xml.C3D2XML.c3d2xmlString
import c3d.GenerateReference.{fetchC3DFile, fetchXMLFile, readFileToString}
import org.custommonkey.xmlunit.Diff

class Sample01Spec extends FeatureSpec {

  feature("Sample01 files should be read correctly") {

    scenario("Eb015pi.c3d") {
      val refXML  = readFileToString(fetchXMLFile("sample01/Eb015pi.xml"))
      val readXML = c3d2xmlString(fetchC3DFile("sample01/Eb015pi.c3d")).getOrElse(fail())
      val diff = new Diff(refXML, readXML)
      assert(diff.similar())
    }

    scenario("Eb015pr.c3d") {
      val refXML  = readFileToString(fetchXMLFile("sample01/Eb015pr.xml"))
      val readXML = c3d2xmlString(fetchC3DFile("sample01/Eb015pr.c3d")).getOrElse(fail())
      val diff = new Diff(refXML, readXML)
      assert(diff.similar())
    }

    scenario("Eb015si.c3d") {
      val refXML  = readFileToString(fetchXMLFile("sample01/Eb015si.xml"))
      val readXML = c3d2xmlString(fetchC3DFile("sample01/Eb015si.c3d")).getOrElse(fail())
      val diff = new Diff(refXML, readXML)
      assert(diff.similar())
    }

    scenario("Eb015sr.c3d") {
      val refXML  = readFileToString(fetchXMLFile("sample01/Eb015sr.xml"))
      val readXML = c3d2xmlString(fetchC3DFile("sample01/Eb015sr.c3d")).getOrElse(fail())
      val diff = new Diff(refXML, readXML)
      assert(diff.similar())
    }

    scenario("Eb015vi.c3d") {
      val refXML  = readFileToString(fetchXMLFile("sample01/Eb015vi.xml"))
      val readXML = c3d2xmlString(fetchC3DFile("sample01/Eb015vi.c3d")).getOrElse(fail())
      val diff = new Diff(refXML, readXML)
      assert(diff.similar())
    }

    scenario("Eb015vr.c3d") {
      val refXML  = readFileToString(fetchXMLFile("sample01/Eb015vr.xml"))
      val readXML = c3d2xmlString(fetchC3DFile("sample01/Eb015vr.c3d")).getOrElse(fail())
      val diff = new Diff(refXML, readXML)
      assert(diff.similar())
    }

  }

}
