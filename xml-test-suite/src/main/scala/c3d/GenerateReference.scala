package c3d

import org.rogach.scallop._
import c3d.c3d2xml.C3D2XML
import java.io.File
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

object GenerateReference {

  /** Configuration of command-line arguments. */
  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    version("GenerateReference")
    banner("""Usage: GenerateReference [OPTIONS]
             |
             |GenerateReference creates reference XML files from C3D files.  It is used to
             |create the XML files that are part of the XML test suite for scala-c3d.  The
             |XML files that are generated should be carefully hand-checked before being
             |accepted into the reference suite.  This program should NOT be called
             |automatically, but rather be invoked when any major changes to scala-c3d
             |require modifications to the test suite.
             |
             |Options:
             |""".stripMargin)
    footer("Copyright (C) Jonathan Merritt 2013")
    val sample08 = toggle("sample08", default=Some(false), noshort=true,
      descrYes="Generates reference XML file for the sample08 C3D files",
      descrNo="  [OFF BY DEFAULT]")
  }

  def main(args: Array[String]) {
    
    // parse command-line arguments
    val conf = new Conf(args)

    // someAction is the OR of all actions; if no action is specified then display the help string
    val someAction: Boolean = conf.sample08()
    if (!someAction) conf.printHelp()

    // execute individual actions
    if (conf.sample08()) generateSample08()

  }

  /** Fetches a C3D file from the c3d.org-example-files directory, checking that it exists. */
  def fetchC3DFile(name: String): File = {
    val c3dFile: File = new File(s"./c3d.org-example-files/$name")
    assert(c3dFile.exists, s"File '${c3dFile.getName}' could not be found.\n" +
      "Have you run the fetch-c3d-example-files sbt task?")
    c3dFile
  }

  /** Fetches an XML file from the ./xml-test-suite/reference directory. */
  def fetchXMLFile(name: String): File = new File(s"./xml-test-suite/reference/$name")

  /** Reads a file to a String. */
  def readFileToString(inFile: File): String = {
    val encoded: Array[Byte] = Files.readAllBytes(Paths.get(inFile.toURI))
    val encoding: Charset = Charset.defaultCharset
    encoding.decode(ByteBuffer.wrap(encoded)).toString
  }

  /** Generates an XML file from a C3D file using the directories specified by [[fetchC3DFile]] and [[fetchXMLFile]]. */
  private def generateXMLfromC3D(c3dName: String, xmlName: String) = 
    C3D2XML.c3d2xml(fetchC3DFile(c3dName), fetchXMLFile(xmlName))

  /** Generates XML file for the Sample08 examples from c3d.org.
    * 
    * All of the Sample08 C3D files are the same C3D file encoded in different ways.  However, since POINT:DATA_START
    * is stored differently for each file, a separate XML comparison file is required for each.
    * 
    * Please see the readme.txt file provided with sample08 for more information.
    */
  private def generateSample08() = {
    generateXMLfromC3D("sample08/EB015PI.c3d", "sample08/EB015PI.xml")
    generateXMLfromC3D("sample08/TESTAPI.c3d", "sample08/TESTAPI.xml")
    generateXMLfromC3D("sample08/TESTBPI.c3d", "sample08/TESTBPI.xml")
    generateXMLfromC3D("sample08/TESTCPI.c3d", "sample08/TESTCPI.xml")
    generateXMLfromC3D("sample08/TESTDPI.c3d", "sample08/TESTDPI.xml")
  }

}
