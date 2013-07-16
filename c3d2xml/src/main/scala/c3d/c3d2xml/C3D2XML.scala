package c3d.c3d2xml

import java.io.{File, FileWriter, IOException}
import c3d.{C3D, Group, Parameter}
import org.rogach.scallop._
import scala.collection.immutable._
import scala.xml._
import scala.reflect.runtime.universe._
import scalaz.{Failure, Success, Validation}

object C3D2XML {

  /** Configuration of command-line arguments. */
  class Conf(arguments: Array[String]) extends ScallopConf(arguments) {
    val fileName = trailArg[String](name = "filename.c3d", required = true,
      descr = "Name of the C3D file to convert to XML.")
    val output = opt[String](descr = "Name of an output XML file.")
  }

  /** Application main entry point.
    * 
    * @param args command-line arguments
    */
  def main(args: Array[String]) {
    // parse command-line settings
    val conf = new Conf(args)

    // read the input file specified
    val c3dFileName: String = conf.fileName()

    // create name of output file
    val xmlFileName: String = if (conf.output.isDefined) conf.output() else c3dFileNameToXMLName(c3dFileName)

    // do conversion
    c3d2xml(new File(c3dFileName), new File(xmlFileName)).fold (
      errString => {
        println("Could not perform conversion.")
        println(s"  Error: $errString")
      },
      success => success
    )
  }

  /** Converts a C3D `File` to an XML `File`.
    *
    * This method is suitable for calling as a library function.
    *
    * @param c3dFile C3D input `File`
    * @param xmlFile XML output `File`
    */
  def c3d2xml(c3dFile: File, xmlFile: File): Validation[String, Unit] = {
    // read the C3D file
    val c3d = C3D.read(c3dFile)
    // generate xml tree
    val xml: Elem = generateXML(c3d)
    // save XML to file
    saveXML(xmlFile, xml)
  }

  /** Converts a C3D `File` to an XML `String`.
    *
    * @param c3dFile C3D input `File`
    *
    * @return the XML `String`
    */
  def c3d2xmlString(c3dFile: File): String = {
    // read the C3D file
    val c3d = C3D.read(c3dFile)
    // generate xml tree
    val xml: Elem = generateXML(c3d)
    // generate the XML string
    val xmlString = (new PrettyPrinter(80, 2)).format(xml)
    xmlString
  }

  /** Generate XML tree for the C3D file.
    * 
    * @param c3d C3D structure
    * @return XML element
    */
  private def generateXML(c3d: C3D): Elem = {
    <c3d>{
      generateGroups(c3d)
    }</c3d>
  }

  /** Generate XML data for groups and parameters. */
  private def generateGroups(c3d: C3D): Elem = {

    def typeString(ptype: Parameter.Type): String = {
      import Parameter.Type
      ptype match {
        case Type.Character => "character"
        case Type.Integer   => "integer"
        case Type.Byte      => "byte"
        case Type.Float     => "float"
        case _ => throw new IllegalArgumentException("unexpected raw type encountered")
      }
    }

    def dims(dims: Seq[Int]): Elem = <dimensions>{ for (d <- dims) yield <dim size={d.toString}/> }</dimensions>

    def charData(group: String, param: String): PCData = 
      PCData(c3d.getParameter[String](group, param).get.data.mkString)

    def byteData(group: String, param: String): PCData = 
      PCData(c3d.getParameter[Byte](group, param).get.data.map(b => f"0x$b%02X").mkString(","))

    def paramData(g: Group, p: Parameter[_]): PCData = {
      assert(g.parameters contains p)
      import Parameter.Type
      p.parameterType match {
        case Type.Character => charData(g.name, p.name)
        case Type.Byte      => byteData(g.name, p.name)
        case _              => PCData(p.data.mkString(","))
      }
    }

    def param(g: Group, p: Parameter[_]): Elem = {
      assert(g.parameters contains p)
      <parameter 
        name={p.name} 
        description={p.description}
        isLocked={p.isLocked.toString} 
        type={typeString(p.parameterType)}
      >{
        scala.xml.Group(Seq(
          dims(p.dimensions),
          <data>{ paramData(g, p) }</data>
        ))
      }</parameter>
    }

    <groups>{
      for (group <- c3d.groups) yield {
        <group 
          name={group.name}
          description={group.description}
          isLocked={group.isLocked.toString}
        >{
          for (parameter <- group.parameters) yield param(group, parameter)
        }</group>
      }
    }</groups>

  }

  /** Saves an XML tree to a file.
    * 
    * @param xmlFileName name of the XML file to which the XML document should be saved
    * @param xml XML tree
    */
  private def saveXML(xmlFile: File, xml: Elem): Validation[String, Unit] = {
    val xmlString = (new PrettyPrinter(80, 2)).format(xml)
    val fileWriter = new FileWriter(xmlFile)
    try {
      fileWriter.write(xmlString)
      Success()
    } catch {
      case ioe: IOException => Failure(ioe.toString)
    } finally {
      fileWriter.close()
    }
  }

  /** Converts a C3D file name to an XML file name.
    * 
    * Rules: If the C3D file name ends in ".c3d" (case insensitive) then this extension is first removed.  Finally,
    * a ".xml" extension is added.
    * 
    * @param fileName name of the C3D file
    * @return XML file name
    */
  def c3dFileNameToXMLName(fileName: String): String = {
    val trimmedFileName = if (endsWithCI(fileName, ".c3d")) fileName.dropRight(4) else fileName
    s"$trimmedFileName.xml"
  }

  /** Tests if `baseString` ends with `testEnding` in a case-insensitive fashion. */
  def endsWithCI(baseString: String, testEnding: String): Boolean = {
    baseString.takeRight(testEnding.length).toUpperCase == testEnding.toUpperCase
  }

}
