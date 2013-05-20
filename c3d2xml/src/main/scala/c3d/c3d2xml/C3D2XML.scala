package c3d.c3d2xml

import java.io.{File, FileWriter, IOException}
import c3d.{C3D, Parameter}
import org.rogach.scallop._
import scala.xml._
import scala.reflect.runtime.universe._
import scalaz.{Failure, Success, Validation}

object C2D2XML {

  /** Configuration of command-line arguments. */
  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
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
    * 
    * @return `Validation` of `Unit` for success and a `String` for failure.
    */
  def c3d2xml(c3dFile: File, xmlFile: File): Validation[String, Unit] = {
    // read the C3D file
    C3D.read(c3dFile).flatMap { c3d =>
      // generate xml tree
      val xml: Elem = generateXML(c3d)
      // save XML to file
      saveXML(xmlFile, xml)
    }
  }

  /** Converts a C3D `File` to an XML `String`.
    * 
    * @param c3dFile C3D input `File`
    * 
    * @return `Validation` containing the XML `String`, or a `String` for failure.
    */
  def c3d2xmlString(c3dFile: File): Validation[String, String] = {
    // read the C3D file
    C3D.read(c3dFile).map { c3d =>
      // generate xml tree
      val xml: Elem = generateXML(c3d)
      // generate the XML string
      val xmlString = (new PrettyPrinter(80, 2)).format(xml)
      xmlString
    }
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
    <groups>{
      for (group <- c3d.groups) yield {
        <group 
          name={group.name}
          description={group.description}
          isLocked={group.isLocked.toString}
        >{
          for (parameter <- group.parameters) yield {
            val typeString = parameter.parameterType match {
              case Parameter.Type.Character => "character"
              case Parameter.Type.Integer   => "integer"
              case Parameter.Type.Byte      => "byte"
              case Parameter.Type.Float     => "float"
              case _ => {
                println("unexpected raw parameter type found")
                sys.exit(-1)
              }
            }
            <parameter 
              name={parameter.name}
              description={parameter.description}
              isLocked={parameter.isLocked.toString}
              type={typeString}
            >{
              <dimensions>{
                for (d <- parameter.dimensions) yield <dimension size={d.toString}/>
              }</dimensions>
              <data>{
                val data = parameter.parameterType match {
                  case Parameter.Type.Character => {
                    val stringParam = c3d.getParameter[String](group.name, parameter.name).get
                    PCData(stringParam.data.mkString)
                  }
                  case Parameter.Type.Byte => {
                    val pdata = parameter.asInstanceOf[Parameter[Byte]].data
                    PCData(pdata.map(b => f"0x$b%02X").mkString(","))
                  }
                  case _ => PCData(parameter.data.mkString(","))
                }
                data
              }</data>
            }</parameter>
          }
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
