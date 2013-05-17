package c3d.c3d2xml

import java.io.{File, FileWriter, IOException}
import c3d.{C3D, Parameter}
import org.rogach.scallop._
import scala.xml._
import scala.reflect.runtime.universe._

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
    val c3d: C3D = readC3DFile(c3dFileName)

    // create name of output file
    val xmlFileName: String = if (conf.output.isDefined) conf.output() else c3dFileNameToXMLName(c3dFileName)
    println(s"xmlFileName = $xmlFileName")

    // generate XML tree
    val xml: Elem = generateXML(c3d)

    // save XML to file
    saveXML(xmlFileName, xml)

  }

  /** Generate XML tree for the C3D file.
    * 
    * @param c3d C3D structure
    * @return XML element
    */
  def generateXML(c3d: C3D): Elem = {
    <c3d>{
    generateGroups(c3d)
    }</c3d>
  }

  /** Generate XML data for groups and parameters. */
  def generateGroups(c3d: C3D): Elem = {
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
  def saveXML(xmlFileName: String, xml: Elem) {
    val xmlString = (new PrettyPrinter(80, 2)).format(xml)
    val fileWriter = new FileWriter(new File(xmlFileName))
    try {
      fileWriter.write(xmlString)
    } catch {
      case ioe: IOException => {
        println(s"Could not write file $xmlFileName.")
        println(s"  Error message: ${ioe.toString}")
      }
    } finally {
      fileWriter.close()
    }
  }

  /** Reads a C3D file, failing with sys.exit().
    * 
    * @param fileName name of input C3D file
    * @return C3D structure
    */
  def readC3DFile(fileName: String): C3D = {
    C3D.read(new File(fileName)) fold (
      failString => {
        println(s"Could not read C3D file $fileName.")
        println(s"  Error message: $failString")
        sys.exit(-1)
      },
      c3d => c3d
    )
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
