package c3d

trait C3D {

  /** Processor type used to store floating point or integer data in this C3D file. */
  def processorType: ProcessorType
  /** Type of data storage for 3D and/or analog data. */
  def storedDataType: StoredDataType
}
