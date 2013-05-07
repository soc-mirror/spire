package spire.math

object UtilAnnotations {
  class WorkInProgress(msg: String = "") extends annotation.Annotation
  class Broken(msg: String = "") extends annotation.Annotation
  class Finished(msg: String = "") extends annotation.Annotation
  class ToDo(msg: String = "") extends annotation.Annotation
  class Source(msg: String = "") extends annotation.Annotation
  class Tested(msg: String = "") extends annotation.Annotation
  class Endianess(end: Endian) extends annotation.Annotation
  sealed trait Endian
  object BigEndian extends Endian
  object LittleEndian extends Endian
  object Independent extends Endian
  object Unknown extends Endian
}
