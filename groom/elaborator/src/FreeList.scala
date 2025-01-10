package groom.elaborator

import groom.rtl.backend._
import chisel3.experimental.SerializableModuleGenerator
import mainargs._
import chisel3.experimental.util.SerializableModuleElaborator

object FreeListMain extends SerializableModuleElaborator {
  implicit object PathRead extends TokensReader.Simple[os.Path] {
    def shortName = "path"
    def read(strs: Seq[String]) = Right(os.Path(strs.head, os.pwd))
  }

  val className: String = getClass.getSimpleName.replace("$", "")
  type D = FreeList
  type P = FreeListParameter
  type M = FreeListParameterMain

  @main
  case class FreeListParameterMain(
    @arg(name = "renameWidth") renameWidth: Int,
    @arg(name = "pregNum") pregNum: Int) {
    def convert: P = FreeListParameter(renameWidth, pregNum)
  }

  implicit def ALUParameterMainParser: ParserForClass[M] = ParserForClass[M]

  @main
  def config(@arg(name = "parameter") parameter: M) =
    os.write.over(os.pwd / s"${className}.json", configImpl(parameter.convert))

  @main
  def design(@arg(name = "parameter") parameter: os.Path) = {
    val (firrtl, annos) = designImpl[D, P](os.read.stream(parameter))
    os.write.over(os.pwd / s"$className.fir", firrtl)
    os.write.over(os.pwd / s"$className.json", annos)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}