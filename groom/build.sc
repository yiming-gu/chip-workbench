import mill._
import mill.scalalib._
import mill.define.{Command, TaskModule}
import mill.scalalib.publish._
import mill.scalalib.scalafmt._
import mill.scalalib.TestModule.Utest
import mill.util.Jvm
import coursier.maven.MavenRepository
import $file.common
import $file.dependencies.chisel.build
import $file.dependencies.arithmetic.common
import $file.dependencies.`chisel-interface`.common
import $file.dependencies.`berkeley-hardfloat`.common
import $file.dependencies.rvdecoderdb.common

object v {
  val scala    = "2.13.15"
  val mainargs = ivy"com.lihaoyi::mainargs:0.5.0"
  val oslib    = ivy"com.lihaoyi::os-lib:0.9.1"
  val upickle  = ivy"com.lihaoyi::upickle:3.3.1"
  val spire    = ivy"org.typelevel::spire:latest.integration"
  val evilplot = ivy"io.github.cibotech::evilplot:latest.integration"
}

object chisel extends Chisel

trait Chisel extends millbuild.dependencies.chisel.build.Chisel {
  def crossValue              = v.scala
  override def millSourcePath = os.pwd / "dependencies" / "chisel"
}

object arithmetic extends Arithmetic

trait Arithmetic extends millbuild.dependencies.arithmetic.common.ArithmeticModule {
  override def millSourcePath = os.pwd / "dependencies" / "arithmetic" / "arithmetic"
  def scalaVersion            = T(v.scala)

  def chiselModule    = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy       = None
  def chiselPluginIvy = None

  def spireIvy:    T[Dep] = v.spire
  def evilplotIvy: T[Dep] = v.evilplot
}

object axi4 extends AXI4

trait AXI4 extends millbuild.dependencies.`chisel-interface`.common.AXI4Module {
  override def millSourcePath = os.pwd / "dependencies" / "chisel-interface" / "axi4"
  def scalaVersion            = v.scala

  def mainargsIvy = v.mainargs

  def chiselModule    = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy       = None
  def chiselPluginIvy = None
}

object hardfloat extends Hardfloat

trait Hardfloat extends millbuild.dependencies.`berkeley-hardfloat`.common.HardfloatModule {
  override def millSourcePath = os.pwd / "dependencies" / "berkeley-hardfloat" / "hardfloat"
  def scalaVersion            = T(v.scala)

  def chiselModule    = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy       = None
  def chiselPluginIvy = None
}

object rvdecoderdb extends RVDecoderDB

trait RVDecoderDB extends millbuild.dependencies.rvdecoderdb.common.RVDecoderDBJVMModule with ScalaModule {
  def scalaVersion            = T(v.scala)
  def osLibIvy                = v.oslib
  def upickleIvy              = v.upickle
  override def millSourcePath = os.pwd / "dependencies" / "rvdecoderdb" / "rvdecoderdb"
}

object groom extends Groom

trait Groom extends millbuild.common.GroomModule with ScalafmtModule {
  override def millSourcePath = os.pwd / "groomemu" / "src"
  def scalaVersion = T(v.scala)

  override def sources = T.sources {
    super.sources() ++ Seq(PathRef(millSourcePath))
  }
  def arithmeticModule  = arithmetic
  def axi4Module        = axi4
  def hardfloatModule   = hardfloat
  def rvdecoderdbModule = rvdecoderdb
  def riscvOpcodesPath  = T.input(PathRef(os.pwd / "dependencies" / "riscv-opcodes"))

  def chiselModule    = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy       = None
  def chiselPluginIvy = None
}

// object configgen extends ConfigGen

// trait ConfigGen extends millbuild.common.ConfigGenModule with ScalafmtModule {
//   def scalaVersion = T(v.scala)

//   def groomModule = groom

//   def mainargsIvy = v.mainargs
// }

// object rocketv extends RocketV

// trait RocketV extends millbuild.common.RocketVModule with ScalafmtModule {
//   def scalaVersion      = T(v.scala)
//   def rvdecoderdbModule = rvdecoderdb
//   def riscvOpcodesPath  = T.input(PathRef(os.pwd / "dependencies" / "riscv-opcodes"))
//   def hardfloatModule   = hardfloat
//   def axi4Module        = axi4

//   def chiselModule    = Some(chisel)
//   def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
//   def chiselPluginIvy = None
//   def chiselIvy       = None
// }

// object groomemu extends GroomEmulator

// trait GroomEmulator extends millbuild.common.GroomEmulatorModule {
//   def scalaVersion = T(v.scala)

//   def groomModule = groom

//   def chiselModule    = Some(chisel)
//   def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
//   def chiselPluginIvy = None
//   def chiselIvy       = None
// }

// object rocketemu     extends RocketEmulator
// trait RocketEmulator extends millbuild.common.RocketEmulatorModule {
//   def scalaVersion = T(v.scala)

//   def rocketVModule = rocketv

//   def chiselModule    = Some(chisel)
//   def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
//   def chiselPluginIvy = None
//   def chiselIvy       = None
// }

object panamaconverter extends PanamaConverter

trait PanamaConverter extends millbuild.dependencies.chisel.build.PanamaConverter {
  def crossValue = v.scala

  override def millSourcePath = os.pwd / "dependencies" / "chisel" / "panamaconverter"

  def scalaVersion = T(v.scala)
}

// Module to generate RTL from json config
object elaborator extends Elaborator

trait Elaborator extends millbuild.common.ElaboratorModule {
  def scalaVersion = T(v.scala)

  def panamaconverterModule = panamaconverter

  def circtInstallPath = T.input(PathRef(os.Path(T.ctx().env("CIRCT_INSTALL_PATH"))))

  def generators = Seq(
    groom,
  )

  def mainargsIvy = v.mainargs

  def chiselModule    = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselPluginIvy = None
  def chiselIvy       = None
}

// object omreaderlib extends OMReaderLib

// trait OMReaderLib extends millbuild.common.OMReaderLibModule {
//   def scalaVersion = T(v.scala)

//   def panamaconverterModule = panamaconverter

//   def circtInstallPath = T.input(PathRef(os.Path(T.ctx().env("CIRCT_INSTALL_PATH"))))

//   def mainargsIvy = v.mainargs

//   def chiselModule    = Some(chisel)
//   def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
//   def chiselPluginIvy = None
//   def chiselIvy       = None
// }

// object omreader extends OMReader

// trait OMReader extends millbuild.common.OMReaderModule {
//   def scalaVersion = T(v.scala)

//   def panamaconverterModule = panamaconverter
//   def omreaderlibModule     = omreaderlib

//   def circtInstallPath = T.input(PathRef(os.Path(T.ctx().env("CIRCT_INSTALL_PATH"))))

//   def mainargsIvy = v.mainargs

//   def chiselModule    = Some(chisel)
//   def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
//   def chiselPluginIvy = None
//   def chiselIvy       = None
// }

// /** A simple release flow for T1 generator: package required dependency to flat jar. usage: mill
//   * t1package.{sourceJar,jar} out/t1package/sourceJar.dest/out.jar -> t1package-sources.jar
//   * out/t1package/jar.dest/out.jar -> t1package.jar out/t1package/chiselPluginJar.dest/out.jar -> chiselPlugin.jar these
//   * two jar is enough for this usages: object somepackagethatdependsont1 extends ScalaModule { def unmanagedClasspath =
//   * T(Seq(PathRef(os.pwd / "t1package.jar"), PathRef(os.pwd / "t1package-sources.jar"))) } For Jiuyang's Team, this is
//   * used for link T1 to NDA Blackboxes that cannot be open-sourced
//   */
// object t1package extends ScalaModule {
//   def scalaVersion = T(v.scala)
//   def moduleDeps   = super.moduleDeps ++ Seq(groom, groomemu, panamaconverter, omreaderlib)
//   override def sourceJar: T[PathRef] = T(
//     Jvm.createJar(
//       T.traverse(transitiveModuleDeps)(dep => T.sequence(Seq(dep.allSources, dep.resources, dep.compileResources)))()
//         .flatten
//         .flatten
//         .map(_.path)
//         .filter(os.exists),
//       manifest()
//     )
//   )
//   def chiselPluginJar = T {
//     val jar = T.dest / "out.jar"
//     os.copy(chisel.pluginModule.jar().path, jar)
//     PathRef(jar)
//   }
// }
