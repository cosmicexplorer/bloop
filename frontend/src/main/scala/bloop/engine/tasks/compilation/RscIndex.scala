// scalafmt: { maxcolumn = 130 }
package bloop.engine.tasks.compilation

import bloop.{Compiler => BloopCompiler, CompileProducts}
import bloop.data.Project
import bloop.engine.Dag
import bloop.io.AbsolutePath
import bloop.logging.{DebugFilter, Logger}

import monix.eval.Task
import xsbti.compile.PreviousResult

import rsc.report.StoreReporter

import scala.concurrent.Promise

case class RscCompileOutput(project: Option[Project], newClasspath: Seq[AbsolutePath]) {
  def asProducts: CompileProducts = {
    val (readOnlyClasses, newClasses) = newClasspath match {
      case Seq(dir) => (dir -> dir)
      case Seq(dir1, dir2) => (dir1 -> dir2)
      case x => throw new Exception(
        s"we expect only one or two classpath entries: was $x")
    }
    val emptyResult = PreviousResult.of(
      java.util.Optional.empty[xsbti.compile.CompileAnalysis],
      java.util.Optional.empty[xsbti.compile.MiniSetup])

    CompileProducts(
      readOnlyClasses.underlying,
      newClasses.underlying,
      emptyResult,
      emptyResult,
      Set.empty,
      Map.empty,
      Array())
  }
}

object RscCompileOutput {
  def empty = RscCompileOutput(None, Seq())
}

// FIXME: make this not a singleton!!
object RscIndex {
  import scala.meta.internal.semanticdb.Scala.Descriptor
  import scala.meta.scalasig.lowlevel.Name

  import rsc.classpath.Classpath

  import java.util.HashMap
  import java.util.concurrent.ConcurrentHashMap

  implicit val _ctx = DebugFilter.All

  val dcache: ConcurrentHashMap[String, (Descriptor, String)] =
    new ConcurrentHashMap(50000)
  val cacheSymIsEmbedded: HashMap[String, Boolean] = new HashMap()
  val ncache: HashMap[String, Name] = new HashMap(50000)

  val classpath = Classpath(List(), 32)

  case class ProjectName(name: String)
  val compileInvocationCache: ConcurrentHashMap[ProjectName, Promise[RscCompileOutput]] =
    new ConcurrentHashMap()

  def getOutputPromise(name: ProjectName): Promise[RscCompileOutput] =
    Option(compileInvocationCache.get(name)).getOrElse {
      val p = Promise[RscCompileOutput]()
      // NB: double-checked locking!!
      Option(compileInvocationCache.putIfAbsent(name, p))
        // `.putIfAbsent()` returns the previous value -- if null, then the promise `p` we've just
        // created is now the canonical promise.
        .getOrElse(p)
    }

  def registerMissing(name: ProjectName)(implicit logger: Logger): Unit = {
    val p = getOutputPromise(name)
    if (p.isCompleted) {
      logger.warn(s"project $name to mark missing already contained $p!!")
    } else {
      p.success(RscCompileOutput.empty)
      logger.info(s"missing output registered for project $name!")
    }
    ()
  }

  def registerCompiled(project: Project, successful: BloopCompiler.Result.Success)(implicit logger: Logger): Unit = {
    val newDirs = Seq(successful.products.newClassesDir, successful.products.readOnlyClassesDir)
      .map(AbsolutePath(_))
    val result = RscCompileOutput(Some(project), newDirs)
    // logger.info(s"result for project $project was: $result")
    val p = getOutputPromise(ProjectName(project.name))
    // logger.info(s"got promise $p!")
    if (p.isCompleted) {
      logger.warn(s"project name clash! previous result $p for $project. dropping current result $result")
    } else {
      p.success(result)
      logger.info(s"initialized result $result for project $project")
    }
    ()
  }

  def outline(project: Project)(implicit logger: Logger): Task[RscCompileOutput] = {
    logger.info(s"attempting to outline project $project with deps ${project.dependencies}")
    val dependentRscTasks: Seq[Task[RscCompileOutput]] = project.dependencies
      .map(ProjectName(_))
      .map(getOutputPromise(_))
      .map(f => Task.fromFuture(f.future))

    // NB: using Task.gather to maintain the same classpath ordering! See https://monix.io/docs/2x/eval/task.html!
    val inputClasspath: Task[Seq[AbsolutePath]] = Task.gather(dependentRscTasks)
      .map(_.flatMap(_.newClasspath))
      .map { rscCompileClasspath =>
        val cpWithLibs = rscCompileClasspath ++ project.rawClasspath
        logger.info(s"indexing rsc classpath entries $cpWithLibs")
        // NB: Here, we index the classpath so that subsequent rsc invocations can avoid init time!
        // FIXME: convert this into returning a Future or Task instead of a synchronous .go()?!
        classpath.go(cpWithLibs.map(_.underlying).toList)
        cpWithLibs
      }

    val outputJar = project.genericClassesDir.resolve("out.jar")
    logger.info(s"output jar $outputJar for project $project")
    val doRscCompile: Task[RscCompileOutput] = inputClasspath.flatMap { rscClasspath =>
      val rscSettings = rsc.settings.Settings(
        abi = rsc.settings.Abi212,
        artifacts = List(rsc.settings.ArtifactScalasig),
        cp = rscClasspath.map(_.underlying).toList,
        // NB: Output to the same classes dir as normal compiles (??!)
        d = outputJar.underlying,
        debug = true,
        ins = project.sources.map(_.underlying).toList,
        notypeWarn = true,
        xprint = Set("timings"),
        dcache = dcache,
        cacheSymIsEmbedded = new HashMap[String, Boolean](), // cacheSymIsEmbedded,
        ncache = new HashMap[String, Name](),                // ncache,
        classpath = classpath
      )
      // logger.info(s"rscSettings $rscSettings for project $project")

      val rscReporter = rsc.report.StoreReporter(rscSettings)
      val rscCompiler = rsc.Compiler(rscSettings, rscReporter)
      Task {
        import rsc.report._
        implicit val _ctx: bloop.logging.DebugFilter = bloop.logging.DebugFilter.All
        // FIXME: make this return a Future or Task instead of synchronously/statefully blocking!!
        rscCompiler.run2()
        rscReporter.messages.foreach {
          case _: rsc.report.ErrorSummary => ()
          case x => x.sev match {
            case FatalSeverity => logger.error(x.str)
            case ErrorSeverity => logger.error(x.str)
            case WarningSeverity => logger.warn(x.str)
            case VerboseSeverity => logger.info(x.str)
          }
        }
        if (rscReporter.problems.isEmpty) RscCompileOutput(Some(project), Seq(outputJar))
        else throw new Exception("???/rsc failed!! see messages!!!")
      }.doOnFinish { _ =>
        rscCompiler.close()
        Task.unit
      }
    }

    val outputPromise = getOutputPromise(ProjectName(project.name))
    if (outputPromise.isCompleted) {
      logger.warn(s"project $project is already completed with $outputPromise!")
      Task.fromFuture(outputPromise.future)
    } else {
      doRscCompile.flatMap { rscOutput =>
        if (outputPromise.isCompleted) {
          logger.warn(s"project $project is already completed with $outputPromise!! new is $rscOutput")
          Task.fromFuture(outputPromise.future)
        } else {
          outputPromise.success(rscOutput)
          Task.now(rscOutput)
        }
      }
    }
  }
}
