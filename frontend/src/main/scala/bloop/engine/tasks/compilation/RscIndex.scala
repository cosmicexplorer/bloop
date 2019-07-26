// scalafmt: { maxcolumn = 130 }
package bloop.engine.tasks.compilation

import bloop.{Compiler => BloopCompiler, _}
import bloop.data.Project
import bloop.io.AbsolutePath
import bloop.logging.{DebugFilter, Logger}

import monix.eval.Task
import monix.execution.CancelableFuture
import xsbti.compile.PreviousResult

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration._

case class RscArgs(args: Seq[String])

case class RscCompileOutput(project: Option[RscProjectName], var newClasspath: Seq[AbsolutePath]) {
  def asProducts: CompileProducts = synchronized {
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

  def asResultBundle: ResultBundle = {
    val output = BloopCompiler.Result.RscSuccess(asProducts)
    ResultBundle(
      fromCompiler = output,
      successful = None,
      runningBackgroundTasks = CancelableFuture.unit,
      realCompilationTask = None
    )
  }

  def prependClasspath(rhs: RscCompileOutput): Unit = synchronized {
    newClasspath = rhs.newClasspath ++ newClasspath
  }
}

object RscCompileOutput {
  lazy val empty = RscCompileOutput(None, Seq())
}

case class RscIndex(targetMapping: Map[RscProjectName, RscTargetInfo]) extends RscCompiler {
  import scala.meta.internal.semanticdb.Scala.Descriptor
  import scala.meta.scalasig.lowlevel.Name

  import rsc.report.StoreReporter
  import rsc.classpath.Classpath

  import java.util.HashMap
  import java.util.concurrent.ConcurrentHashMap

  implicit val _ctx = DebugFilter.All

  private val dcache: ConcurrentHashMap[String, (Descriptor, String)] = new ConcurrentHashMap(50000)
  private val cacheSymIsEmbedded: HashMap[String, Boolean] = new HashMap()
  private val ncache: HashMap[String, Name] = new HashMap(50000)

  private val classpath = Classpath(List(), 32)

  // sealed abstract class RscOrScalacOutput
  // case class RscOutput(output: RscCompileOutput) extends RscOrScalacOutput
  // case class ScalacOutput(output: CompileProducts) extends RscOrScalacOutput

  private val rscMixedCompileClasspath: ConcurrentHashMap[RscProjectName, Promise[RscCompileOutput]] =
    new ConcurrentHashMap
  // private val runtimeClasspath: ConcurrentHashMap[RscProjectName, Promise[ResultBundle]] =
  //   new ConcurrentHashMap

  // def getScalacPromise

  // Returns a promise of an output, and whether the promise returned is new!
  def getOutputPromise(name: RscProjectName): (Promise[RscCompileOutput], Boolean) =
    Option(rscMixedCompileClasspath.get(name)).map((_ -> false)).getOrElse {
      val p = Promise[RscCompileOutput]()
      // NB: double-checked locking!!
      Option(rscMixedCompileClasspath.putIfAbsent(name, p))
        .map((_ -> false))
        // `.putIfAbsent()` returns the previous value -- if null, then the promise `p` we've just
        // created is now the canonical promise.
        .getOrElse((p -> true))
    }

  private def runningPromisesSequence: Seq[(RscProjectName, Promise[RscCompileOutput])] =
    rscMixedCompileClasspath.asScala.toSeq

  def displayRunningPromisesSummary: String = {
    val summary = runningPromisesSequence.map {
      case (RscProjectName(name), p) => s"$name:\t$p"
    }.mkString("\n")
    s"project\tpromise\n------\n$summary"
  }

  def maybeGetRscCompile(project: Project): Option[RscArgs] =
    targetMapping.get(RscProjectName(project.name)) match {
      case Some(RscTargetInfo(ZincOnly, _)) => None
      case Some(RscTargetInfo(ZincJava, _)) => None
      case None => None
      case Some(RscTargetInfo(RscAndZinc, rscArgs)) => Some(RscArgs(rscArgs))
    }

  def maybeGetWorkflow(project: Project): Option[RscWorkflow] = targetMapping
    .get(RscProjectName(project.name))
    .map(_.workflow)

  def registerMissing(name: RscProjectName)(implicit logger: Logger): Boolean = {
    val (p, _) = getOutputPromise(name)
    p.trySuccess(RscCompileOutput.empty)
  }

  def registerCompiled(project: Project, successful: BloopCompiler.Result.Success)(implicit logger: Logger): Boolean = {
    val name = RscProjectName(project.name)
    val newDirs = Seq(successful.products.newClassesDir, successful.products.readOnlyClassesDir)
      .map(AbsolutePath(_))
    val result = RscCompileOutput(Some(name), newDirs)
    val (p, _) = getOutputPromise(name)
    val isNew: Boolean = p.trySuccess(result)
    if (isNew) {
      logger.info(s"initialized result $result for project $project")
    } else {
      val previousResult = Await.result(p.future, FiniteDuration(0, "s"))
      assert(previousResult.project.nonEmpty)
      assert(result.project.nonEmpty)
      logger.warn(s"project name clash! new result $result for $project. pushing previous result $previousResult to the back!")
      previousResult.prependClasspath(result)
    }
    isNew
  }

  def outline(project: Project, rscArgs: RscArgs)(implicit logger: Logger): Task[RscCompileOutput] = {
    getOutputPromise(RscProjectName(project.name)) match {
      case (p, false) =>
        logger.info(s"blocking on previous compile promise $p for project $project!")
        return Task.fromFuture(p.future)
      case (_, true) =>
        logger.info(s"attempting to outline project $project with deps ${project.dependencies}")
    }

    val dependentRscTasks: Seq[Task[RscCompileOutput]] = project.dependencies
      .map(RscProjectName(_))
      .map(getOutputPromise(_))
      .map {
        case (p, _) => Task.fromFuture(p.future)
      }

    // NB: using Task.gather to maintain the same classpath ordering! See https://monix.io/docs/2x/eval/task.html!
    val inputClasspath: Task[Seq[AbsolutePath]] = Task.gather(dependentRscTasks)
      .map(_.flatMap(_.newClasspath))

    logger.info(s"rsc args for project $project were: $rscArgs")
    val baseSettings = rsc.settings.Settings.parse(rscArgs.args.toList).get.copy(
      xprint = Set("timings"),
      dcache = dcache,
      cacheSymIsEmbedded = new HashMap[String, Boolean](), // cacheSymIsEmbedded,
      ncache = new HashMap[String, Name](),                // ncache,
      classpath = classpath
    )

    val outputJar = AbsolutePath(baseSettings.d)
    logger.info(s"output jar $outputJar for project $project")

    val indexClasspath = inputClasspath.flatMap { rscClasspath =>
      // FIXME: do we need anything other than the base settings, since the compiles should output
      // to the same place????
      val entireInputCp = rscClasspath.toList ++ baseSettings.cp.map(AbsolutePath(_))
      // FIXME: convert this into returning a Future or Task instead of a synchronous .go()?!
      val indexTask = Task.fork(Task.eval(classpath.go(entireInputCp.map(_.underlying))))
      val settings = baseSettings.copy(cp = entireInputCp.map(_.underlying))
      val reporter = rsc.report.StoreReporter(settings)
      val compiler = rsc.Compiler(settings, reporter)
      indexTask.map(Unit => (settings, reporter, compiler))
    }
    val doRscCompile: Task[RscCompileOutput] = indexClasspath.flatMap {
      case (settings, reporter, compiler) =>
        Task {
          import rsc.report._
          implicit val _ctx: bloop.logging.DebugFilter = bloop.logging.DebugFilter.Compilation
          // FIXME: make this return a Future or Task instead of synchronously/statefully blocking!!
          compiler.run2()
          reporter.messages.foreach {
            case _: rsc.report.ErrorSummary => ()
            case x => x.sev match {
              case FatalSeverity => logger.error(x.str)
              case ErrorSeverity => logger.error(x.str)
              case WarningSeverity => logger.warn(x.str)
              case VerboseSeverity => logger.info(x.str)
            }
          }
          val name = RscProjectName(project.name)
          if (reporter.problems.isEmpty) RscCompileOutput(Some(name), Seq(outputJar))
          else throw new Exception("???/rsc failed!! see messages!!!")
        }.doOnFinish { _ =>
          compiler.close()
          Task.unit
        }
    }

    val (outputPromise, _) = getOutputPromise(RscProjectName(project.name))
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

object RscIndex {
  def parse(serializedTargetMapping: Map[String, (String, Seq[String])]): RscIndex = {
    val map = serializedTargetMapping.map {
      case (name, (workflowArg, rscArgs)) =>
        val workflow: RscWorkflow = workflowArg match {
          case "rsc-and-zinc" => RscAndZinc
          case "zinc-java" => ZincJava
          case "zinc-only" => ZincOnly
          case _ => throw new Exception(s"unrecognized rsc compatibility $workflowArg!!")
        }
        (RscProjectName(name) -> RscTargetInfo(workflow, rscArgs))
    }
    RscIndex(map)
  }
}
