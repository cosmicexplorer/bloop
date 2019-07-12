package bloop.engine.tasks.compilation

import bloop.Compiler
import bloop.engine.Dag
import bloop.engine.caches.LastSuccessfulResult

import monix.execution.CancelableFuture
import monix.eval.Task

case class BackgroundNonRscCompileTaskResult(
  result: PartialCompileResult,
  dag: Dag[PartialCompileResult]
)

case class ResultBundle(
    fromCompiler: Compiler.Result,
    successful: Option[LastSuccessfulResult],
    runningBackgroundTasks: CancelableFuture[Unit],
    realCompilationTask: Option[Task[BackgroundNonRscCompileTaskResult]] = None
)

object ResultBundle {
  val empty: ResultBundle =
    ResultBundle(Compiler.Result.Empty, None, CancelableFuture.unit)

  def apply(
      fromCompiler: Compiler.Result,
      successful: Option[LastSuccessfulResult]
  ): ResultBundle = ResultBundle(fromCompiler, successful, CancelableFuture.unit)
}
