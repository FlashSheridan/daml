// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf
package speedy
package perf

import com.daml.bazeltools.BazelRunfiles._
import com.daml.lf.archive.{Decode, UniversalArchiveReader}
import java.io.File
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

import com.daml.lf.data._
import com.daml.lf.data.Ref._
import com.daml.lf.language.Ast._
import com.daml.lf.archive.{Decode, UniversalArchiveReader}
import com.daml.lf.speedy.Pretty._
import com.daml.lf.speedy.SError._
import com.daml.lf.types.Ledger
import java.io.File


object Fib {
  def fib(n: Int): Int = {
    if (n == 0 || n == 1) {
      return 1;
    } else {
      return fib(n-1) + fib(n-2);
    }
  }
}

@State(Scope.Thread)
class CollectAuthorityState {
  val darFile = new File(rlocation("daml-lf/repl/CollectAuthority.dar"))
  val packages = UniversalArchiveReader().readFile(darFile).get
  val packagesMap = Map(packages.all.map {
    case (pkgId, pkgArchive) => Decode.readArchivePayloadAndVersion(pkgId, pkgArchive)._1
  }: _*)
  val scenarioRunner = Repl.ScenarioRunnerHelper(packagesMap)
  val mainPackageId = packages.main._1
  val mainPackage = packagesMap.get(mainPackageId).head
  val mainModule = mainPackage.modules.get(DottedName.assertFromSegments(Seq("CollectAuthority"))).head
  val defn = mainModule.definitions.get(DottedName.assertFromSegments(Seq("test"))).head
  val body = defn match {
    case DValue(_, _, body, _) => body
    case _ => sys.error("CollectAuthority:test is not a value definition")
  }
  val steps1 = run()

  def run(): Int = {
    val (machine, errOrLedger) = scenarioRunner.run(body)
    errOrLedger match {
      case Left((err, ledger @ _)) =>
        println(prettyError(err, machine.ptx).render(128))
        sys.error("scenario execution failed")
      case Right((diff @ _, steps @ _, ledger)) =>
        steps
    }
  }

  def arg(): Int = 20
}

class CollectAuthority {
  @Benchmark @BenchmarkMode(Array(Mode.AverageTime)) @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def bench(state: CollectAuthorityState): Int = {
    state.run()
  }
}

// The DAML-LF Read-Eval-Print-Loop
@SuppressWarnings(
  Array(
    "org.wartremover.warts.Any"
  ))
object Repl {

  private val nextSeed = {
    // We use a static seed to get reproducible run
    val seeding = crypto.Hash.secureRandom(crypto.Hash.hashPrivateKey("lf-repl"))
    () =>
      Some(seeding())
  }

  def bench(id: String, file: String): Unit = {
    val state = load(file)
    for (i @ _ <- 1 to 20) {
      invokeScenario(state, id)
    }
  }

  case class State(
      packages: Map[PackageId, Package],
      mainPackageId: PackageId,
      scenarioRunner: ScenarioRunnerHelper,
  )

  case class ScenarioRunnerHelper(packages: Map[PackageId, Package]) {
    private val build = Speedy.Machine
      .newBuilder(PureCompiledPackages(packages).right.get, Time.Timestamp.MinValue, nextSeed())
      .fold(err => sys.error(err.toString), identity)
    def run(expr: Expr)
      : (Speedy.Machine, Either[(SError, Ledger.Ledger), (Double, Int, Ledger.Ledger)]) =
      (build(expr), ScenarioRunner(build(expr)).run())
  }

  case class Command(help: String, action: (State, Seq[String]) => State)

  // Load DAML-LF packages from a set of files.
  def load(darFile: String): State = {
    val packages =
      UniversalArchiveReader().readFile(new File(darFile)).get
    val packagesMap = Map(packages.all.map {
      case (pkgId, pkgArchive) => Decode.readArchivePayloadAndVersion(pkgId, pkgArchive)._1
    }: _*)

    State(
      packages = packagesMap,
      mainPackageId = packages.main._1,
      scenarioRunner = ScenarioRunnerHelper(packagesMap)
    )
  }

  def buildExpr(state: State, id: String): Option[Expr] = {
    lookup(state.packages, state.mainPackageId, id) match {
      case None =>
        println("Error: " + id + " not found.")
        None
      case Some(DValue(_, _, body, _)) =>
        Some(body)
      case Some(_) =>
        println("Error: " + id + " is not a value.")
        None
    }
  }

  def invokeScenario(state: State, id: String): (Boolean, State) = {
    buildExpr(state, id)
      .map { expr =>
        val (machine, errOrLedger) =
          state.scenarioRunner.run(expr)
        errOrLedger match {
          case Left((err, ledger @ _)) =>
            println(prettyError(err, machine.ptx).render(128))
            (false, state)
          case Right((diff @ _, steps @ _, ledger)) =>
            // NOTE(JM): cannot print this, output used in tests.
            println(s"done in ${diff.formatted("%.2f")}ms, ${steps} steps")
            // println(prettyLedger(ledger).render(128))
            (true, state)
        }
      }
      .getOrElse((false, state))
  }

  def lookup(packages: Map[PackageId, Package], mainPackageId: PackageId, id: String): Option[Definition] = {
    val qualName = QualifiedName.fromString(id) match {
      case Left(err) => sys.error(s"Cannot parse qualified name $id: $err")
      case Right(x) => x
    }
    for {
      pkg <- packages.get(mainPackageId)
      module <- pkg.modules.get(qualName.module)
      defn <- module.definitions.get(qualName.name)
    } yield defn
  }
}
