package sequenceglue

import java.io.File

import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext, Future }
import scala.io.Source

case class FASTASequence(name: String, sequence: String)

case class Overlap(f1: FASTASequence, f2: FASTASequence, value: Int) {
  def isGluable: Boolean =
    value > (Math.min(f1.sequence.length, f2.sequence.length) / 2)
}

object FASTA {

  /**
   * Reads a FASTA-formatted file into memory as a collection of `FASTASequence`s.
   *
   * @param file the FASTA-formatted file to parse
   * @param demarcator symbol that demarcates a new FASTA sequence and prefixes its name. Defaults to ">".
   * @return a List of FASTASequences.
   */
  def parseFile(file: File, demarcator: String = ">"): List[FASTASequence] = {
    @tailrec
    def parseLines(lines: List[String], acc: List[FASTASequence]): List[FASTASequence] = {
      lines match {
        case head :: tail if head.startsWith(demarcator) =>
          val name = head.stripPrefix(demarcator)

          val sequence = tail
            .takeWhile(!_.startsWith(demarcator))
            .reduceLeft(_ + _)

          val fastaSeq = FASTASequence(name = name, sequence = sequence)

          parseLines(tail.dropWhile(!_.startsWith(demarcator)), fastaSeq :: acc)
        case _ => acc
      }
    }

    val src = Source.fromFile(file)

    try {
      val lines = src.getLines.toList

      parseLines(lines, List())
    } finally {
      src.close()
    }
  }

  def overlap(f1: FASTASequence, f2: FASTASequence): Int =
    f1.sequence
      .tails
      .find(tail => f2.sequence.startsWith(tail))
      .fold(0) { _.length }

  def findOverlaps(sequences: Set[FASTASequence])(implicit ec: ExecutionContext): Future[Vector[Overlap]] = {
    if (sequences.isEmpty) {
      Future.successful(Vector())
    } else {
      val current = sequences.head
      val rest = sequences - current

      val leftF = Future(findOverlapsLeft(current, rest, Vector()))
      val rightF = Future(findOverlapsRight(current, rest, Vector()))

      for {
        left <- leftF
        right <- rightF
      } yield (left ++ right)
    }
  }

  def findOverlapsLeft(
    current: FASTASequence,
    sequences: Set[FASTASequence],
    acc: Vector[Overlap]): Vector[Overlap] = {

    val overlaps = sequences.map { candidate =>
      Overlap(
        f1 = candidate,
        f2 = current,
        value = overlap(candidate, current))
    }.filter(_.isGluable)

    if (overlaps.isEmpty) {
      acc
    } else {
      val maxOverlap = overlaps.maxBy(_.value)
      val next = maxOverlap.f1

      findOverlapsLeft(next, sequences - next, acc.+:(maxOverlap))
    }
  }

  def findOverlapsRight(
    current: FASTASequence,
    sequences: Set[FASTASequence],
    acc: Vector[Overlap]): Vector[Overlap] = {

    val overlaps = sequences.map { candidate =>
      Overlap(
        f1 = current,
        f2 = candidate,
        value = overlap(current, candidate))
    }.filter(_.isGluable)

    if (overlaps.isEmpty) {
      acc
    } else {
      val maxOverlap = overlaps.maxBy(_.value)
      val next = maxOverlap.f2

      findOverlapsRight(next, sequences - next, acc :+ maxOverlap)
    }
  }

  def glue(overlaps: Seq[Overlap]): String = {
    if (overlaps.isEmpty) {
      ""
    } else {
      overlaps.foldLeft(overlaps.head.f1.sequence) { (acc, overlap) =>
        acc.dropRight(overlap.value) + overlap.f2.sequence
      }
    }
  }
}
