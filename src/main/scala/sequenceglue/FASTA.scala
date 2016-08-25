package sequenceglue

import java.io.File

import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext, Future }
import scala.io.Source

case class FASTASequence(name: String, sequence: String)

/**
 * Data type representing two reads and how much they overlap on f1's right side and f2's left side.
 *
 * e.g.
 *   f1 = XXXAGA
 *   f2 = AGAXXX
 *   value = 3
 */
case class Overlap(f1: FASTASequence, f2: FASTASequence, value: Int) {

  /**
   * An overlap is gluable if the overlap value is over half of the lengths of the sequences.
   */
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

  /**
   * Determines the overlap between two sequences on the first sequence's right side and the second sequences's
   * left side.
   *
   * @param f1 first sequence, comparing its right side
   * @param f2 second sequence, comparing its left side
   * @return the number of chars that overlap on the first sequence's right side and the second sequence's left side.
   */
  def overlap(f1: FASTASequence, f2: FASTASequence): Int =
    f1.sequence
      .tails
      .find(tail => f2.sequence.startsWith(tail))
      .fold(0) { _.length }

  /**
   * Finds the overlaps of a Set of FASTASequences.
   * Returns a Vector of Overlaps in order from left to right.
   *
   * Under the hood, it finds overlaps in the left and right directions in parallel.
   *
   * @param sequences a Set of FASTASequences
   * @param f2 second sequence, comparing its left side
   * @return a Future containing Vector of Overlaps in order from left to right.
   */
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

  /**
   * Finds the overlaps of a Set of FASTASequences, working in the left direction.
   * Returns a Vector of Overlaps in order from left to right.
   *
   * @param current the current sequence for which to find a match
   * @param sequences Set of candidate sequences
   * @param acc accumulation Vector of Overlaps
   * @return a Vector of Overlaps in order from left to right.
   */
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

  /**
   * Finds the overlaps of a Set of FASTASequences, working in the right direction.
   * Returns a Vector of Overlaps in order from left to right.
   *
   * @param current the current sequence for which to find a match
   * @param sequences Set of candidate sequences
   * @param acc accumulation Vector of Overlaps
   * @return a Vector of Overlaps in order from left to right.
   */
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

  /**
   * Glues a sequence of overlaps into a superstring from left to right.
   *
   * @param overlaps a sequence of overlaps to glue together
   * @return a superstring
   */
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
