package sequenceglue

import java.io.File

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.time.{ Minutes, Span }
import org.scalatest.{ FlatSpec, Matchers }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FASTASpec extends FlatSpec
  with Matchers
  with ScalaFutures
  with TypeCheckedTripleEquals {

  private def getFile(path: String): File = new File(getClass.getResource(path).toURI)

  "parseFile" should "return valid FASTASequences" in {
    val file1 = getFile("/sample.txt")
    val file2 = getFile("/bigger_sample.txt")

    val sequences = FASTA.parseFile(file1)
    val moreSequences = FASTA.parseFile(file2)

    sequences.length should ===(4)
    sequences.foreach { fseq =>
      fseq.sequence should not be empty
    }

    moreSequences.length should ===(50)
    moreSequences.foreach { fseq =>
      fseq.sequence should not be empty
    }
  }

  "overlap" should "be 0 for the empty case" in {
    val f1 = FASTASequence("test1", "")
    val f2 = FASTASequence("test2", "")

    FASTA.overlap(f1, f2) should ===(0)
    FASTA.overlap(f2, f1) should ===(0)
  }

  it should "be 0 for non overlapping reads" in {
    val f1 = FASTASequence("test1", "GATTACA")
    val f2 = FASTASequence("test2", "TATAGAC")

    FASTA.overlap(f1, f2) should ===(0)
    FASTA.overlap(f2, f1) should ===(0)
  }

  it should "be correct for a simple case, right biased" in {
    val f1 = FASTASequence("test1", "ATTAGACCTG")
    val f2 = FASTASequence("test2", "AGACCTGCCG")

    FASTA.overlap(f1, f2) should ===(7)
  }

  it should "be correct for a simple case, left biased" in {
    val f1 = FASTASequence("test1", "GCCGGAATAC")
    val f2 = FASTASequence("test2", "ACCTGCCGGA")

    FASTA.overlap(f2, f1) should ===(6)
  }

  "Overlap.isGluable" should "be correct for a simple case" in {
    val f1 = FASTASequence("test1", "ATTAGACCTG")
    val f2 = FASTASequence("test2", "AGACCTGCCGACG")

    Overlap(f1, f2, FASTA.overlap(f1, f2)).isGluable shouldBe true
    Overlap(f2, f1, FASTA.overlap(f2, f1)).isGluable shouldBe false
  }

  "findOverlapsRight" should "find overlaps for a simple case" in {
    val f1 = FASTASequence("test1", "ATTAGACCTG")
    val f2 = FASTASequence("test2", "AGACCTGCCG")
    val f3 = FASTASequence("test3", "CCTGCCGGAA")

    val res = FASTA.findOverlapsRight(
      current = f1,
      sequences = Set(f2, f3),
      acc = Vector())

    res should ===(Vector(
      Overlap(
        f1, f2, 7),
      Overlap(
        f2, f3, 7)))
  }

  "findOverlapsLeft" should "find overlaps for a simple case" in {
    val f1 = FASTASequence("test1", "CCTGCCGGAA")
    val f2 = FASTASequence("test2", "AGACCTGCCG")
    val f3 = FASTASequence("test3", "ATTAGACCTG")

    val res = FASTA.findOverlapsLeft(
      current = f1,
      sequences = Set(f2, f3),
      acc = Vector())

    res should ===(Vector(
      Overlap(
        f3, f2, 7),
      Overlap(
        f2, f1, 7)))
  }

  "glue" should "glue several overlaps together in sequence" in {
    val f1 = FASTASequence("test1", "CCTGCCGGAA")
    val f2 = FASTASequence("test2", "AGACCTGCCG")
    val f3 = FASTASequence("test3", "ATTAGACCTG")

    val overlaps = Vector(
      Overlap(
        f3, f2, 7),
      Overlap(
        f2, f1, 7))

    val glued = FASTA.glue(overlaps)

    glued should ===("ATTAGACCTGCCGGAA")
    List(f1, f2, f3).forall(fseq => glued.contains(glued)) shouldBe true
  }

  "End-to-end integration" should "return a valid superstring of all sequences in a file" in {
    val file = getFile("/bigger_sample.txt")

    val sequences = FASTA.parseFile(file)

    val result: Future[(String, Boolean)] = for {
      overlaps <- FASTA.findOverlaps(sequences.toSet)
      glued = FASTA.glue(overlaps)
      validSuperstring = sequences.forall(fseq => glued.contains(fseq.sequence))
    } yield {
      (glued, validSuperstring)
    }

    whenReady(result, timeout = Timeout(Span(2, Minutes))) { case (glued, validSuperstring) =>
      glued should not be empty
      validSuperstring shouldBe true
    }
  }
}
