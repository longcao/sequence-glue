package sequenceglue

import java.io.File

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Main {
  def main(args: Array[String]): Unit = {
    val fileName = args(0)

    val file = new File(fileName)

    val sequences = FASTA.parseFile(file)

    val result = for {
      overlaps <- FASTA.findOverlaps(sequences.toSet)
      glued = FASTA.glue(overlaps)
      validSuperstring = sequences.forall(fseq => glued.contains(fseq.sequence))
    } yield {
      println(glued)
      println(s"\nTotal length: ${glued.length}")
      println(s"\nValid superstring?: $validSuperstring\n")
    }

    Await.result(result, Duration.Inf)
  }
}
