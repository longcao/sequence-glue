sequence-glue
=============

Scala project to solve [http://rosalind.info/problems/long/](http://rosalind.info/problems/long/), generally speaking, the [shortest common subsequence problem](https://en.wikipedia.org/wiki/Shortest_common_supersequence_problem), where we want to find the shortest superstring of genome reads.

Reads a [FASTA format](https://en.wikipedia.org/wiki/FASTA_format) text file and determines the shortest superstring.

# Running

`sbt run /path/to/fasta.txt`

# Testing

`sbt test`

# Design choices

* Very simple FASTA parsing into Scala data types. This could be an opportunity for optimization since I read in the entire file to memory before processing, which could prove cumbersome with a larger file.
* Algorithm follows a general greedy shape:
  1. Start with any FASTA sequence, call this the current.
  2. In parallel:
    1. For each remaining sequence, find the next sequence that overlaps the most by over half the length of each read with the tail of the current sequence. Repeat until no sequences remain.
        current: XXXAGGA
        next:    AGGAXXX
    2. Same as above, but working to the left of the current sequence.
        current: CGATXXX
        next:    XXXCGAT
  3. Glue resulting overlaps together linearly.

# Tradeoffs, TODO

* At scale, since this uses heavy amounts of in-memory processing, GC tuning _might_ be needed.
* There might be more clever or heavyweight ways to calculate this faster. One idea I had was to build superstrings in parallel with each start being a unique superstring, this may allow you to use something like Spark or MapReduce to start work from different points in parallel, and you'd pick the shortest result. Might be overkill, though.
* Implementation of finding overlaps in the left or right directions leaves something to be desired due to how similar the code looks, just with flipped parameters. There might be a better way to express this with less copypasta but I was spinning my tires trying to doing so.
* More elegant error handling/validation for FASTA parsing.
* Property-based testing with [ScalaCheck](http://scalacheck.org/), generating valid FASTASequences that overlap for the purposes of testing the functions. For the time being, using reference files for testing helped me implement this.