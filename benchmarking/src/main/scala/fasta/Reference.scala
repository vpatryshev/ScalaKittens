package fasta

import java.io.{File, FilenameFilter}

import anuragsingh.SuffixTree
import fastq.Read

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import ukkonen.Lib
import ukkonen.Lib._
import ukkonen.MatchPoint

/**
  * Stores reference genome for any kind of pathogen; can match
  * Created by vpatryshev on 1/30/17.
  */
class Fragment(val name: String, val data: CharSequence) {
  log(s"New Fragment $name")
  val chars = "ACTG"
  val translate: Map[Char, Byte] = chars.zipWithIndex .map { case (c, i) => c -> ('A' + i).toByte } .toMap .withDefaultValue('E'.toByte) 
  
  def translate(s: CharSequence, bytes: Array[Byte]): Array[Byte] = {
    for (i <- 0 until s.length) bytes(i) = translate(s.charAt(i))

    bytes
  }
  
  def translate(s: String): Array[Byte] = {
    translate(s, new Array[Byte](s.length))
  }
  
  val Delim = '@'.toByte
  
  val bytes = new Array[Byte](data.length + 1)

  translate(data, bytes)
  bytes(data.length) = Delim

  val tree: SuffixTree = SuffixTree.fromBytes(bytes)

  def alignments(segment: String, k: Int, max: Int = Integer.MAX_VALUE): List[MatchPoint] = {
    val bytes = translate(segment)
    tree.listSlidingAlignments(bytes, k, max)
  }
  
  def alignments(segment: String) = tree.listAlignments(translate(segment))
}

case class MatchResult(read: Read, reference: Reference, alignments: List[(String, MatchPoint)], max: Int) {

  val showOneRow = ((fragname: String, point: MatchPoint) => {
    val x = s"${fragname.split(" ", 2).head},${point.location}"
    x
  } ).tupled

  def report: Option[String] = {
    if (alignments.size < max) {
      log(s"${read.id}: Just ${alignments.size} alignments found.")
      None
    }
    else {
      val buf = new StringBuilder
      for {a <- alignments} buf.append(s"  ${reference.name},${showOneRow(a)}\n")
      
      Some(buf.toString)
    }
  }
}


class Reference(val name: String, val fragments: List[Fragment]) {
  private[fasta] def alignments(sample: String): List[(String, MatchPoint)] = {
    val tuples: List[List[(String, MatchPoint)]] = fragments map (f => f.alignments(sample) map (p => (f.name, p)))
    
    tuples.flatten
  }

  def alignments(sample: String, kmer: Int, max: Int = Integer.MAX_VALUE): List[(String, MatchPoint)] = {
    val tuples: List[List[(String, MatchPoint)]] = fragments map (f => f.alignments(sample, kmer, max) map (p => (f.name, p)))

    tuples.flatten
  }

  import ExecutionContext.Implicits.global
  
  def listAlignments(r: Read, kmer: Int, max: Int = Integer.MAX_VALUE): Future[MatchResult] = Future {
    MatchResult(r, this, alignments(r.value, kmer, max), max)
  }
}

object Reference {
  import Lib._
  private val FilenameFormat = "([^\\.]+)\\.fasta$".r
  
  def nameFrom(file: File): String = {
//    file.getName.split("/").last.split("\\.").dropRight(1).mkString(".")
    (file.getName: String) 
    match {
      case FilenameFormat(name) => name
      case bs => fail(s"This is not a .fasta file: $file")
    }
  } 

  private def newFrag(input: BufferedIterator[String]): Option[Fragment] = {
    val sb = new StringBuilder
    Some(input) filter (_.hasNext) map { i =>
      val name = i.next
      sb.clear()
      while (i.hasNext && !i.head.startsWith(">")) {
        sb.append(i.next)        
      }
      val frag = new Fragment(name.tail.trim, sb)
      Runtime.getRuntime.gc()
      frag
    }
  }

  private def readFragment(i: BufferedIterator[String]) = 
    Option(i) filter (_.hasNext) flatMap newFrag
  
  def apply(filepath: String): Reference = apply(new File(filepath))
  
  def apply(file: File): Reference = {
    log(s"Reading $file...")
    def allocatedMemory = Runtime.getRuntime.totalMemory-Runtime.getRuntime.freeMemory

    val t0 = (Runtime.getRuntime.totalMemory + M/2)/M
    val m0 = allocatedMemory

    val input = openFile(file).buffered
    
    val name = nameFrom(file)
    
    val streamOfFragments = 
      Stream.continually(readFragment(input)) takeWhile(_.isDefined)

    val segments = streamOfFragments.toList map (_.get) // TODO(vlad): this is not monadic!
    val ref = new Reference(name, segments)
    val m1 = allocatedMemory
    val dm = (m1 - m0 + M/2)/M
    val mm = (Runtime.getRuntime.maxMemory+M/2)/M
    val t1 = (Runtime.getRuntime.totalMemory + M/2)/M

    log(s"Created a Genome $name, ${segments.length} segments; took ${dm}mb out of ${mm}mb, t1=$t1, t0=$t0")
    ref
  }
  
  def readAll(dirname: String): Map[String, Reference] = {
    val files: List[File] = listRefFiles(dirname)

    val references = files.map(apply)
    
    references.map(r => r.name -> r).toMap
  }

  val FastaOnly = new FilenameFilter {
    override def accept(dir: File, name: String) = name.endsWith(".fasta")
  }

  def listRefFiles(dirname: String): List[File] = {
    val dir = new File(dirname)
    require(dir.exists, s"Not found: $dirname")
    require(dir.isDirectory, s"Not a dir: $dirname")
    
    dir.listFiles(FastaOnly).toList
  }
}