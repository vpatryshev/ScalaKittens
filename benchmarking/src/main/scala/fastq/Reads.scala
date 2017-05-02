package fastq

import scala.language.postfixOps
import java.io.{PrintWriter, File}

import ukkonen.Lib
import Lib._

/**
  * Created by vpatryshev on 1/30/17.
  */
case class Read(id: String, value: String, strand: String, score: String) {
  override def equals(other: Any) = 
    other.isInstanceOf[Read] && other.asInstanceOf[Read].value == value
}

class Reads(val data: List[Read]) {
  def writeTo(file: File): Unit = {
    val pw = new PrintWriter(file)
    data foreach {
      r => {
        pw.println(r.id)
        pw.println(r.value)
        pw.println(r.strand)
        pw.println(r.score)
      }
    }
    pw.close()
  }
}

object Reads {
  def apply(file: File): Reads = {
    val input = openFile(file)

    val grouped: Iterator[String]#GroupedIterator[String] = input.grouped(4)
    
    val reads = grouped.collect(_.toList match {
      case id :: value :: strand:: score ::_ => Read(id, value, strand, score)
    }) toList
    
    log(s"Found ${reads.size} reads total")
    
    val sorted = reads.sortBy(_.value)
    
    val seed = sorted.head
    val uniques = ((seed::Nil) /: sorted.tail) { 
      case (out, read) => if(read.value == out.head.value) out else read::out 
    }
    
    log(s"Unique reads: ${uniques.size}")

    new Reads(uniques.sortBy(_.id))
  }
}