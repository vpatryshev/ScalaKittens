package samples
//http://java-performance.info/jmh/

class Sample {

//  @Benchmark
  def range(): Int =
    1.to(1000)
      .filter(_ % 2 == 0)
      .count(_.toString.length == 4)

//  @Benchmark
  def iterator(): Int =
    Iterator.from(1)
      .takeWhile(_ < 1000)
      .filter(_ % 2 == 0)
      .count(_.toString.length == 4)
}
