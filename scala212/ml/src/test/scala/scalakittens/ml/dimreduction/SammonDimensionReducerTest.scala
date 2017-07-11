package scalakittens.ml.dimreduction

import org.specs2.mutable.Specification

import scalakittens.la.Spaces.{R2, R3}
import math._
import scalakittens.la.Norm
import scalakittens.ml.TestTools._
import scalakittens.ml.dimreduction.Viz._

/**
  * Created by vpatryshev on 7/3/17.
  * see also https://lvdmaaten.github.io/tsne/
  */
class SammonDimensionReducerTest extends Specification {
  
  "Test figure" should {
    "be ok" in {
      val points: List[R3.Vector] = testFigure3dButterfly.toList
      val toView1: List[(Int, Double, Double)] = points.map ((v:R3.Vector) => {
        ((v.apply(2)*100).toInt, v.apply(0), v.apply(1))
      })
        
        visualize("Horizontal", toView1)

      val toView2: List[(Int, Double, Double)] = points.map ((v:R3.Vector) => {
        ((v.apply(0)*100).toInt, v.apply(1), v.apply(2))
      })

      visualize("Vertical1", toView2)

      val toView3: List[(Int, Double, Double)] = points.map ((v:R3.Vector) => {
        ((v.apply(1)*100).toInt, v.apply(0), v.apply(2))
      })

      visualize("Vertical2", toView3)
      ok
    }
  }
  
  "SammonDimensionReducer" should {

    "reduce butterfly with PCA and Sammon" in {
      val pca = new PcaDimensionReducer[R3.type, R2.type](R3, R2, precision = 0.001, 50)
      
      val sammon: DimensionReducer[R3.Vector, R2.Vector] = SammonDimensionReducer.withPCA[R3.type, R2.type](R3, R2, 300)
      
      val source = testFigure3dButterfly
      val vpca = pca.reduce(source).toList map {v => ("P", v.apply(0), v.apply(1))}
      visualize("after pca", vpca)
      
      val vsam = sammon.reduce(source).toList map {v => ("S", v.apply(0), v.apply(1))}
      visualize("after sammon", vsam)
      ok
    }

    "reduce an almost-cube withPCA" in {
      val pca = new PcaDimensionReducer[R3.type, R2.type](R3, R2, precision = 0.001, 50)

      val sammon: DimensionReducer[R3.Vector, R2.Vector] = SammonDimensionReducer.withPCA[R3.type, R2.type](R3, R2, 300)

      val source = Array(
        R3.Vector(0,0,0),
        R3.Vector(0,0,1),
        R3.Vector(0,1,0),
        R3.Vector(0,1,1),
        R3.Vector(1,0,0),
        R3.Vector(1,0,0.9),
        R3.Vector(1,0.9,0),
        R3.Vector(1,0.9,0.9)
      )
      val vpca = pca.reduce(source).toList map {v => ("P", v.apply(0), v.apply(1))}
      visualize("after pca", vpca)

      val vsam = sammon.reduce(source).toList map {v => ("S", v.apply(0), v.apply(1))}
      visualize("after sammon", vsam)
      ok
    }

    "reduce an exact cube withPCA" in {
      val pca = new PcaDimensionReducer[R3.type, R2.type](R3, R2, precision = 0.001, 50)

      val sammon: DimensionReducer[R3.Vector, R2.Vector] = SammonDimensionReducer.withPCA[R3.type, R2.type](R3, R2, 300)

      val source = Array(
        R3.Vector(0,0,0),
        R3.Vector(0,0,1),
        R3.Vector(0,1,0),
        R3.Vector(0,1,1),
        R3.Vector(1,0,0),
        R3.Vector(1,0,1),
        R3.Vector(1,1,0),
        R3.Vector(1,1,1)
      )
      val vpca = pca.reduce(source).toList map {v => ("P", v.apply(0), v.apply(1))}
      visualize("after pca", vpca)

      val vsam = sammon.reduce(source).toList map {v => ("S", v.apply(0), v.apply(1))}
      visualize("after sammon", vsam)
      ok
    }

  }
}
