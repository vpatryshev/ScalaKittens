package scalakittens.la;

/**
 * Wrote this to make sure that trivial array-based operations are done
 * without boxing/unboxing; that's what Java is for.
 * When a vector is OnArray, and dimensions are known, there's no need
 * to be paranoid.
 * 
 * All this made Word2Vec twice faster.
 * 
 * Created by vpatryshev on 6/26/17.
 */
public class ArrayOps {
  public static void nudge(double[] xs, double[] ys, double q) {
    for (int i = 0; i < xs.length; i++) xs[i] += ys[i]*q;
  }

  public static void addTo(double[] xs, double[] ys) {
    for (int i = 0; i < xs.length; i++) xs[i] += ys[i];
  }

  public static void subtractFrom(double[] xs, double[] ys) {
    for (int i = 0; i < xs.length; i++) xs[i] -= ys[i];
  }

  public static void multBy(double[] xs, double q) {
    for (int i = 0; i < xs.length; i++) xs[i] *= q;
  }

  public static double scalarProduct(double[] xs, double[] ys) {
    double s = 0.0;
    for (int i = 0; i < xs.length; i++) s += xs[i] * ys[i];
    
    return s;
  }
}
