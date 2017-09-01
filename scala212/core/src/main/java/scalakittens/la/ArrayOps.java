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

  public abstract static class Folding2 {
    abstract public double op(double x, double y);

    public double fold(double[] first, double[] second) {
      double s = 0.0;
      for (int i = 0; i < first.length; i++) {
        s += op(first[i], second[i]);
      }
      return s;
    }

    public double foldUpTo(double[] first, double[] second, double limit) {
      double s = 0.0;
      for (int i = 0; i < first.length && s < limit; i++) {
        s += op(first[i], second[i]);
      }
      return s;
    }
  }

  static private final Folding2 product = new Folding2() {
    public double op(double x, double y) { return x*y; }
  };

  public static double scalarProduct(double[] xs, double[] ys) {
    return product.fold(xs, ys);
  }
  
  static private final Folding2 l2distance = new Folding2() {
    public double op(double x, double y) {
      double d = x - y;
      return d * d;
    }
  };
  
  public static double l2(double[] xs) {
    return Math.sqrt(scalarProduct(xs, xs));
  }
  
  public static double l2(double[] xs, double[] ys) {
    final double squareDistance = l2distance.fold(xs, ys);return Math.sqrt(squareDistance);
  }
  
  public static final Folding2 sammonErrorMeasure = new Folding2() {
    public double op(double x, double y) {
      double d = x - y;
      return y == 0.0 ? 0.0 : d * d / y; // zeroes are on diagonal
    }
  };
}
