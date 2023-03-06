package main.scala.NumberClasses

import collection.immutable.Range.BigDecimal.bigDecAsIntegral

object FractionalFractions {

  trait FractionIsFractional[T](implicit integral : Integral[T]) extends Fractional[Fraction[T]] {
    def plus(x: Fraction[T], y: Fraction[T]): Fraction[T] = x + y
    def minus(x: Fraction[T], y: Fraction[T]): Fraction[T] = x - y
    def times(x: Fraction[T], y: Fraction[T]): Fraction[T] = x * y
    def negate(x: Fraction[T]): Fraction[T] = -x
    def fromInt(x: Int): Fraction[T] = new Fraction[T]().fromInt(x)
    def parseString(str: String): Option[Fraction[T]] = {
      val halves = str.split("/")
      if (halves.length != 2) Option.empty else {
        val numer = integral.parseString(halves.head)
        val denom = integral.parseString(halves.tail.head)
        if (numer.isEmpty || denom.isEmpty) Option.empty
        else Option.apply(new Fraction[T](numer.get,denom.get))
      }
    }
    def toInt(x: Fraction[T]): Int = x.toInt
    def toLong(x: Fraction[T]): Long = x.toLong
    def toFloat(x: Fraction[T]): Float = x.toFloat
    def toDouble(x: Fraction[T]): Double = x.toDouble
    def div(x: Fraction[T], y: Fraction[T]): Fraction[T] = x / y
    // logic in Numeric base trait mishandles abs(-0.0)
    override def abs(x: Fraction[T]): Fraction[T] = x.abs
    // logic in Numeric base trait mishandles sign(-0.0) and sign(Double.NaN)
    override def sign(x: Fraction[T]): Fraction[T] = if (x > x.zero()) x.one() else if (x < x.zero()) -x.one() else x.zero()

    override def compare(x: Fraction[T], y: Fraction[T]): Int = if (x > y) 1 else if (x < y) -1 else 0
  }

  implicit object RationalIsFractional extends FractionIsFractional[Int]

  implicit object LongRationalIsFractional extends FractionIsFractional[Long]

  implicit object BigRationalIsFractional extends FractionIsFractional[BigInt]

  implicit object ByteRationalIsFractional extends FractionIsFractional[Byte]

  implicit object CharRationalIsFractional extends FractionIsFractional[Char]

  implicit object ShortRationalIsFractional extends FractionIsFractional[Short]

  implicit object BigDecRationalIsFractional extends FractionIsFractional[BigDecimal]

}



