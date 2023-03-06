package main.scala.NumberClasses

class Fraction [T](x : T, y : T)(implicit integral : Integral[T]){
  // require is used to enforce a precondition on the caller

  require(y != integral.zero, "denominator must be non-zero")

  // define a greatest common divisor method we can use to simplify Fraction[T]s
  private def gcd(a: T, b: T): T = integral.abs(if (b == integral.zero) a else gcd(b, integral.rem(a,b)))

  val g: T = gcd(x, y)

  val numer: T = integral.quot(x, g)
  val denom: T = integral.quot(y, g)

  // define a second constructor
  def this(x: T)(implicit integral : Integral[T]) = this(x, integral.one)


  def zero() : Fraction[T] = new Fraction[T](integral.zero)

  def one() : Fraction[T] = new Fraction[T](integral.one)

  //empty constructor gives 0

  def this()(implicit integral : Integral[T]) = this(integral.zero)

  // define methods on this class
  def add(r: Fraction[T]): Fraction[T] =
    new Fraction[T](integral.plus(integral.times(numer, r.denom), integral.times(r.numer, denom)),
      integral.times(denom, r.denom))

  def +(r: Fraction[T]): Fraction[T] = add(r)

  // negation
  def neg = new Fraction[T](integral.negate(numer), denom)
  def unary_- : Fraction[T] = neg

  def sub(r: Fraction[T]): Fraction[T] = add(r.neg)

  def -(r: Fraction[T]): Fraction[T] = sub(r)

  def mult(r: Fraction[T]) =
    new Fraction[T](integral.times(numer, r.numer), integral.times(denom, r.denom))

  def *(r: Fraction[T]): Fraction[T] = mult(r)

  def div(r: Fraction[T]) =
    new Fraction[T](integral.times(numer, r.denom), integral.times(denom, r.numer))

  def /(r: Fraction[T]): Fraction[T] = div(r)

  def less(r: Fraction[T]): Boolean = integral.compare(integral.times(numer, r.denom), integral.times(denom, r.numer))  < 0

  def <(r: Fraction[T]): Boolean = less(r)

  def more(r: Fraction[T]): Boolean = integral.compare(integral.times(numer, r.denom), integral.times(denom, r.numer))  > 0

  def >(r: Fraction[T]): Boolean = more(r)

  def max(r: Fraction[T]): Fraction[T] = if (less(r)) r else this

  def min(r: Fraction[T]): Fraction[T] = if (more(r)) r else this

  def inv: Fraction[T] = new Fraction[T](denom, numer)
  def unary_/ : Fraction[T] = inv

  def toDouble : Double = integral.toDouble(numer) / integral.toDouble(denom)

  def fromInt(n: Int) : Fraction[T] = new Fraction[T](integral.fromInt(n))

  override
  def toString: String = numer.toString + "/" + denom.toString
}

