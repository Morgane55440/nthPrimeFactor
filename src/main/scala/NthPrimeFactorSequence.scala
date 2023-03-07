package main.scala

import main.scala.LazyUtils.LazyListUtils
import main.scala.NumberClasses.Fraction
import main.scala.LazyUtils.LazyListUtils.DoubleLazyList
import main.scala.NumberClasses.FractionsAreFractional.RationalIsFractional


object NthPrimeFactorSequence {

  def main(argv: Array[String]): Unit = {

    lazy val primes : LazyList[BigInt] = 2 #:: LazyList.from(0).map(n => {
      var k = primes.apply(n) + 1
      val prev = primes.take(n).toList
      while(prev.exists(p => k % p == 0)) k += 1
      k
    });

    lazy val primeIndex : LazyList[BigInt] = LazyListUtils.indexedIterate(0, BigInt.int2bigInt(0))((a, b) => BigInt.int2bigInt(b.toInt + (if (primes.apply(b.toInt) <= a) 1 else 0)))

    val powersOfTwo = LazyList.iterate(1)(_ * 2)

    class Decomposition(d : Int , seq : List[BigInt]){
      val depth: BigInt = d
      val sequence: List[BigInt] = seq
      require(depth >= 1)
      require(sequence.length >= depth)
      val head: BigInt = sequence.apply((depth - 1).toInt)
      val size : Fraction[BigInt]= new Fraction[BigInt](sequence.product, powersOfTwo.apply(d))
    }

    lazy val primeDec : LazyList[List[BigInt]] = List() #:: List() #:: LazyList.from(2).map(n =>{
      val prime = primes.find(p => n % p  == 0).get
      prime :: primeDec.apply((n / prime).toInt)
    })
    def primesGreaterThan(n : BigInt) : LazyList[BigInt] = primes.drop(primes.indexWhere(_ >= n))

    val oddPrimes : LazyList[BigInt] = primes.drop(1)


    lazy val oddPrimeDecByLengthBySmallestPrime : LazyList[LazyList[LazyList[List[BigInt]]]] = oddPrimes.map(primesGreaterThan(_).map(
      List(_))) #:: oddPrimeDecByLengthBySmallestPrime.map(curLength =>
      curLength.map(l => primesGreaterThan(l.head.head : BigInt).map(p => curLength.apply(
        primeIndex.apply(p.toInt).toInt - 1).map(p +: _)).sortedFlatten(Ordering.fromLessThan[List[BigInt]](_.product < _.product))))


    lazy val oddPrimeDecByLength : LazyList[LazyList[List[BigInt]]] =  oddPrimeDecByLengthBySmallestPrime.map(_.head)

    val oddPrimeDecMinLength : LazyList[LazyList[List[BigInt]]] = LazyList.from(0).map(n => List(BigInt.int2bigInt(n))) #:: LazyList.from(0).map(n => oddPrimeDecByLength.drop(n).sortedFlatten(Ordering.fromLessThan[List[BigInt]](_.product < _.product)))

    lazy val sequencesByIndex : LazyList[LazyList[Decomposition]] = primeDec.drop(2).map(
      new Decomposition(1, _)) #:: oddPrimeDecMinLength.apply(2).map(new Decomposition(2, _)
    ) #:: LazyList.from(3).map(n => oddPrimeDecMinLength.apply(n).map(new Decomposition(n, _)))


    val mergedSequences =  sequencesByIndex.sortedFlatten(Ordering.fromLessThan[Decomposition](_.size < _.size))

    val TheSequence = mergedSequences.map(_.head)
    println(TheSequence.take(1000).mkString("[",", ","]"))



    for (i : Int <- 0 to 100000){
      var fst = mergedSequences.apply(i)
      var snd = mergedSequences.apply(i + 1)
      require(fst.size < snd.size, "this : " + fst.sequence + " depth=" + fst.depth + " should be smaller than this : " + snd.sequence+ " depth=" + snd.depth)
    }

  }

}
