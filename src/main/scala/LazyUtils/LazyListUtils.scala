package main.scala.LazyUtils

import main.scala.generic.TupleOP.unpack3

object LazyListUtils {

  /** Similar to [[scala.collection.immutable.LazyList.iterate(A, scala.Function1<A,A>)]]
   *
   * @param start  the index of the first element of the new LazyList
   * @param fst  the value of the first element of the new LazyList
   * @param f a function that takes the current index and element and generates the next element
   * @tparam V the element type of the returned LazyList
   * @return A LazyList containing the elements recursively computed with the indices by the function fun.
   *
   *         if we call l(n) the nth element of the LazyList (starting at 0), the output will be of the form
   *
   *         start, f(start, fst), f(f(start, fst), fst + 1), fun(f(f(start, fst), fst + 1), fst + 2), ....
   * @example  iterate(1, 1)(_*_) will return the list of all factorials starting at 0!
   *           i.e [1, 1, 2, 6, 24, 120, 720, ...]
   */

  def indexedIterate[U : Numeric, V](start: U, fst: V)(f : (U, V) => V) : LazyList[V] = {
    val num = implicitly[Numeric[U]]
    lazy val res: LazyList[V] = fst #:: LazyList.iterate[U](start)(num.plus(_, num.one)).zip(res).map(unpack3(f))
    res
  }


  implicit class DoubleLazyList[V](val l : LazyList[LazyList[V]]){

    /**
     * Flattens a `LazyList` of `LazyLists` into a new sorted `LazyList`.
     *
     * Each element sequence, as well as the sequence formed by the heads of the subsequences,
     * should be sorted with the same `Ordering` before calling; otherwise,
     * the results are undefined.
     *
     * The sequence should be infinite, and all subsequence should be infinite;otherwise,
     * the results are undefined.
     *
     * The operation is stable. That is, elements that are equal (as determined by
     * `ord.compare`) appear in the same order in the sorted sequence as in the original.
     *
     * @see [[scala.math.Ordering]]
     *
     * @see [[scala.collection.immutable.LazyList]]
     *
     *
     * @param ord the ordering to be used to compare elements.
     * @return a LazyList, consisting of the sorted sequence of all the elements of all element sequences
     * @note if any one element appears infinitely many times across the element sequences,
     *       then no greater element (as determined by `ord.compare`) will appear in the result.
     *
     *@note as far as stability is concerned, the order is defined first by the index of the original subsequence
     *      and second by the index in said subsequence (i.e. L.apply(0).apply(10) is considered
     *      before L.apply(1).apply(0))
     *
     * @example
     *          This example uses [[scala.math.Ordering.IntOrdering]]
     *
     *          given a `LazyList` of the form
     *          &#91;&#91;2, 4, 8, 16, 32, ...],
     *          &#91;3, 9, 27, 81, ...],
     *          &#91;5, 25, 125, 625, ...],
     *          ... ]
     *
     *          containing the integer powers of primes, it's merge would be
     *          &#91;2, 3, 4, 5, 7, 8, 9, 11, 13, 16, 17, 19, 23, 25, 27, 29, 31, 32, ...]
     *
     */

    def sortedFlatten(ord: Ordering[V]) : LazyList[V] =
      LazyList.iterate((l.head.head,
        List(l.head.drop(1), l.apply(1))))(
        x =>{
          val heads = x._2.map(_.head)
          val nextVal = heads.min(ord)
          val sourceIndex = heads.indexOf(nextVal)
          assert(sourceIndex >= 0)
          val nextList = x._2.updated(sourceIndex, x._2.apply(sourceIndex).drop(1)) ::: (
            if (sourceIndex == x._2.length - 1 ) List(l.apply(sourceIndex + 1)) else List())
          (nextVal, nextList)
        }
      ).map(_._1)
  }
}
