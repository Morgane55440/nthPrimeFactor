package main.scala.generic

object TupleOP {

  def unpack3[U,V,W](fun : (U,V) => W) : ((U, V)) => W  = x  => fun(x._1, x._2)
  def unpack4[U,V,W,X](fun : (U,V,W) => X) : ((U, V, W)) => X  = x  => fun(x._1, x._2, x._3)
  def unpack5[U,V,W,X,Y](fun : (U,V,W,X) => Y) : ((U, V, W, X)) => Y  = x  => fun(x._1, x._2, x._3, x._4)

  def pack3[U,V,W](fun : ((U, V)) => W) : (U,V) => W  = (a, b)  => fun((a,b))
  def pack4[U,V,W,X](fun : ((U, V, W)) => X) : (U,V,W) => X  = (a, b, c)  => fun((a,b,c))
  def pack5[U,V,W,X,Y](fun : ((U, V, W, X)) => Y) : (U,V,W,X) => Y  = (a, b, c, d)  => fun((a,b,c,d))

  def tupleMerge3r[U,V,W](a : (U,V), b : W) : (U,V,W) = (a._1, a._2, b)
  def tupleMerge3l[U,V,W](a : U, b : (V,W)) : (U,V,W) = (a, b._1, b._2)
  def tupleMerge4r[U,V,W,X](a : (U,V,W), b : X) : (U,V,W,X) = (a._1, a._2, a._3, b)
  def tupleMerge4l[U,V,W,X](a : U, b : (V,W,X)) : (U,V,W,X) = (a, b._1, b._2, b._3)
  def tupleMerge4m[U,V,W,X](a : (U,V), b : (W,X)) : (U,V,W,X) = (a._1, a._2, b._1, b._2)

}
