/*
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 */

package play.api.libs.functional

import scala.language.higherKinds

trait Arrow[A[_, _]] {
  arr =>

  def identity[B]: A[B, B]
  def arr[B, C](f: B => C): A[B, C]
  def compose[B, C, D](f: A[C, D], g: A[B, C]): A[B, D]

  final def >>>[B, C, D](f: A[B, C], g: A[C, D]): A[B, D] = compose(g, f)
  final def <<<[B, C, D](f: A[C, D], g: A[B, C]): A[B, D] = compose(f, g)
  final def monoid[B]: Monoid[A[B, B]] = new Monoid[A[B, B]] {
    def identity = arr.identity[B]
    def append(f: A[B, B], g: A[B, B]) = arr.compose(f, g)
  }

}

class ArrowOps[A[_, _], B, C](f: A[B, C])(implicit arr: Arrow[A]) {

  def >>>[D](g: A[C, D]): A[B, D] = arr.compose(g, f)
  def <<<[D](g: A[D, B]): A[D, C] = arr.compose(f, g)

}
