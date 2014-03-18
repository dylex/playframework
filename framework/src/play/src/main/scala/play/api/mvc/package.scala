/*
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package play.api

/**
 * Contains the Controller/Action/Result API to handle HTTP requests.
 *
 * For example, a typical controller:
 * {{{
 * object Application extends Controller {
 *
 *   def index = Action {
 *     Ok("It works!")
 *   }
 *
 * }
 * }}}
 */
package object mvc {

  /**
   * Alias types for Sockets
   */
  object Socket {

    /**
     * A Socket Out
     */
    type Out[A] = play.api.libs.iteratee.Iteratee[A, Unit]

  }

  import scala.language.implicitConversions
  import scala.language.higherKinds

  implicit def toGenericActionBuilder[R[_], A](invoker: HigherOrderActionFunction[Request, R, A]): GenericActionBuilder[R, A] = new GenericActionBuilder[R, A](invoker)
  implicit def toActionBuilder[R[_]](invoker: HigherOrderActionFunction[Request, R, AnyContent]): ActionBuilder[R] = new ActionBuilder[R](invoker)
  def Action[A]: HigherOrderActionFunction[Request, Request, A] = HigherOrderActionFunction.identity[A]

}
