package org.iyunbo.coding
package monad

trait Traverse[F[_]] extends Functor[F] { self =>

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Id[A] = A

  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A] = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idMonad)

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(
      G: Applicative[G],
      H: Applicative[H]
  ): (G[F[B]], H[F[B]]) = traverse[({ type f[x] = (G[x], H[x]) })#f, A, B](fa)(
    a => (f(a), g(a))
  )(G product H)

  def compose[H[_]](implicit
      H: Traverse[H]
  ): Traverse[({ type f[x] = F[H[x]] })#f] = new Traverse[
    ({
      type f[x] = F[H[x]]
    })#f
  ] {
    override def traverse[G[_]: Applicative, A, B](fa: F[H[A]])(
        f: A => G[B]
    ): G[F[H[B]]] = self.traverse(fa)((ha: H[A]) => H.traverse(ha)(f))
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {

  val listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(implicit
        M: Applicative[M]
    ): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[M[_], A, B](
        oa: Option[A]
    )(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
      oa match {
        case Some(a) => M.map(f(a))(Some(_))
        case None    => M.unit(None)
      }
  }

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[M[_], A, B](
        ta: Tree[A]
    )(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] =
      M.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(
        Tree(_, _)
      )
  }
}
