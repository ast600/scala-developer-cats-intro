package me.chuwy.otusbats

trait Monad[F[_]] extends Functor[F] {
  self =>
  def flatMapFromMonad[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  override final def mapFromFunctor[A, B](fa: F[A])(f: A => B): F[B] = {
    self.flatMapFromMonad(fa){ f andThen self.point }
  }

  def flatten[A](fa: F[F[A]]): F[A] = {
    flatMapFromMonad(fa) { identity[F[A]] }
  }
}

object Monad {
  // Без smart-конструктора, т.к. у интерфейса больше одного метода
  // Можно было бы сделать конструктор, если бы Pure/Point был вынесен в отдельный type class

  //summoner
  def apply[F[_]](implicit monad: Monad[F]): Monad[F] = monad

  // instances

  type RightBiasedEither[T] = Either[_, T]

  implicit val ListMonad: Monad[List] = new Monad[List] {
    override def flatMapFromMonad[A, B](fa: List[A])
                                       (f: A => List[B]): List[B] = fa.flatMap { f }

    override def point[A](a: A): List[A] = List(a)
  }

  implicit val OptionMonad: Monad[Option] = new Monad[Option] {
    override def flatMapFromMonad[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap { f }

    override def point[A](a: A): Option[A] = Some(a)
  }

  implicit val RightBiasedEitherMonad: Monad[RightBiasedEither] = new Monad[RightBiasedEither] {
    override def flatMapFromMonad[A, B](fa: RightBiasedEither[A])
                                       (f: A => RightBiasedEither[B]): RightBiasedEither[B] = {
      fa match {
        case Right(value) => f(value)
        case _ => fa.asInstanceOf[RightBiasedEither[B]] // left
      }
    }

    override def point[A](a: A): RightBiasedEither[A] = Right(a)
  }

  // ops

  implicit class PointSyntax[A](value: A) {
    def point[F[_]](implicit monad: Monad[F]): F[A] = monad.point(value)
  }

  implicit class MonadSyntax[F[_], A](container: F[A]) {
    def mapFromFunctor[B](f: A => B)(implicit monad: Monad[F]): F[B] = monad.mapFromFunctor(container){ f }

    def flatMapFromMonad[B](g: A => F[B])(implicit monad: Monad[F]): F[B] = monad.flatMapFromMonad(container){ g }
  }
}


