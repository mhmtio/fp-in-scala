package io.terrafino.fp.errorhandling

import scala.{ Option => _, Some => _, Either => _, _ } // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
