package memoize.data

import scala.reflect.Typeable
import scala.reflect.TypeTest
import java.lang.reflect.TypeVariable
import scala.reflect.Manifest

class Dynamic private(val underlying: Any, ct: Manifest[_]):
  
  def cast[A](using ca: Manifest[A]): Option[A] =
    if ct <:< ca then Some(underlying.asInstanceOf[A]) else None
  
  override def equals(x: Any): Boolean = 
    x match
      case other: Dynamic => other.underlying == underlying
      case _ => super.equals(x)

object Dynamic:  
  def apply[A](v: A)(using tta: Manifest[A]) = 
    new Dynamic(v, tta)
