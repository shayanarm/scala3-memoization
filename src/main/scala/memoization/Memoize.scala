package memoization

import scala.collection.mutable

import scala.quoted.*

sealed trait Storage:
  def getOrEval(key: Any, eval: => Any): Any

object Storage:
  trait WeakHashMap extends Storage:
    private val memo: mutable.WeakHashMap[Any, Any] = mutable.WeakHashMap()
    override def getOrEval(key: Any, eval: => Any): Any = synchronized {
      memo.getOrElseUpdate(key, eval)
    }
  trait HashMap extends Storage:
    private val memo: mutable.HashMap[Any, Any] = mutable.HashMap()
    override def getOrEval(key: Any, eval: => Any): Any = synchronized {
      memo.getOrElseUpdate(key, eval)
    }
