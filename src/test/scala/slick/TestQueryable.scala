package scala.slick
import language.{reflectiveCalls,implicitConversions}

object TestQueryable extends App{
  val q : Queryable[Any] = Queryable.apply
  // nesting
  q.map(e1 => q.map(e2=>e1))
}
