package scala.slick
object TestQueryable extends App{
  val q : Queryable[String] = Queryable[String]
  q.filter(_ == "").filter(_ == "")
}