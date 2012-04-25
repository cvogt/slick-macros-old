package scala.slick

final case class table(name:String) extends StaticAnnotation
final case class column(name:String) extends StaticAnnotation
import scala.reflect.makro.Context

object Queryable{
  def apply = new Queryable[Any](null)
  def factory[S]( projection:scala.reflect.mirror.Expr[Any] ) : Any = {
    assert( ! scala.reflect.mirror.showRaw(projection.tree).contains( """newTermName("$mr")""" ) )
    null
  }
}

case class Utils[C <: Context]( c:C ) {
  import c.mirror._
  import c.{Tree=>_}
  object removeDoubleReify extends c.mirror.Transformer {
    def apply( tree:Tree ) = transform(tree)
    override def transform(tree: Tree): Tree = {
      super.transform {
        tree match {
          case  //Apply( // needed to account for ApplyToImplicitArgs
                Apply(TypeApply(Select(_this, termname), _), reified::Nil )
                //,_)
                if termname.toString == "factory" => c.unreifyTree(reified)
          case //Apply(
              Apply(Select(_this, termname), reified::Nil )
              //,_)
              if termname.toString == "factory" => c.unreifyTree(reified) 
          case _ => tree
        }
      }
    }
  }
}

object QueryableMacros{
  private def _helper[C <: Context,S:c.TypeTag]( c:C )( name:String, projection:c.mirror.Expr[_] ) = {
    import c.mirror._
    val reifiedExpression = Expr[reflect.mirror.Expr[Queryable[S]]](
    c.reifyTree( c.reflectMirrorPrefix, c.typeCheck(
      Utils[c.type](c).removeDoubleReify(
        Apply(Select(c.prefix.tree, newTermName( "_"+name+"_placeholder" )), List( projection.tree ))
       ).asInstanceOf[Tree]
      )))
    val res = c.reify{ Queryable.factory[S]( reifiedExpression.eval )}
    println(showRaw(res.tree.asInstanceOf[c.mirror.Tree]))
    res
  }

  def map[T:c.TypeTag, S:c.TypeTag]
               (c: scala.reflect.makro.Context)
               (projection: c.mirror.Expr[T => S]): c.mirror.Expr[Any] = _helper[c.type,S]( c )( "map", projection )
}

class Queryable[T]( protected[slick] val query:Any ){
  def _map_placeholder[S]( projection: T => S ) : Any = ???
  def map[S]( projection: T => S ) : Any = macro QueryableMacros.map[T,S]
}

