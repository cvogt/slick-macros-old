package scala.slick

final case class table(name:String) extends StaticAnnotation
final case class column(name:String) extends StaticAnnotation
import scala.reflect.makro.Context

object Queryable{
  def apply[T:Manifest] = new Queryable[T]
  def factory[S](driver:Any, projection:Any ) : Queryable[S] = {
    println("factory")
    val q = new Queryable[S]
    println("factory done")
    q
  }
}

class UnsupportedMethodException(msg : String = "" ) extends Exception(msg)

case class Utils[C <: Context]( c:C ) {
  import c.mirror._
  import c.{Tree=>_}
  object removeDoubleReify extends c.mirror.Transformer {
    def apply( tree:Tree ) = transform(tree)
    override def transform(tree: Tree): Tree = {
      super.transform {
        tree match {
          case  //Apply( // needed to account for ApplyToImplicitArgs
                Apply(TypeApply(Select(_this, termname), _), List( _ , reified ) )
                //,_)
                if termname.toString == "factory" => c.unreifyTree(reified)
          case //Apply(
              Apply(Select(_this, termname), List( _ , reified ) )
              //,_)
              if termname.toString == "factory" => c.unreifyTree(reified)
          case _ => tree
        }
      }
    }
  }
}

object QueryableMacros{
  private def _helper[C <: Context{type PrefixType = Queryable[T]},T:c.TypeTag,S:c.TypeTag]( c:C )( name:String, projection:c.mirror.Expr[_] ) = {
    import c.mirror._
    val element_type = implicitly[TypeTag[S]].tpe

    //val qname = newTermName( "q123412341" ) //FIXME: c.fresh )
    //val qdef = Expr[Unit]( ValDef(NoMods, qname, TypeTree().setType(c.prefix.tree.tpe), c.prefix.tree) )
    //val quse = Expr[ Queryable[T] ]( Ident(qname) )
/*    println(showRaw(        Utils[c.type](c).removeDoubleReify(
          Apply(Select(c.prefix.tree, newTermName( "_"+name+"_placeholder" )), List( projection.tree ))
         ).asInstanceOf[Tree]
        ))
*/
    val reifiedExpression = Expr[reflect.mirror.Expr[Queryable[S]]](
      c.reifyTree( c.reflectMirrorPrefix, c.typeCheck(
        Utils[c.type](c).removeDoubleReify(
          Apply(Select(c.prefix.tree, newTermName( "_"+name+"_placeholder" )), List( projection.tree ))
         ).asInstanceOf[Tree]
        )
      )
    )
    c.reify{
      Queryable.factory[S](c.prefix.eval.driver, null ) // -> TestQueryable.scala:4: error: exception during macro expansion: scala.reflect.internal.Types$TypeError: value _filter_placeholder is not a member of type parameter T
      //Queryable.factory[S](c.prefix.eval.driver, reifiedExpression.eval ) // -> java.lang.ClassCastException: scala.reflect.api.Trees$ValDef cannot be cast to scala.collection.Seq
    }
  }

  def filter[T:c.TypeTag]
               (c: scala.reflect.makro.Context{type PrefixType = Queryable[T]})
               (projection: c.mirror.Expr[T => Boolean]): c.mirror.Expr[scala.slick.Queryable[T]] = _helper[c.type,T,T]( c )( "filter", projection )
}

class Queryable[T]{
  val driver = null
  def _filter_placeholder( projection: T => Boolean ) : Queryable[T] = ???
  def filter( projection: T => Boolean ) : Queryable[T] = macro QueryableMacros.filter[T]
}

