package io.getquill.dsl

import io.getquill.util.Messages._
import scala.reflect.macros.whitebox.{ Context => MacroContext }

class MetaDslMacro(val c: MacroContext) {
  import c.universe._

  def materializeQueryMeta[T](implicit t: WeakTypeTag[T]): Tree = {
    val props = expandProperties[T]
    q"""
      new ${c.prefix}.QueryMeta[$t] {
        override val expand = ${expandQuery[T](props)}
        override val extract = ${extract[T](props)}
      }
    """
  }

  def materializeUpdateMeta[T](implicit t: WeakTypeTag[T]): Tree =
    q"""
      new ${c.prefix}.UpdateMeta[$t] {
        override val expand = ${expandAction[T]("update")}
      }
    """

  def materializeInsertMeta[T](implicit t: WeakTypeTag[T]): Tree =
    q"""
      new ${c.prefix}.InsertMeta[$t] {
        override val expand = ${expandAction[T]("insert")}
      }
    """

  private def expandQuery[T](props: List[Symbol])(implicit t: WeakTypeTag[T]) = {
    val terms = props.map(_.name.toTermName)
    val elements =
      terms.map { term =>
        q"x.$term"
      }
    q"${c.prefix}.quote((q: ${c.prefix}.Query[$t]) => q.map(x => (..$elements)))"
  }

  private def extract[T](props: List[Symbol])(implicit t: WeakTypeTag[T]) = {
    val params =
      props.zipWithIndex.map {
        case (prop, idx) =>
          q"implicitly[${c.prefix}.Decoder[${prop.typeSignature}]].apply($idx, row)"
      }
    q"(row: ${c.prefix}.ResultRow) => new $t(..$params)"
  }

  private def expandAction[T](method: String)(implicit t: WeakTypeTag[T]): Tree = {
    val terms = expandProperties[T].map(_.name.toTermName)
    val assignments =
      terms.map { term =>
        q"(v: $t) => v.$term -> value.$term"
      }
    q"${c.prefix}.quote((q: ${c.prefix}.EntityQuery[$t], value: $t) => q.${TermName(method)}(..$assignments))"
  }

  private def expandProperties[T](implicit t: WeakTypeTag[T]) =
    caseClassConstructor(t.tpe) match {
      case None              => c.fail("Can't expand a non-case class")
      case Some(constructor) => constructor.paramLists.flatten
    }

  private def caseClassConstructor(t: Type) =
    t.members.collect {
      case m: MethodSymbol if (m.isPrimaryConstructor) => m
    }.headOption
}