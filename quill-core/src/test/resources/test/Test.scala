package test

import io.getquill.MirrorContext
import io.getquill.MirrorIdiom
import io.getquill.SnakeCase
import io.getquill.context.mirror.Row
import scala.reflect.ClassTag

class PersonId(val id: Int) extends AnyVal

object Test extends App {
  
  val ctx = new MirrorContext[MirrorIdiom, SnakeCase]
  import ctx._
  
  case class Person(id: PersonId, name: String, age: Int)
  
  val person = Person(new PersonId(0), "a", 1)
  
  val q = quote((p: Person) => query[Person].insert(p).returning(_.id))
  
  println(run(q(lift(person))).extractor(Row(1)))
//  
//  run(query[Person].map(_.age))
//  
//  
//  def ins[T: ClassTag : QueryMeta] = {
//    run(query[T])
//  }
//  
//  println(ins[Person].string)
}