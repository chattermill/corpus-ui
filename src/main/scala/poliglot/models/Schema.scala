package poliglot.models

import org.widok.{Buffer, Opt, Var}

import scala.collection.mutable

class Token(val orth: String,
            val lemma: String,
            val tag: String)

class Entity(val id: Int,
             val parent: Int,
             val tokens: Var[(Int, Int)],
             val dependency: Opt[Int] = Opt(), // ID this entity semantically depends on
             val alignment: Opt[Int] = Opt(), // ID of target entity
             val `class`: Opt[String] = Opt())
{
  def size: Int =
    tokens.get match { case (l, u) => u - l }
}

class Tokens(val tokens: Seq[Token],
             val entities: Buffer[Entity])

class Translation(val source: Tokens,
                  val target: Tokens,
                  val done: Var[Boolean],
                  val disabledDiagnostics: mutable.ArrayBuffer[String])

class Translations(val translations: Buffer[Translation])
