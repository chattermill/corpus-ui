package poliglot.models

import org.widok.{Buffer, Opt, Var}

import scala.collection.mutable

case class Token(orth: String, lemma: String, tag: String)

case class Entity(id: Int,
                  parent: Int,
                  tokens: Var[(Int, Int)],
                  dependency: Opt[Int] = Opt(), // ID this entity semantically depends on
                  alignment: Opt[Int] = Opt(), // ID of target entity
                  `class`: Opt[String] = Opt())
{
  def size: Int =
    tokens.get match { case (l, u) => u - l }
}

case class Tokens(tokens: Seq[Token], entities: Buffer[Entity])

case class Translation(source: Tokens,
                       target: Tokens,
                       done: Var[Boolean],
                       disabledDiagnostics: mutable.ArrayBuffer[String])

case class Translations(translations: Buffer[Translation])
