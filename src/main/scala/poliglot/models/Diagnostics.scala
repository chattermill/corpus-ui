package poliglot.models

import org.widok.Ref

/** TODO So far only the source translations are considered.
  * TODO Also maintain language-specific rules (models.diagnostics.German)
  */
object Diagnostics {
  /* Takes translation, returns first failing entity. */
  type Diagnostic = Translation => Option[Ref[Entity]]
  type Id = String
  type Description = String
  type Result = (Ref[Translation], Ref[Entity], Id)

  def tooLargeEntity(tr: Translation): Option[Ref[Entity]] =
    tr.source.entities.get.find(entity =>
      entity.get.`class`.toOption.exists(_.nonEmpty) &&
        entity.get.size != 1)

  def missingDependency(tr: Translation): Option[Ref[Entity]] =
    tr.source.entities.get.find(entity =>
      entity.get.`class`.toOption.exists(_.nonEmpty) &&
        entity.get.dependency.toOption.isEmpty)

  val diagnostics = Seq[(Diagnostic, Id, Description)](
    (tooLargeEntity,
      "tooLargeEntity",
      "A tagged entity may not exceed the length of one token"),

    (missingDependency,
      "missingDependency",
      "A tagged entity must have a parent")
  )

  def accumulate(translations: Translations): Seq[Result] = {
    translations.translations.get.flatMap { tr =>
      diagnostics.flatMap { case (diag, ident, _) =>
        if (tr.get.disabledDiagnostics.contains(ident)) None
        else diag(tr.get).map(ent => (tr, ent, ident))
      }
    }
  }

  def description(id: Id): Description =
    diagnostics.find { case (_, curId, _) => id == curId }.get._3
}
