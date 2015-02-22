package poliglot.models

/** TODO So far only the source translations are considered.
  * TODO Also maintain language-specific rules (models.diagnostics.German)
  */
object Diagnostics {
  /* Takes translation, returns first failing entity. */
  type Diagnostic = Translation => Option[Entity]
  type Id = String
  type Description = String
  type Result = (Translation, Entity, Id)

  def tooLargeEntity(tr: Translation): Option[Entity] =
    tr.source.entities.get.find(entity =>
      entity.`class`.toOption.exists(_.nonEmpty) &&
        entity.size != 1)

  def missingDependency(tr: Translation): Option[Entity] =
    tr.source.entities.get.find(entity =>
      entity.`class`.toOption.exists(_.nonEmpty) &&
        entity.dependency.toOption.isEmpty)

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
        if (tr.disabledDiagnostics.contains(ident)) None
        else diag(tr).map(ent => (tr, ent, ident))
      }
    }
  }

  def description(id: Id): Description =
    diagnostics.find { case (_, curId, _) => id == curId }.get._3
}
