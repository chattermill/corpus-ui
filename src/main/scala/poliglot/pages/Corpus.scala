package poliglot.pages

import org.scalajs.dom
import org.scalajs.dom.MouseEvent

import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

import org.widok._
import org.widok.bindings.HTML
import org.widok.bindings.Bootstrap._

import poliglot.models
import poliglot.helpers.{NodeJS, CustomPage}
import poliglot.models.{Diagnostics, Database}

case class Corpus() extends CustomPage {
  val ctrlPressed = Var(false)
  ctrlPressed << Document.keyDown.merge(Document.keyUp).map(_.ctrlKey)

  val diagnostics = Buffer[Diagnostics.Result]()

  val dbPath = Opt[String]()
  dbPath.flatProduce(NodeJS.args.headOption)
  dbPath.attach(_ => loadCorpus())

  val classSetPath = Opt[String]()
  classSetPath.flatProduce(NodeJS.args.tail.headOption)
  classSetPath.attach(p => Database.readClassTags(p))

  val importPath = Opt[String]()
  importPath.attach { path =>
    val tr = Database.translations(path)

    tr.onSuccess { case res =>
      var added = 0
      res.translations.get.foreach { trans =>
        if (!translations.get.translations.get
          .exists(_.source.tokens == trans.source.tokens))
        {
          translations.get.translations += trans
          added += 1
        }
      }

      alert(s"Imported $added translations")
    }
  }

  val translations = Opt[models.Translations]()
  val translation = Opt[models.Translation]()

  def alert(str: String) {
    ctrlPressed := false /* CTRL does not get released otherwise. */
    dom.alert(str)
  }

  def loadCorpus() {
    val tr = Database.translations(dbPath.get)

    tr.onFailure { case e =>
      alert("Database could not be loaded.")
      println(e)
    }

    tr.onSuccess { case res =>
      translations := res
      translation.flatProduce(res.translations.get.headOption)
    }
  }

  def loadDiagnostics() {
    val errors = Diagnostics.accumulate(translations.get)
    diagnostics.set(errors)
  }

  def removeUnreachable(ents: Buffer[models.Entity]) {
    def leadsToRoot(ent: models.Entity): Boolean = {
      if (ent.parent == -1) true
      else {
        val parent = ents.get.find(_.id == ent.parent)
        if (parent.isEmpty) false
        else leadsToRoot(parent.get)
      }
    }

    val queue = Buffer[models.Entity]()
    ents.get.foreach { ent =>
      if (!leadsToRoot(ent)) {
        println(s"Found unreachable: $ent")
        queue.append(ent)
      }
    }

    queue.get.foreach { q =>
      ents.remove(q)
    }
  }

  def saveCorpus() {
    translations.get.translations.get.foreach { tr =>
      removeUnreachable(tr.source.entities)
      removeUnreachable(tr.target.entities)
    }

    val tr = Database.saveTranslations(dbPath.get, translations.get)

    tr.onFailure { case e =>
      alert("Database could not be saved.")
      println(e)
    }

    tr.onSuccess { case _ => println("Database saved.") }

    if (diagnostics.get.nonEmpty) loadDiagnostics()
  }

  def splitEntity(ents: Buffer[models.Entity],
                   entity: models.Entity,
                   token: Int)
  {
    if (ents.exists$(_.parent == entity.id)) {
      alert("The left entity has children. Delete these first.")
      return
    }

    val (parentFrom, parentTo) = entity.tokens.get

    entity.tokens := (parentFrom, token)

    ents.insertAfter(entity,
      new models.Entity(
        id = nextId(ents),
        parent = entity.parent,
        tokens = Var((token, parentTo))
      )
    )

    saveCorpus()
  }

  def mergeEntity(ents: Buffer[models.Entity],
                  entity: models.Entity,
                  ev: MouseEvent)
  {
    if (ents.exists$(_.parent == entity.id)) {
      alert("The left entity has children. Delete these first.")
      return
    }

    ents.afterOption$(entity).foreach { after =>
      if (ents.exists$(_.parent == after.id)) {
        alert("The right entity has children. Delete these first.")
        return
      }

      entity.tokens := (entity.tokens.get._1, after.tokens.get._2)
      ents.remove(after)
    }

    saveCorpus()
  }

  def removeRecursively(ents: Buffer[models.Entity], refs: Seq[models.Entity]) {
    val queue = Buffer[models.Entity]()
    refs.foreach { ref =>
      removeRecursively(ents, ents.filter$(_.parent == ref.id).get)
      queue.append(ref)
    }
    queue.get.foreach(ents.remove)
  }

  def expandEntity(ents: Buffer[models.Entity],
                   children: ReadBuffer[models.Entity],
                   entity: models.Entity)
  {
    if (children.get.size == 1) {
      alert("Only sub-entities can be expanded.")
      return
    }

    val elems = ents.get.filter(_.parent == entity.id)

    if (elems.nonEmpty) {
      if (dom.confirm("Element already expanded. Delete?"))
        removeRecursively(ents, elems)
    } else ents += new models.Entity(
      id = nextId(ents),
      parent = entity.id,
      tokens = Var((entity.tokens.get._1, entity.tokens.get._2))
    )

    saveCorpus()
  }

  def nextId(ents: Buffer[models.Entity]): Int =
    ents.get.sortBy(_.id).last.id + 1

  def range(tokens: ReadChannel[(Int, Int)]): ReadBuffer[Int] =
    tokens.flatMapBuf { case (l, u) => Buffer(l until u: _*) }

  def tokenString(tks: models.Tokens,
                  ipt: (Int, Int)): String =
    ipt match { case (l, u) =>
      (l until u).map(id => tks.tokens(id).orth).mkString(" ")
    }

  def viewTokens(tks: models.Tokens,
                 entity: models.Entity,
                 tokens: ReadChannel[(Int, Int)])
    : DeltaBuffer[HTML.Container.Inline] =
  {
    range(tokens).map { token =>
      HTML.Container.Inline(
        HTML.Container.Inline()
          .css("token-separator")
          .show(ctrlPressed.map(_ && entity.tokens.get._1 != token))
          .cursor(HTML.Cursor.Pointer)
          .onClick(_ => splitEntity(tks.entities, entity, token))

        , HTML.Container.Inline(tks.tokens(token).orth)
          .attribute("data-hint", {
            val tk = tks.tokens(token)
            s"${tk.tag} (${tk.lemma})"
          })
          .css("token", "hint--top")
      )
    }
  }

  def viewEntity(tks: models.Tokens, children: ReadBuffer[models.Entity]) = {
    HTML.Anchor(
      children.map { entity =>
        HTML.Container.Inline(
          viewTokens(tks, entity, entity.tokens)

          , HTML.Container.Inline()
            .show(children.isLast(entity).map(!_))
            .onClick(ev => mergeEntity(tks.entities, entity, ev))
            .css("entity-merge")
            .css("entity-separator")
        ).css("disable-selection")
         .onDoubleClick(ev => expandEntity(tks.entities, children, entity))
      }
    )
  }

  def entitiesWithChildren(ents: ReadBuffer[models.Entity]): ReadBuffer[models.Entity] =
    ents.flatMapCh { entity =>
      ents
        .exists(_.parent == entity.id)
        .partialMap { case true => entity }
    }

  def viewEntityTree(tks: models.Tokens,
                     other: models.Tokens,
                     entityId: Int): HTML.List.Item =
  {
    val children = tks.entities.filter(_.parent == entityId).buffer

    val select = if (entityId != -1) {
      val entity = tks.entities.get.find(_.id == entityId).get

      entity.`class`.values.tail.attach { _ => saveCorpus() }
      entity.alignment.values.tail.attach { _ => saveCorpus() }
      entity.dependency.values.tail.attach { _ => saveCorpus() }

      val alignments = entitiesWithChildren(other.entities)
        .flatMap(e => e.tokens.map(rng => (e, tokenString(other, rng))).toBuffer)

      val dependencies = entitiesWithChildren(tks.entities)
        .filter(cur => cur.id != entityId)
        .buffer
        .flatMap(e => e.tokens.map(rng => (e, tokenString(tks, rng))).toBuffer)

      val alignment = entity.alignment.biMap[Option[(models.Entity, String)]](
        id => alignments.find$(_._1.id == id),
        entity => if (entity.isDefined) entity.get._1.id else -1)

      val dependency = entity.dependency.biMap[Option[(models.Entity, String)]](
        id => dependencies.find$(_._1.id == id),
        entity => if (entity.isDefined) entity.get._1.id else -1)

      val `class` = entity.`class`.biMap[Option[String]](
        tag => Database.classTags.get.find(_ == tag),
        ref => if (ref.isDefined) ref.get else "")

      def fmt(e: (models.Entity, String)) = e._2

      Inline(
        HTML.Input.Select().default("").bind(alignments, fmt, alignment)
        , HTML.Input.Select().default("").bind(dependencies, fmt, dependency)
        , HTML.Input.Select().default("").bind(Database.classTags, identity[String], `class`))
    } else Inline()

    HTML.List.Item(
      viewEntity(tks, children)
      , select
      , HTML.List.Unordered(
        children.map { child =>
          viewEntityTree(tks, other, child.id)
        }
      )
    ).show(children.nonEmpty)
  }

  def body() = Inline(
    Lead(
      HTML.Raw("<b>Usage:</b> Press <kbd>Ctrl</kbd> and select the dotted marker to create a new entity. Click a marker to connect two adjacent entities. Double-click an entity to expand it. If an entity is already expanded, double-clicking will remove it instead. Changes are saved automatically.")
    )

    , HTML.Text.Bold("Database: "), HTML.Input.File().accept(".xml").bind(dbPath), HTML.LineBreak() // TODO Provide two-way binding
    , HTML.Text.Bold("Import dump: "), HTML.Input.File().accept(".xml").bind(importPath), HTML.LineBreak()

    , translation.map { tr =>
      HTML.Container.Generic(
        HTML.List.Unordered(viewEntityTree(tr.source, tr.target, -1))
        , HTML.List.Unordered(viewEntityTree(tr.target, tr.source, -1))
      ).css("tree")
    }

    , Button(Glyphicon.Ok(), " Diagnostics")
      .onClick(_ => loadDiagnostics())
      .show(translation.nonEmpty)

    , Button(Glyphicon.Ok(), " Mark as (un)done").onClick { _ =>
      translation.get.done := !translation.get.done.get
      saveCorpus()
    }.show(translation.nonEmpty)

    , Button(Glyphicon.ArrowLeft(), " Previous").onClick(_ => prev())
      .show(translation.flatMap(tr => translations.flatMap(_.translations.isHead(tr))).map(!_))
      .show(translation.nonEmpty)

    , Button(Glyphicon.ArrowRight(), " Next").onClick(_ => next())
      .show(translation.flatMap(tr => translations.flatMap(_.translations.isLast(tr))).map(!_))
      .show(translation.nonEmpty)

    , HTML.LineBreak()

    , translations.flatMapBuf(_.translations).map { tr =>
      val idx = translations.get.translations.indexOf(tr)
      Button(
        translation.is(tr).map[Widget[_]](
          if (_) HTML.Text.Bold(idx + 1)
          else idx + 1)
      ).size(Size.ExtraSmall)
       .cssState(tr.done, "btn-success")
       .cssState(tr.done.map(!_), "btn-danger")
       .cssState(translation.is(tr), "active")
       .onClick(_ => translation := tr)
    }

    , HTML.LineBreak()

    , HTML.List.Unordered(diagnostics.map { case ref @ (tr, ent, id) =>
      HTML.List.Item(
        HTML.Text.Bold(
          (translations.get.translations.indexOf(tr) + 1).toString + ": ")
        , Diagnostics.description(id)
        , Button(Glyphicon.Book(), " Load").onClick(_ => translation := tr)
        , Button(Glyphicon.Book(), " Ignore").onClick { _ =>
          tr.disabledDiagnostics += id
          diagnostics.remove(ref)
          saveCorpus()
        }
      )
    })
  )

  def prev() {
    val prev = translations.get.translations.beforeOption$(translation.get)
    translation.flatProduce(prev)
  }

  def next() {
    val next = translations.get.translations.afterOption$(translation.get)
    translation.flatProduce(next)
  }

  translation.attach { att =>
    if (att.source.entities.get.isEmpty)
      att.source.entities +=
        new models.Entity(0, -1, Var((0, att.source.tokens.length)))

    if (att.target.entities.get.isEmpty)
      att.target.entities +=
        new models.Entity(0, -1, Var((0, att.target.tokens.length)))
  }

  def ready(route: InstantiatedRoute) { }
}
