package poliglot.pages

import org.scalajs.dom
import org.scalajs.dom.{KeyboardEvent, MouseEvent}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

import org.widok._
import org.widok.bindings.Bootstrap.{Glyphicon, Button}
import org.widok.bindings.{Bootstrap, HTML}

import poliglot.models
import poliglot.helpers.{NodeJS, CustomPage}
import poliglot.models.{Diagnostics, Database}

case class Corpus() extends CustomPage {
  val ctrlPressed = Var(false)
  dom.document.onkeyup = (e: KeyboardEvent) => ctrlPressed := e.ctrlKey
  dom.document.onkeydown = (e: KeyboardEvent) => ctrlPressed := e.ctrlKey

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
      res.translations.get.foreach { case Ref(trans) =>
        if (!translations.get.translations.get
          .exists(_.get.source.tokens == trans.source.tokens))
        {
          translations.get.translations += trans
          added += 1
        }
      }

      alert(s"Imported $added translations")
    }
  }

  val translations = Opt[models.Translations]()
  val translation = Opt[Ref[models.Translation]]()

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

    diagnostics.clear()
    diagnostics ++= Buffer(errors: _*)
  }

  def removeUnreachable(ents: Buffer[models.Entity]) {
    def leadsToRoot(ent: models.Entity): Boolean = {
      if (ent.parent == -1) true
      else {
        val parent = ents.get.find(_.get.id == ent.parent)
        if (parent.isEmpty) false
        else leadsToRoot(parent.get.get)
      }
    }

    val queue = Buffer[models.Entity]()
    ents.get.foreach { case entRef @ Ref(ent) =>
      if (!leadsToRoot(ent)) {
        println("found unreachable: " + ent)
        queue.append(entRef)
      }
    }

    queue.get.foreach { q =>
      ents.remove(q)
    }
  }

  def saveCorpus() {
    translations.get.translations.get.foreach { tr =>
      removeUnreachable(tr.get.source.entities)
      removeUnreachable(tr.get.target.entities)
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
                   entity: Ref[models.Entity],
                   token: Int)
  {
    if (ents.get.find(_.get.parent == entity.get.id).nonEmpty) {
      alert("The left entity has children. Delete these first.")
      return
    }

    val (parentFrom, parentTo) = entity.get.tokens.get

    entity.get.tokens := (parentFrom, token)

    ents.insertAfter(entity,
      models.Entity(
        id = nextId(ents),
        parent = entity.get.parent,
        tokens = Var((token, parentTo))
      )
    )

    saveCorpus()
  }

  def mergeEntity(ents: Buffer[models.Entity],
                  entity: Ref[models.Entity],
                  ev: MouseEvent)
  {
    if (ents.get.find(_.get.parent == entity.get.id).nonEmpty) {
      alert("The left entity has children. Delete these first.")
      return
    }

    ents.afterOption(entity).foreach { case afterRef @ Ref(after) =>
      if (ents.get.find(_.get.parent == after.id).nonEmpty) {
        alert("The right entity has children. Delete these first.")
        return
      }

      entity.get.tokens := (entity.get.tokens.get._1, after.tokens.get._2)
      ents.remove(afterRef)
    }

    saveCorpus()
  }

  def removeRecursively(ents: Buffer[models.Entity], refs: Seq[Ref[models.Entity]]) {
    val queue = Buffer[models.Entity]()
    refs.foreach { ref =>
      removeRecursively(ents, ents.get.filter(_.get.parent == ref.get.id))
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

    val elems = ents.get.filter(_.get.parent == entity.id)

    if (elems.nonEmpty) {
      if (dom.confirm("Element already expanded. Delete?"))
        removeRecursively(ents, elems)
    } else ents += models.Entity(
      id = nextId(ents),
      parent = entity.id,
      tokens = Var((entity.tokens.get._1, entity.tokens.get._2))
    )

    saveCorpus()
  }

  def nextId(ents: Buffer[models.Entity]): Int =
    ents.get.sortBy(_.get.id).last.get.id + 1

  def range(tokens: ReadChannel[(Int, Int)]): ReadBuffer[Int] =
    tokens.flatMapBuf { case (l, u) => Buffer(l until u: _*) }

  def tokenString(tks: models.Tokens,
                  ipt: ReadChannel[(Int, Int)]): ReadChannel[String] =
    ipt.map { case (l, u) =>
      (l until u).map(id => tks.tokens(id).orth).mkString(" ")
    }

  def viewTokens(tks: models.Tokens,
                 entity: Ref[models.Entity],
                 tokens: ReadChannel[(Int, Int)])
    : ReadBuffer[HTML.Container.Inline] =
  {
    range(tokens).map { case Ref(token) =>
      HTML.Container.Inline(
        HTML.Container.Inline()
          .css("token-separator")
          .show(ctrlPressed.map(_ && entity.get.tokens.get._1 != token))
          .cursor(HTML.Cursor.Pointer)
          .bindMouse(Event.Mouse.Click,
            (_: MouseEvent) => splitEntity(tks.entities, entity, token))

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
      children.map { case entityRef @ Ref(entity) =>
        HTML.Container.Inline(
          viewTokens(tks, entityRef, entity.tokens)

          , HTML.Container.Inline()
            .show(children.isLast(entityRef).map(!_))
            .bindMouse(Event.Mouse.Click,
              (ev: MouseEvent) => mergeEntity(tks.entities, entityRef, ev))
            .css("entity-merge")
            .css("entity-separator")
        ).css("disable-selection")
         .bindMouse(Event.Mouse.DoubleClick,
            (ev: MouseEvent) => expandEntity(tks.entities, children, entity))
      }
    )
  }

  def entitiesWithChildren(ents: ReadBuffer[models.Entity]): ReadBuffer[models.Entity] =
    ents.flatMapCh(entity =>
      ents
        .exists(_.parent == entity.get.id)
        .map(if (_) Some(entity) else None))

  def viewEntityTree(tks: models.Tokens,
                     other: models.Tokens,
                     entityId: Int): HTML.List.Item =
  {
    val children = tks.entities.filter(_.parent == entityId)

    val select = if (entityId != -1) {
      val entity = tks.entities.get.find(_.get.id == entityId).get.get

      entity.`class`.values.tail.attach { _ => saveCorpus() }
      entity.alignment.values.tail.attach { _ => saveCorpus() }
      entity.dependency.values.tail.attach { _ => saveCorpus() }

      val alignment = entity.alignment.biMap[Option[Ref[models.Entity]]](
        id => other.entities.get.find(_.get.id == id),
        entity => if (entity.isDefined) entity.get.get.id else -1)

      val dependency = entity.dependency.biMap[Option[Ref[models.Entity]]](
        id => tks.entities.get.find(_.get.id == id),
        entity => if (entity.isDefined) entity.get.get.id else -1)

      val `class` = entity.`class`.biMap[Option[Ref[String]]](
        tag => Database.classTags.get.find(_.get == tag),
        ref => if (ref.isDefined) ref.get.get else "")

      val alignments = entitiesWithChildren(other.entities)
        .mapToCh[String](ent => tokenString(other, ent.tokens).map(Some(_)))

      val dependencies = entitiesWithChildren(tks.entities)
        .filter(cur => cur.id != entityId)
        .mapToCh[String](ent => tokenString(tks, ent.tokens).map(Some(_)))

      Inline(
        HTML.Input.Select().default("").bind(alignments, alignment)
        , HTML.Input.Select().default("").bind(dependencies, dependency)
        , HTML.Input.Select().default("").bind(Database.classTags.mapTo(t => t), `class`))
    } else Inline()

    HTML.List.Item(
      viewEntity(tks, children)
      , select
      , HTML.List.Unordered(
        children.map { child =>
          viewEntityTree(tks, other, child.get.id)
        }
      )
    ).show(children.nonEmpty)
     .asInstanceOf[HTML.List.Item] // TODO Widok fix
  }

  def body() = Inline(
    Bootstrap.Lead(
      HTML.Raw("<b>Usage:</b> Press <kbd>Ctrl</kbd> and select the dotted marker to create a new entity. Click a marker to connect two adjacent entities. Double-click an entity to expand it. If an entity is already expanded, double-clicking will remove it instead. Changes are saved automatically.")
    )

    , HTML.Text.Bold("Database: "), HTML.Input.File().accept(".xml").bind(dbPath), HTML.LineBreak() // TODO Provide two-way binding
    , HTML.Text.Bold("Import dump: "), HTML.Input.File().accept(".xml").bind(importPath), HTML.LineBreak()

    , translation.map { case Ref(tr) =>
      HTML.Container.Generic(
        HTML.List.Unordered(viewEntityTree(tr.source, tr.target, -1))
        , HTML.List.Unordered(viewEntityTree(tr.target, tr.source, -1))
      ).css("tree")
    }

    , Button(Glyphicon.Ok)("Diagnostics")
      .bind { (_: Unit) => loadDiagnostics() }
      .show(translation.nonEmpty)

    , Button(Glyphicon.Ok)("Mark as (un)done").bind { (_: Unit) =>
      translation.get.get.done := !translation.get.get.done.get
      saveCorpus()
    }.show(translation.nonEmpty)

    , Button(Glyphicon.ArrowLeft)("Previous").bind((_: Unit) => prev())
      .show(translation.flatMap(tr => translations.flatMap(_.translations.isHead(tr))).map(!_)) // TODO shorter
      .show(translation.nonEmpty)

    , Button(Glyphicon.ArrowRight)("Next").bind((_: Unit) => next())
      .show(translation.flatMap(tr => translations.flatMap(_.translations.isLast(tr))).map(!_)) // TODO shorter
      .show(translation.nonEmpty)

    , HTML.LineBreak()

    , translations.flatMapBuf(_.translations).map { tr =>
      val idx = translations.get.translations.indexOf(tr)
      Button(Glyphicon.None, Button.Size.ExtraSmall)(
        translation.equal(tr).map(
          if (_) HTML.Text.Bold((idx + 1).toString) // TODO without .toString
          else HTML.Text((idx + 1).toString))
      ).cssCh(tr.get.done.map(if (_) "btn-success" else "btn-danger"))
       .cssCh(translation.equal(tr), "active")
       .bind((_: Unit) => translation := tr)
    }

    , HTML.LineBreak()

    , HTML.List.Unordered().bind(diagnostics) { case ref @ Ref((tr, ent, id)) =>
      HTML.List.Item(
        HTML.Text.Bold(
          (translations.get.translations.indexOf(tr) + 1).toString + ": ")
        , Diagnostics.description(id)
        , Button(Glyphicon.Book)("Load").bind((_: Unit) => translation := tr)
        , Button(Glyphicon.Book)("Ignore").bind { (_: Unit) =>
          tr.get.disabledDiagnostics += id
          diagnostics.remove(ref)
          saveCorpus()
        }
      )
    }
  )

  def prev() {
    val prev = translations.get.translations.beforeOption(translation.get)
    translation.flatProduce(prev)
  }

  def next() {
    val next = translations.get.translations.afterOption(translation.get)
    translation.flatProduce(next)
  }

  translation.attach { case Ref(att) =>
    if (att.source.entities.get.isEmpty)
      att.source.entities +=
        models.Entity(0, -1, Var((0, att.source.tokens.length)))

    if (att.target.entities.get.isEmpty)
      att.target.entities +=
        models.Entity(0, -1, Var((0, att.target.tokens.length)))
  }

  def ready(route: InstantiatedRoute) { }
}
