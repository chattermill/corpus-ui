package poliglot.models

import org.widok._

import scala.collection.mutable
import scala.concurrent.Future

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

import poliglot.helpers.NodeJS

object Database {
  val classTags = Buffer[String]()

  def readClassTags(path: String) {
    val file = NodeJS.readFile(path)
    file.onSuccess { case f =>
      val tags = f.lines
        .map(_.trim)
        .filter(line => !line.startsWith("#") && line.nonEmpty)
        .toList
        .sorted

      classTags.clear()
      classTags ++= Buffer(tags: _*)
    }
  }

  val et = js.Dynamic.global.require("elementtree")

  def parseToken(tree: js.Object, id: Int): Token = {
    val token = tree.asInstanceOf[js.Dynamic]

    Token(
      token.findtext("./orth").asInstanceOf[String],
      token.findtext("./lemma").asInstanceOf[String],
      token.findtext("./tag").asInstanceOf[String]
    )
  }

  def encodeToken(base: js.Dynamic, tk: Token) {
    val node = et.SubElement(base, "token")
    val orth = et.SubElement(node, "orth"); orth.text = tk.orth
    val lemma = et.SubElement(node, "lemma"); lemma.text = tk.lemma
    val tag = et.SubElement(node, "tag"); tag.text = tk.tag
  }

  def parseEntity(tree: js.Object): Entity = {
    val entity = tree.asInstanceOf[js.Dynamic]

    Entity(
      id = entity.findtext("./id").asInstanceOf[String].toInt,
      parent = entity.findtext("./parent").asInstanceOf[String].toInt,
      tokens = Var(
        (entity.findtext("./startToken").asInstanceOf[String].toInt,
          entity.findtext("./endToken").asInstanceOf[String].toInt)
      ),
      dependency = UndefOr.any2undefOrA(entity.findtext("./dependency")).map(x => Opt(x.asInstanceOf[String].toInt)).getOrElse(Opt()),
      alignment = UndefOr.any2undefOrA(entity.findtext("./alignment")).map(x => Opt(x.asInstanceOf[String].toInt)).getOrElse(Opt()),
      `class` = UndefOr.any2undefOrA(entity.findtext("./class")).map(x => Opt(x.asInstanceOf[String])).getOrElse(Opt())
    )
  }

  def encodeEntity(base: js.Dynamic, ent: Entity) {
    val node = et.SubElement(base, "entity")
    val id = et.SubElement(node, "id"); id.text = ent.id.toString
    val parent = et.SubElement(node, "parent"); parent.text = ent.parent.toString
    val startToken = et.SubElement(node, "startToken"); startToken.text = ent.tokens.get._1.toString
    val endToken = et.SubElement(node, "endToken"); endToken.text = ent.tokens.get._2.toString

    ent.dependency.toOption.foreach { v =>
      val dependency = et.SubElement(node, "dependency"); dependency.text = v.toString
    }

    ent.alignment.toOption.foreach { v =>
      val alignment = et.SubElement(node, "alignment"); alignment.text = v.toString
    }

    ent.`class`.toOption.foreach { v =>
      val `class` = et.SubElement(node, "class"); `class`.text = v.toString
    }
  }

  def parseTokens(tree: js.Object): Tokens = {
    val tokens = tree.asInstanceOf[js.Dynamic]

    Tokens(
      tokens = tokens.findall("./tokens/*").asInstanceOf[js.Array[js.Object]].toList.zipWithIndex.map((parseToken _).tupled),
      entities = Buffer(tokens.findall("./entities/*").asInstanceOf[js.Array[js.Object]].toArray.map(parseEntity): _*)
    )
  }

  def parseString(tree: js.Object): String =
    tree.asInstanceOf[js.Dynamic].findtext("./").toString

  def parseTranslation(tree: js.Object): Translation = {
    val translation = tree.asInstanceOf[js.Dynamic]

    Translation(
      source = parseTokens(translation.find("./source").asInstanceOf[js.Object]),
      target = parseTokens(translation.find("./target").asInstanceOf[js.Object]),
      done = Var(UndefOr.any2undefOrA(translation.findtext("./done")).exists(_.asInstanceOf[String] == "true")),
      disabledDiagnostics = mutable.ArrayBuffer(translation.findall("./disabledDiagnostic").asInstanceOf[js.Array[js.Object]].toArray.map(parseString): _*)
    )
  }
  
  def translations(path: String): Future[Translations] = {
    NodeJS.readFile(path).map { xml =>
      val tree = et.parse(xml)

      Translations(
        translations = Buffer(tree.findall("translation").asInstanceOf[js.Array[js.Object]].toArray.map(parseTranslation): _*)
      )
    }
  }

  def encodeTokens(tks: Tokens, node: js.Dynamic) {
    val tokens = et.SubElement(node, "tokens"); tks.tokens.foreach(cur => encodeToken(tokens, cur))
    val entities = et.SubElement(node, "entities"); tks.entities.foreach { ent =>
      if (!tks.entities.get.exists(_.get.parent == ent.id)) {
        /* Reset alignment and dependency if entity has no children. */
        ent.alignment.clear()
        ent.dependency.clear()
        ent.`class`.clear()
      }

      encodeEntity(entities, ent)
    }
  }

  def encodeTranslation(translation: Translation, parent: js.Dynamic) {
    val node = et.SubElement(parent, "translation")
    val source = et.SubElement(node, "source"); encodeTokens(translation.source, source)
    val target = et.SubElement(node, "target"); encodeTokens(translation.target, target)
    val done = et.SubElement(node, "done"); done.text = if (translation.done.get) "true" else "false"
    translation.disabledDiagnostics.foreach { v =>
      val disabledDiagnostic = et.SubElement(node, "disabledDiagnostic"); disabledDiagnostic.text = v.toString
    }
  }

  def encodeTranslations(translations: Translations): js.Dynamic = {
    val node = et.Element("translations")
    translations.translations.foreach(tr => encodeTranslation(tr, node))
    node
  }

  def saveTranslations(path: String, translations: Translations): Future[Unit] = {
    val root = encodeTranslations(translations)
    val etree = js.Dynamic.newInstance(et.ElementTree)(root)
    val xml = etree.write(js.Dynamic.literal("indent" -> 2, "xml_declaration" -> false))
    NodeJS.writeFile(path, xml.asInstanceOf[String])
  }
}
