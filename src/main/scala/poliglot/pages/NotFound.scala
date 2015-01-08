package poliglot.pages

import org.widok._

import poliglot.helpers.CustomPage

case class NotFound() extends CustomPage {
  def body() = "Page not found."
  def ready(route: InstantiatedRoute) { }
}
