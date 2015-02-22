package poliglot.helpers

import org.widok._
import org.widok.bindings.Bootstrap._

trait Navigation {
  def header() = NavigationBar(
    Container(
      NavigationBar.Header(
        NavigationBar.Toggle(),
        NavigationBar.Brand("Parallel corpus editor")
      )
    )
  )
}

trait CustomPage extends Page with Navigation {
  def body(): View
  def view() = Inline(header(), Container(body()))
}
