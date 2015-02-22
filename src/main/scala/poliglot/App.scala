package poliglot

import org.widok._

object Routes {
  val corpus = Route("/", pages.Corpus)
  val notFound = Route("/404", pages.NotFound)

  val routes = Set(corpus, notFound)
}

object App extends RoutingApplication(Routes.routes, Routes.notFound)
