package poliglot.helpers

import scala.concurrent.{Future, Promise}

import scala.scalajs.js

object NodeJS {
  val fs = js.Dynamic.global.require("fs")
  val gui = js.Dynamic.global.require("nw.gui")

  val args = gui.App.argv
    .asInstanceOf[js.Array[String]]
    .toList

  def readFile(path: String): Future[String] = {
    val p = Promise[String]()
    fs.readFile(path, "utf8", (err: String, data: String) => {
      if (err != null) p.failure(new Error(err))
      else p.success(data)
    })
    p.future
  }

  def writeFile(path: String, data: String): Future[Unit] = {
    val p = Promise[Unit]()
    fs.writeFile(path, data, (err: String) => {
      if (err != null) p.failure(new Error(err))
      else p.success(())
    })
    p.future
  }
}
