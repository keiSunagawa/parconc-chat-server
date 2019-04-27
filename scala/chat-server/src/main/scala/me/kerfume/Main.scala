package me.kerfume

import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.clock._
import ClientHandle.StubHandle

object Main extends App {
  import Chat.Helper._

  def run(args: List[String]) = talk(StubHandle("kerfume")).provide(Live).fold(error => 1, _ => 0)
}

object Live extends Console.Live with Clock.Live with Chat.Develop with ClientHandle.Develop
