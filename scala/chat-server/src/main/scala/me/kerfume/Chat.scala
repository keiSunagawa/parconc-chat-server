package me.kerfume

import scalaz.zio._
import scalaz.zio.stm._
import scalaz.zio.console._

trait Chat[Handle] {
  import ClientHandle.Helper._
  import Chat._

  def talk(handle: Handle) = for {
    _ <- printMessage(handle, "Hello!!")
    ms <- STM.atomically {
      for {
        client <- Client.newClient("kerfume", handle)
        _ <- client.sendMessage(Message.Command("hello."))
        s <- client.sendChan.take
      } yield s.asInstanceOf[Message.Command].command
    }
    _ <- putStrLn(ms)
  } yield ()
}

object Chat {
  // state
  type ClientName = String
  type Reason = String
  sealed trait Message
  object Message {
    case class Notice(msg: String) extends Message
    case class Tell(to: ClientName, msg: String) extends Message
    case class Broadcast(msg: String) extends Message
    case class Command(command: String) extends Message
  }
  case class Client[Handle] private (
    name: ClientName,
    handle: Handle,
    sendChan: TQueue[Message],
    kicked: TQueue[Reason] // single capacity queue.
  ) {
    def sendMessage(msg: Message): STM[Nothing, Unit] = sendChan.offer(msg)
  }

  object Client {
    def newClient[Handle](name: ClientName, handle: Handle): STM[Nothing, Client[Handle]] = {
      for {
        chan <- TQueue.make[Message](100) // can't make unbounded queue.
        kicked <- TQueue.make[Reason](1) // like Haskell's 'TVar (Maybe String)'.
      } yield Client(
        name = name,
        handle = handle,
        sendChan = chan,
        kicked = kicked
      )
    }
  }
  case class Server[Handle](clients: Map[ClientName, Client[Handle]])

  // service and implements
  trait Service[Handle] {
    def chat: Chat[Handle]
  }
  import ClientHandle.StubHandle
  trait Develop extends Service[StubHandle] {
    lazy val chat = new Chat[StubHandle] {}
  }

  object Helper {
    def talk[Handle](handle: Handle): ZIO[Service[Handle] with ClientHandle.Service[Handle] with Console, Throwable, Unit] = ZIO.accessM(_.chat.talk(handle))
  }
}

trait ClientHandle[Handle] {
  def printMessage(handle: Handle, msg: String): Task[Unit]
}

object ClientHandle {
  case class SocketHandle()
  case class StubHandle(name: String) {
    import scala.Console
    // like scala.zio.console.Console.Live#puStrLn
    def println(msg: String): UIO[Unit] = IO.effectTotal(Console.withOut(Console.out)(Console.println(msg)))
    def getLine(): UIO[String] = IO.effectTotal(s"hello. may name is $name.")
  }
  trait Service[Handle] {
    def clientHandle: ClientHandle[Handle]
  }
  trait Live extends Service[SocketHandle] {
    lazy val clientHandle = ???
  }
  trait Develop extends Service[StubHandle] {
    lazy val clientHandle = new ClientHandle[StubHandle] {
      def printMessage(handle: StubHandle, msg: String): Task[Unit] = handle.println(msg)
    }
  }

  object Helper {
    def printMessage[Handle](handle: Handle, msg: String): ZIO[Service[Handle], Throwable, Unit] = ZIO.accessM(_.clientHandle.printMessage(handle, msg))
  }
}
