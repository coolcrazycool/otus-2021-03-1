package me.chuwy.otusfp

import scala.concurrent.ExecutionContext.global
import cats.data.{Kleisli, OptionT, ReaderT}
import cats.effect.IO.{IOCont, Uncancelable}
import cats.effect._
import cats.implicits.toFoldableOps
import com.comcast.ip4s.IpLiteralSyntax
import com.comcast.ip4s.Literals.ipv4
import io.circe.{Decoder, DecodingFailure, Encoder, JsonObject}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.literal.JsonStringContext
import org.http4s.{AuthedRoutes, Header, HttpApp, HttpRoutes, Request, Status}
import org.http4s.circe.{CirceEntityDecoder, CirceEntityEncoder, jsonEncoderOf, jsonOf}
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.Forwarded.Node.Port
import org.http4s.implicits._
import org.http4s.server.defaults.{Host, IPv4Host}
import org.http4s.server.{AuthMiddleware, Router, Server}
import org.http4s.server.websocket.WebSocketBuilder
import org.typelevel.ci.CIString
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe.CirceEntityCodec._

import java.nio.charset.StandardCharsets
import scala.concurrent.duration._
import scala.util.Try


object Restful {

  type Counter[F[_]] = Ref[F, Int]

  case class CounterClass(counter: Int)

  def counterService(counter: Counter[IO]): HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "counter" => for {
        c <- counter.updateAndGet(_ + 1)
        resp <- Ok(CounterClass(c).asJson)
      } yield resp

      case r @ GET -> Root / "slow" / chunk / total / time => (
        Try(chunk.toInt).toOption,
        Try(total.toInt).toOption,
        Try(time.toInt).toOption
        ) match {
        case (Some(ch), Some(tot), Some(tm)) if ch >=0 && tot >= 0 && tm >= 0 => Ok(
          r.body
            .take(tot)
            .chunkN(ch)
            .metered[IO](tm.seconds)
            .evalMapChunk(chunk => IO.pure(s"<<$chunk>>"))
        )

        case _ => BadRequest("Wrong types")
      }
    }

  def httpApp(counter: Counter[IO]): HttpApp[IO] = Router(
    "/" -> counterService(counter)
  ).orNotFound

  val builder = for {
    counter <- Resource.eval(Ref.of[IO, Int](0))
    s <- EmberServerBuilder
      .default[IO]
      .withHost(ipv4"127.0.0.1")
      .withPort(port"8080")
      .withHttpApp(httpApp(counter))
      .build
  } yield s
}
