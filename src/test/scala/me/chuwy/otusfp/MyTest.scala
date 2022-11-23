package me.chuwy.otusfp

import cats.effect.implicits._
import cats.effect.testing.specs2.CatsEffect
import cats.effect.{IO, Ref, Resource}
import io.circe.Json
import me.chuwy.otusfp.Restful
import org.http4s.Method.GET
import org.http4s.Request
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.implicits._
import org.specs2.mutable.Specification
import io.circe.generic.auto._
import io.circe.literal.JsonStringContext
import io.circe.syntax._
import org.http4s.circe.CirceEntityCodec._

import scala.concurrent.duration._


class MyTest extends Specification with CatsEffect {

  type Counter[F[_]] = Ref[F, Int]
  case class CounterClass(counter: Int)

  val client: Resource[IO, Client[IO]] = for {
    counter <- Resource.eval[IO, Counter[IO]](Ref.of(0))
    client = Client.fromHttpApp(Restful.httpApp(counter))
  } yield client

  "Counter Service" should {
    "count 2 and 3" in {
      val req: Request[IO] = Request(method = GET, uri = uri"/counter")
      val expR2 = CounterClass(2)

      val resp2 = for {
        client <- client
        _ <- client.expect[CounterClass](req).toResource
        response <- client.expect[CounterClass](req).toResource
      } yield response

      resp2.map(_.equals(expR2))
    }
  }

  case class Data(data: String)

  "Throttling Service" should {
    "slowly return chunks" in {
      val req: Request[IO] = Request(method = GET, uri = uri"/slow/10/1024/1").withEntity(Data("12345678901234567890123456789012345678901234567890").asJson)

      val resp = """"<<Chunk(123, 34, 100, 97, 116, 97, 34, 58, 34, 49)>>""<<Chunk(50, 51, 52, 53, 54, 55, 56, 57, 48, 49)>>""<<Chunk(50, 51, 52, 53, 54, 55, 56, 57, 48, 49)>>""<<Chunk(50, 51, 52, 53, 54, 55, 56, 57, 48, 49)>>""<<Chunk(50, 51, 52, 53, 54, 55, 56, 57, 48, 49)>>""<<Chunk(50, 51, 52, 53, 54, 55, 56, 57, 48, 34)>>""<<Chunk(125)>>""""

      val respTest = for {
        client <- client
        response <- client.expect[String](req).toResource
      } yield response
      respTest.map(_ must beEqualTo(resp))
    }
  }
}