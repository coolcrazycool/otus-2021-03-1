package me.chuwy.otusfp

import cats.effect.{IOApp, IO}

object Main extends IOApp.Simple {
  def run: IO[Unit] = {
    for {
      _ <- Restful.builder.use(_ => IO.never)
    } yield ()
  }
}
