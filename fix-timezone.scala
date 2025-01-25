//> using scala "3.6.3"
//> using plugin "org.polyvariant:::better-tostring:0.3.17"
//> using dep "com.monovore::decline-effect::2.4.1"
//> using dep "org.typelevel::cats-effect::3.3.14"
//> using dep "co.fs2::fs2-io::3.3.0"
//> using dep "com.lihaoyi::os-lib:0.8.1"
//> using option -Wunused:all
import cats.effect.ExitCode
import cats.effect.IO
import cats.implicits.*
import com.monovore.decline.Argument
import com.monovore.decline.CommandApp
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import fs2.io.file.Files
import fs2.io.file.Path
import sys.process.*

import java.time.LocalDateTime
import java.time.ZoneOffset

given Argument[Path] = Argument[java.nio.file.Path].map(Path.fromNioPath(_))

given arp: Argument[os.FilePath] = Argument[java.nio.file.Path].map(os.FilePath(_))

object FixTimezone extends CommandIOApp("fix-timezone", "Fix timezone"):

  def main: Opts[IO[ExitCode]] = Opts
    .argument[Path]("directory")
    .map(fixPath(_).as(ExitCode.Success))

  def hasGPS(path: Path): IO[Boolean] = IO.interruptibleMany {
    s"exiftool '$path'".!!.contains("GPS")
  }

  def shiftTimezone(path: Path): IO[Unit] =
    IO.interruptibleMany {
      s"jhead -ta-1 '$path'".!!
    }.void

  def fixPath(path: Path): IO[Unit] = {
    Files[IO]
      .list(path)
      .as(1)
      .foldMonoid
      .evalMap(c => IO.println(s"File count: $c"))
      .drain ++
      Files[IO]
        .list(path)
        // .evalFilterNotAsync(maxConcurrent = 10)(hasGPS)
        .evalMap(p => shiftTimezone(p).as(p))
        .debug("Fixed " + _)
  }.compile.drain

end FixTimezone

def parseDate(s: String): LocalDateTime =
  // sorry mom
  s match {
    case s"Photo on $day-$month-$year at $hour.$minute AM" =>
      LocalDateTime.of(year.toInt, month.toInt, day.toInt, hour.toInt, minute.toInt)

    case s"Photo on $day-$month-$year at $hour.$minute PM" =>
      LocalDateTime.of(year.toInt, month.toInt, day.toInt, hour.toInt + 12, minute.toInt)

    case s"Photo on $day-$month-$year at $hour.$minute AM #$_" =>
      LocalDateTime.of(year.toInt, month.toInt, day.toInt, hour.toInt, minute.toInt)

    case s"Photo on $day-$month-$year at $hour.$minute PM #$_" =>
      LocalDateTime.of(year.toInt, month.toInt, day.toInt, hour.toInt + 12, minute.toInt)

    case s"Photo on $day-$month-$year at $hour.$minute #$_" =>
      LocalDateTime.of(year.toInt, month.toInt, day.toInt, hour.toInt, minute.toInt)

    case s"Photo on $day-$month-$year at $hour.$minute" =>
      LocalDateTime.of(year.toInt, month.toInt, day.toInt, hour.toInt, minute.toInt)
  }

object SetTimezoneBooth
  extends CommandApp(
    "set-timezone-booth",
    "Set timezone on a Photo Booth photo based on the name",
    Opts.argument[os.FilePath]("directory").map { base =>
      os.list(base.resolveFrom(os.pwd)).foreach { file =>
        val filename = file.last
        if filename.startsWith(".") then ()
        else {
          val theDate = parseDate(file.baseName)
          // set modified time
          os.mtime.set(file, theDate.toInstant(ZoneOffset.ofHours(1)).toEpochMilli())
          // set exif dates to file dates
          os.proc("jhead", "-dsft", file).call().exitCode
        }
      }
    },
  )
