package module3

import module3.zioConcurrency.printEffectRunningTime
import module3.zio_homework.config.{AppConfig, load}
import zio.clock.Clock
import zio.{IO, UIO, ZIO}
import zio.console.{Console, getStrLn, putStrLn, putStrLnErr}
import zio.duration.durationInt
import zio.random.{Random, nextIntBetween}

import java.io.IOException
import scala.language.postfixOps

package object zio_homework {
    /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет.
   */

   trait Error
   object Error
   {
     case class ParseError(message:String) extends Error
     case object NotGuess extends Error
   }

   lazy val guessProgram: ZIO[Console with Random, Error, Unit] = for {
     _           <- putStrLn("Угадайте число от 1 до 3:")
     userValue   <- getStrLn.orDie.flatMap(str=>ZIO.fromOption(str.toIntOption).orElseFail(Error.ParseError("Не удалось привести к Int")))
     randomValue <- nextIntBetween(1,3)
     _           <- if(userValue==randomValue) putStrLn("Вы угадали!") else putStrLnErr("Вы не угадали") *> ZIO.fail(Error.NotGuess)
   } yield ()

   lazy val retryGuessProgram: ZIO[Console with Random, Nothing, Unit] = guessProgram.foldM(error=>putStrLnErr(s"Произошла ошибка: $error. Повторите ввод") *> retryGuessProgram, _ => ZIO.succeed(()))

  /**
   * 2. реализовать функцию doWhile, которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   */

   def doWhile[R, E, A](body: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] = body.filterOrElse_(condition)(doWhile(body)(condition))

   lazy val testDoWhile: ZIO[Console, IOException, String] = doWhile(putStrLn("Угадайте строку (hint:hello)") *> getStrLn)(_=="hello")

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault: ZIO[Console, Nothing, Unit] = (load <> UIO.succeed(AppConfig("default","default@ss.ru"))).flatMap(c=>putStrLn(c.toString()))

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   *  4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   *  Используйте сервис zio Random
   */
  lazy val eff:ZIO[Random with Clock, Nothing, Int] = nextIntBetween(0,10).delay(1.seconds)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
   lazy val effects: List[ZIO[Random with Clock, Nothing, Int]] = List.fill(10)(eff)

  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

    lazy val app: ZIO[Console with Clock with Random, Nothing, Unit] = printEffectRunningTime(effects.fold(UIO.succeed(0))((acc, el)=>acc.zipWith(el)(_+_)).flatMap(v=>putStrLn(v.toString())))

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

    lazy val appSpeedUp: ZIO[Console with Clock with Random, Nothing, Unit] = printEffectRunningTime(effects.fold(UIO.succeed(0))((acc, el)=>acc.zipWithPar(el)(_+_)).flatMap(v=>putStrLn(v.toString())))


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */
}
