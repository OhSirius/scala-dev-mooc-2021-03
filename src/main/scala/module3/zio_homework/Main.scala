package module3.zio_homework

import zio.{ExitCode, URIO, ZEnv}

object Main extends zio.App{

  def run(args: List[String]): URIO[ZEnv, ExitCode] = testServiceApp.exitCode
}