package collections

import scala.util.Random

/*
В урне 3 белых и 3 черных шара. Из урны дважды вынимают по одному шару, не возвращая их обратно. Найти вероятность появления белого шара
 */
object ProbabilityCheck extends App{

  val maxValue = 10_000_000

  val experiments = (1 to maxValue)
    .map(_ => {
      def isWhite = Random.nextInt(2) == 1
      isWhite || isWhite
    })

  //для isWhite && isWhite (когда оба шара белые)
  //maxValue 10_000 ->        P = 0.2497
  //maxValue 10_000_000 ->    P = 0.250109
  //maxValue 100_000_000 ->   P = 0.24991192
  //maxValue 1_000_000_000 -> P = 0.249983371


  //для условия одного белого шара вероятность 0.75
  val probability = experiments.count(x => x) / experiments.length.toDouble
  println(probability)






}
