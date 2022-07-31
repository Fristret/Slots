package Game

import scala.util.Random

object RNG {

  def generator: Int = Random.between(1, 101)

  def generatorRPG: Int = Random.between(1, 11)

}
