package game.utils

import game.models.SlotObjects.{Column, Symbol, NoSymbol}

object SaveMethods {

  trait SaveTail[A] {
    def getTail(list: List[A]): List[A]
  }

  def getHead(int: Int, list: List[Column]): Symbol = list.headOption match {
    case None => NoSymbol
    case Some(x) => x.elements(int)
  }

  implicit val intTail: SaveTail[Int] = (list: List[Int]) => Option(list.drop(1)) match {
    case None => List()
    case Some(x) => x
  }

  implicit val columnTail: SaveTail[Column] = (list: List[Column]) => Option(list.drop(1)) match {
    case None => List()
    case Some(x) => x
  }

  implicit val elementTail: SaveTail[Symbol] = (list: List[Symbol]) => Option(list.drop(1)) match {
    case None => List()
    case Some(x) => x
  }

  implicit val listTail: SaveTail[List[Symbol]] = (list: List[List[Symbol]]) => Option(list.drop(1)) match {
    case None => List(List())
    case Some(x) => x
  }

  implicit class SaveTailOps[A](value: List[A]) {
    def tailSave(implicit ev: SaveTail[A]): List[A] = ev.getTail(value)
  }
}
