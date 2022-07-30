package Game

import Game.SlotObjects.{Column, Element}

object CheckScreen {

  // 1 -> 1 -> 1 -> 1 -> 1
  def checkRow(int: Int, count: Int = 1, list: List[Column]): List[Element] = count match {
    case 5 => List(list.head.elements(int))
    case _ => List(list.head.elements(int)) ++ checkRow(int, count + 1, list.tail)
  }

  //1 -> 2 -> 1 -> 2 -> 1 || 1 -> 2 -> 1 -> 2 -> 3
  def check2(end: Int, count: Int = 1, list: List[Column]): List[Element] = count match {
    case 5 => List(list.head.elements(end))
    case x if x == 1 || x == 3 => List(list.head.elements(1)) ++ check2(end, count + 1, list.tail)
    case x if x == 2 || x == 4 => List(list.head.elements(2)) ++ check2(end, count + 1, list.tail)
  }

  // 1 -> 3 -> 1 -> 3 -> 1
  def check3(int: Int, count: Int = 1, list: List[Column]): List[Element] = count match {
    case 5 => List(list.head.elements(int))
    case _ => int match {
      case 1 => List(list.head.elements(1)) ++ check3(3, count + 1, list.tail)
      case 3 => List(list.head.elements(3)) ++ check3(1, count + 1, list.tail)
    }
  }

  //1 -> 3 -> 3 -> 3 -> 1 || 3 -> 2 -> 2 -> 2 -> 3
  def check4(int: Int, count: Int = 1, list: List[Column]): List[Element] = count match {
    case 5 => List(list.head.elements(int))
    case 1 => List(list.head.elements(int)) ++ (int match {
      case 1 => check4(int + 2, count + 1, list.tail)
      case 3 => check4(int - 1, count + 1, list.tail)
    })
    case 4 => List(list.head.elements(int)) ++ (int match {
      case 3 => check4(int - 2, count + 1, list.tail)
      case 2 => check4(int + 1, count + 1, list.tail)
    })
    case _ => List(list.head.elements(int)) ++ check4(int, count+1, list.tail)
  }

  // 3 -> 2 -> 1 -> 2 -> 3
  def check5(count: Int = 1, list: List[Column]): List[Element] = count match {
    case 1 => List(list.head.elements(3)) ++ check5(count + 1, list.tail)
    case 5 => List(list.head.elements(3))
    case 3 => List(list.head.elements(1)) ++ check5(count + 1, list.tail)
    case x if x == 2 || x == 4 => List(list.head.elements(2)) ++ check5(count + 1, list.tail)
  }

  //1 -> 2 -> 2 -> 2 -> 3
  def check6(int: Int, count: Int = 1, list: List[Column]): List[Element] = count match {
    case 1 => int match {
      case 1 => List(list.head.elements(int)) ++ check6(3, count + 1, list.tail)
      case 3 => List(list.head.elements(int)) ++ check6(1, count + 1, list.tail)
    }
    case 5 => List(list.head.elements(int))
    case _ => List(list.head.elements(2)) ++ check6(int, count + 1, list.tail)
  }

  // 3 -> 3 -> 1 -> 3 -> 3 || 2 -> 3 -> 1 -> 3 -> 2
  def check7(int: Int, count: Int = 1, list: List[Column]): List[Element] = count match {
    case 5 => List(list.head.elements(int))
    case 1 => List(list.head.elements(int)) ++ check7(int, count + 1, list.tail)
    case 3 => List(list.head.elements(1)) ++ check7(int, count + 1, list.tail)
    case x if x == 2 || x == 4 => List(list.head.elements(3)) ++ check7(int, count + 1, list.tail)
  }

  //1 -> 3 -> 2 -> 3 -> 2
  def check8(count: Int = 1, list: List[Column]): List[Element] = count match {
    case 5 => List(list.head.elements(2))
    case x if x == 2 || x == 4 => List(list.head.elements(3)) ++ check8(count + 1, list.tail)
    case 1 => List(list.head.elements(1)) ++ check8(count + 1, list.tail)
    case 3 => List(list.head.elements(2)) ++ check8(count + 1, list.tail)
  }
}
