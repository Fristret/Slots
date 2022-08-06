package game.utils

import game.models.SlotObjects.{Column, Element}
import game.utils.SaveMethods._

object CheckScreen {

  // 1 -> 1 -> 1 -> 1 -> 1
  def checkRow(int: Int, count: Int = 1, list: List[Column]): List[Element] = count match {
    case 5 => List(getHead(int, list))
    case _ => List(getHead(int, list)) ++ checkRow(int, count + 1, list.tailSave)
  }

  //1 -> 2 -> 1 -> 2 -> 1 || 1 -> 2 -> 1 -> 2 -> 3
  def check2(end: Int, count: Int = 1, list: List[Column]): List[Element] = count match {
    case 5 => List(getHead(end, list))
    case x if x == 1 || x == 3 => List(getHead(1, list)) ++ check2(end, count + 1, list.tailSave)
    case x if x == 2 || x == 4 => List(getHead(2, list)) ++ check2(end, count + 1, list.tailSave)
  }

  // 1 -> 3 -> 1 -> 3 -> 1
  def check3(int: Int, count: Int = 1, list: List[Column]): List[Element] = count match {
    case 5 => List(getHead(int, list))
    case _ => int match {
      case 1 => List(getHead(1, list)) ++ check3(3, count + 1, list.tailSave)
      case 3 => List(getHead(3, list)) ++ check3(1, count + 1, list.tailSave)
    }
  }

  //1 -> 3 -> 3 -> 3 -> 1 || 3 -> 2 -> 2 -> 2 -> 3
  def check4(int: Int, count: Int = 1, list: List[Column]): List[Element] = count match {
    case 5 => List(getHead(int, list))
    case 1 => List(getHead(int, list)) ++ (int match {
      case 1 => check4(int + 2, count + 1, list.tailSave)
      case 3 => check4(int - 1, count + 1, list.tailSave)
    })
    case 4 => List(getHead(int, list)) ++ (int match {
      case 3 => check4(int - 2, count + 1, list.tailSave)
      case 2 => check4(int + 1, count + 1, list.tailSave)
    })
    case _ => List(getHead(int, list)) ++ check4(int, count + 1, list.tailSave)
  }

  // 3 -> 2 -> 1 -> 2 -> 3
  def check5(count: Int = 1, list: List[Column]): List[Element] = count match {
    case 1 => List(getHead(3, list)) ++ check5(count + 1, list.tailSave)
    case 5 => List(getHead(3, list))
    case 3 => List(getHead(1, list)) ++ check5(count + 1, list.tailSave)
    case x if x == 2 || x == 4 => List(getHead(2, list)) ++ check5(count + 1, list.tailSave)
  }

  //1 -> 2 -> 2 -> 2 -> 3
  def check6(int: Int, count: Int = 1, list: List[Column]): List[Element] = count match {
    case 1 => int match {
      case 1 => List(getHead(int, list)) ++ check6(3, count + 1, list.tailSave)
      case 3 => List(getHead(int, list)) ++ check6(1, count + 1, list.tailSave)
    }
    case 5 => List(getHead(int, list))
    case _ => List(getHead(2, list)) ++ check6(int, count + 1, list.tailSave)
  }

  // 3 -> 3 -> 1 -> 3 -> 3 || 2 -> 3 -> 1 -> 3 -> 2
  def check7(int: Int, count: Int = 1, list: List[Column]): List[Element] = count match {
    case 5 => List(getHead(int, list))
    case 1 => List(getHead(int, list)) ++ check7(int, count + 1, list.tailSave)
    case 3 => List(getHead(1, list)) ++ check7(int, count + 1, list.tailSave)
    case x if x == 2 || x == 4 => List(getHead(3, list)) ++ check7(int, count + 1, list.tailSave)
  }

  //1 -> 3 -> 2 -> 3 -> 2
  def check8(count: Int = 1, list: List[Column]): List[Element] = count match {
    case 5 => List(getHead(2, list))
    case x if x == 2 || x == 4 => List(getHead(3, list)) ++ check8(count + 1, list.tailSave)
    case 1 => List(getHead(1, list)) ++ check8(count + 1, list.tailSave)
    case 3 => List(getHead(2, list)) ++ check8(count + 1, list.tailSave)
  }
}
