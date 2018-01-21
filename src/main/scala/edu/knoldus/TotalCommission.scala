
package edu.knoldus

import org.apache.log4j.Logger

import scala.reflect.runtime.universe._

abstract class Commission {
  val value: Int
}

case class ClientSideCommission (value: Int) extends Commission {}

case class StreetSideCommission (value: Int) extends Commission {}


object TypesOfCommission {

  def main (args: Array[String]) {

    val log = Logger.getLogger (getClass)

    sealed trait CommissionDisplay {

      def totalDisplayCommission: String
    }

    class TotalCommission[T <: Commission : TypeTag] (simpleCommission: List[T]) {

      def getTotalCommission: String = {
        simpleCommission.totalDisplayCommission
      }
    }

    def sum (xs: List[Int]): Int = {
      xs match {
        case x :: tail => x + sum (tail) // if there is an element, add it to the sum of the tail
        case Nil => 0 // if there are no elements, then the sum is 0
      }
    }

    implicit class typeChecking[T <: Commission : TypeTag] (simpleCommission: List[T]) extends CommissionDisplay {

      override def totalDisplayCommission: String = {

        typeOf [T] match {
          case client: List[ClientSideCommission] if client =:= typeOf [ClientSideCommission] => s"Client commission = ${simpleCommission.map (_.value).sum}"
          case street: List[streetSideCommission] if street =:= typeOf [StreetSideCommission] => s" Street Commission = ${simpleCommission.map (_.value).sum}"
          case commission: List[Commission] if commission =:= typeOf [Commission] => s"Mingled Commission = ${simpleCommission.map (_.value).sum}"
        }
      }
    }

    val value1 = 5
    val value2 = 10
    val clientObj = ClientSideCommission (value1)
    val clientObj1 = ClientSideCommission (value2)
    val clientObj2 = ClientSideCommission (value2)
    val totalOfList = List (clientObj, clientObj1)
    val totalClientCommission = new TotalCommission[ClientSideCommission](totalOfList)
    log.debug (s"${totalClientCommission.getTotalCommission}\n")
  }
}
