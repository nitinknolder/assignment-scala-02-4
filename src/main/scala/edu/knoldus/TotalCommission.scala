
package edu.knoldus

import org.apache.log4j.Logger

import scala.reflect.runtime.universe._

abstract class Commission {
  val value: Int
}

case class ClientSideCommission(override val value: Int) extends Commission

case class StreetSideCommission(override val value: Int) extends Commission


object TypesOfCommission extends App {

    val log = Logger.getLogger (getClass)

  private sealed trait CommissionDisplay {

    def totalDisplayCommission: String

  }

  class TotalCommission[T <: Commission : TypeTag] (simpleCommission: List[T]) {

      def getTotalCommission: String = {
        simpleCommission.totalDisplayCommission
      }

    private implicit class TypeChecking[T <: Commission : TypeTag] (simpleCommission: List[T]) extends CommissionDisplay {

      val total = simpleCommission.map(_.value).sum
    def totalDisplayCommission: String = {

      typeOf[List[T]] match {
          case client if client =:= typeOf[List[ClientSideCommission]] => s"Client commission = ${total}"
          case street if street =:= typeOf[List[StreetSideCommission]]=> s" Street Commission = ${total}"
          case commission if commission =:= typeOf[List[Commission]]=> s"Mingled Commission = ${total}"
          case _  => "empty"
        }
      }
    }
    }

    val value1 = 5
    val value2 = 10
    val clientObj = ClientSideCommission (value1)
    val clientObj1 = ClientSideCommission (value2)
    val clientObj2 = ClientSideCommission (value2)
    val totalOfList = List (clientObj, clientObj1,clientObj2)
    val totalClientCommission = new TotalCommission[ClientSideCommission](totalOfList)
    log.debug (s"${totalClientCommission.getTotalCommission}\n")

  val streetObj = StreetSideCommission (value1)
  val streetObj1 = StreetSideCommission (value2)
  val streetObj2 = StreetSideCommission (value2)
  val totalOfList1 = List (streetObj, streetObj1,streetObj2)
  val totalClientCommission1 = new TotalCommission[StreetSideCommission](totalOfList1)
  log.debug (s"${totalClientCommission1.getTotalCommission}\n")

}

