package edu.knoldus

abstract class Commision
case class ClientSideCommission(value: Int)
case class StreetSideCommission(value: Int)

private sealed trait CommissionDisplay {

  def totalDisplayCommission: String
}


class CalculateCommision {

}
