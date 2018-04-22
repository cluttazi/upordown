package models

import scala.util.Try

object ElevatorStatus {

  sealed trait EnumVal

  case object Idle extends EnumVal

  case object Up extends EnumVal

  case object Down extends EnumVal

  val elevatorStatus = Seq(Idle, Up, Down)
}

trait Elevator {
  def currentFloor: Int

  def currentShaft: String

  def currentStatus: ElevatorStatus.EnumVal

  def goTo(floor: Int): Try[Unit]

  def effort(floor: Int): Int

  def move: Unit

  def nextFloors: Seq[Int]
}

