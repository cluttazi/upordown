package models

object ElevatorStatus {

  sealed trait EnumVal

  case object Idle extends EnumVal

  case object Up extends EnumVal

  case object Down extends EnumVal

  val elevatorStatus = Seq(Idle, Up, Down)
}

// this trait could actually go, but since I started with it when I refactored to AKKA I prefered to let it be
// probably is going to be replaced in the next iteration if its not needed
trait Elevator {
  def currentFloor: Int

  def currentShaft: String

  def currentStatus: ElevatorStatus.EnumVal

  def goTo(floor: Int): Unit

  def effort(floor: Int): Int

  def move: Unit

  def nextFloors: Seq[Int]
}

