package models

import scala.util.Try

//1. Querying the state of the elevators (what floor are they on and where they are going)
//2. receiving an update about the status of an elevator
//3. receiving a pickup request
//4. time-stepping the simulation.

//models a request used by the elevator system
case class ElevatorRequest(time: Long, floor: Int, up: Boolean, destination: Int)

//possible elevator that can be used to fulfill the request
case class ElevatorOption(elevator: Elevator, effort: Int, queue: Int)

//provides the status of an elevator
case class ElevatorFloor(elevator: Elevator, floor: Int, direction: ElevatorStatus.EnumVal)

trait ElevatorControlSystem {
  //1. Querying the state of the elevators (what floor are they on and where they are going)
  def queryStatus: Seq[ElevatorFloor]

  //2. receiving an update about the status of an elevator
  def update(elevator: Elevator): Unit

  //3. receiving a pickup request
  def pickupRequest(request: ElevatorRequest): Unit

  //4. time-stepping the simulation.
  def simulation: Unit

  def addRequest(req: ElevatorRequest): Try[Unit]

  def addElevator(elevator: Elevator): Try[Unit]
}