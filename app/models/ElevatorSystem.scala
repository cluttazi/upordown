package models

import models.utils.Enums

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

//models a request used by the elevator system
case class ElevatorRequest(time: Long, floor: Int, up: Boolean, destination: Int)
//possible elevator that can be used to fulfill the request
case class ElevatorOption(elevator: Elevator, effort: Int, queue: Int)
//provides the status of an elevator
case class ElevatorFloor(elevator: Elevator, floor: Int, direction: ElevatorStatus.EnumVal)

//1. Querying the state of the elevators (what floor are they on and where they are going)
//2. receiving an update about the status of an elevator
//3. receiving a pickup request
//4. time-stepping the simulation.

trait ElevatorControlSystem {
  //1. Querying the state of the elevators (what floor are they on and where they are going)
  def queryStatus: Seq[ElevatorFloor]

  //3. receiving a pickup request
  def pickupRequest(request: ElevatorRequest): Unit

  //4. time-stepping the simulation.
  def simulation: Unit
}

class ElevatorSystem extends ElevatorControlSystem {
  var elevators: mutable.ListBuffer[Elevator] = new mutable.ListBuffer[Elevator] //current elevators list
  var requests: mutable.ListBuffer[ElevatorRequest] = new mutable.ListBuffer[ElevatorRequest] //current queue of pending requests

  /**
    * Requests a elevator
    *
    * @param request
    */
  override def pickupRequest(request: ElevatorRequest): Unit = this.addRequest(request)

  /**
    * Provides a list with the Elevator, the current floor and if it is going up or down
    *
    * @return
    */
  override def queryStatus: Seq[ElevatorFloor] =
    this.elevators
      .map(e => ElevatorFloor(e, e.currentFloor, e.currentStatus))

  override def simulation: Unit = {
    var now: Long = 0
    val r = scala.util.Random
    val numberOfElevators: Int = r.nextInt(Enums.maxElevators)
    val numberOfRequests: Int = r.nextInt(Enums.maxNumberOfRequests)

    //create elevators
    for (i <- 1 to numberOfElevators) {
      this.addElevator(new Elevator(Enums.maxNumberOfFloors))
    }

    //create requests
    for (i <- 1 to numberOfRequests) {
      this.addRequest(
        ElevatorRequest(
          r.nextInt(Enums.maxNumberOfTime),
          r.nextInt(Enums.maxNumberOfFloors),
          r.nextBoolean(),
          r.nextInt(Enums.maxNumberOfFloors)
        ))
    }

    while (
      this.requests.nonEmpty //we have pending requests
        || this.elevators //or the elevators are still moving
        .map(_.nextFloors.nonEmpty)
        .reduce(_ || _)
    ) {
      now += 1
      this.moveAll
      System.out.println(s"Current Time is t + $now")
      this.printStatus
    }

  }

  /**
    * advances 1 unit of time
    */
  def moveAll: Unit = {
    this.elevators.foreach(_.move)
    var unsatisfiedRequest: mutable.ListBuffer[ElevatorRequest] = new mutable.ListBuffer[ElevatorRequest]
    // iterates the current list of requests
    this.requests.foreach(
      r => {
        // gets a list of possible elevators that can fulfill the request
        val options: mutable.ListBuffer[ElevatorOption] =
          this.availableElevators(r.floor)
        // if the options is not empty then it goes forward, if its empty then adds to the next round
        if (options.nonEmpty) {
          // gets the elevator with the shortest queue
          // there are many ways to approach this problem, I choose the one with the shortest queue
          // which could end in an elevator having too much travel
          val elevator: Elevator =
          options.reduce(
            (a, b) =>
              if (a.queue < b.queue) a
              else b
          ).elevator
          elevator.goTo(r.floor)
          elevator.goTo(r.destination)
        } else {
          unsatisfiedRequest += r
        }
      }
    )
    this.requests = unsatisfiedRequest
    System.out.println(s"This round unsatisfied requests : ${unsatisfiedRequest.length}")
  }

  /**
    * Used to print the status of the Elevators and the queues
    */
  def printStatus: Unit = {
    for (e <- this.elevators) {
      System.out.println(s"Found E on floor: ${
        e.currentFloor
      } going ${
        e.currentStatus
      }")
      System.out.println(s"Next floors are: ")
      for (n <- e.nextFloors) {
        System.out.print(s" $n ")
      }
      System.out.println("")
    }
  }

  /**
    * Adds a request
    *
    * @param req
    * @return
    */
  def addRequest(req: ElevatorRequest): Try[Unit] = Try {
    this.requests += req
  }

  /**
    * Adds an elevator
    *
    * @param elevator
    * @return
    */
  def addElevator(elevator: Elevator): Try[Unit] = Try {
    this.elevators += elevator
  }


  /**
    * Provides a list of available elevators that can serve the requests
    *
    * @param floor : floor to go
    * @return
    */
  private def availableElevators(floor: Int): ListBuffer[ElevatorOption] =
    this.elevators
      .map(e => ElevatorOption(e, e.effort(floor), e.nextFloors.size))
      .filter(e => e.effort >= 0)

}

object ElevatorSystem extends ElevatorSystem