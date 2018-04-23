package models.elevatorcontrolsystem

import models.ElevatorStatus.Idle
import models._
import models.elevator.NonAKKAElevator
import models.utils.Enums

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

class NonAKKAElevatorControlSystem extends ElevatorControlSystem {
  private var _elevators: mutable.ListBuffer[Elevator] = new mutable.ListBuffer[Elevator] //current elevators list
  private var _requests: mutable.ListBuffer[ElevatorRequest] = new mutable.ListBuffer[ElevatorRequest] //current queue of pending requests

  /**
    * Requests a elevator
    *
    * @param request
    */
  override def pickupRequest(request: ElevatorRequest): Unit = this.addRequest(request)

  /**
    * Updates an elevator
    *
    * @param elevator
    */
  override def update(elevator: Elevator): Unit = {
    val optionElevator: Option[Elevator] = this._elevators.find(_.currentShaft == elevator.currentShaft)
    if (optionElevator.isDefined) {
      this._elevators -= optionElevator.get
      this.addElevator(elevator)
    } else {
      this.addElevator(elevator)
    }
  }

  /**
    * Provides a list with the Elevator, the current floor and if it is going up or down
    *
    * @return
    */
  override def queryStatus: Seq[ElevatorFloor] =
    this._elevators
      .map(e => ElevatorFloor(e, e.currentFloor, e.currentStatus))

  /**
    * Time Stepping simulation
    */
  override def simulation: Unit = {
    var now: Long = -1
    val r = scala.util.Random
    val numberOfElevators: Int = r.nextInt(Enums.maxElevators)
    val numberOfRequests: Int = r.nextInt(Enums.maxNumberOfRequests)

    //create elevators
    for (i <- 1 to numberOfElevators) {
      this.addElevator(
        new NonAKKAElevator(initFloor = Enums.maxNumberOfFloors)
      )
    }

    //create requests
    val buildRequests: mutable.ListBuffer[ElevatorRequest] = new mutable.ListBuffer[ElevatorRequest]
    for (i <- 1 to numberOfRequests) {
      buildRequests += {
        val currentFloor: Int = r.nextInt(Enums.maxNumberOfFloors)
        val possibleDestination: Int = currentFloor + Math.abs(r.nextInt(Enums.maxNumberOfFloors))
        val up: Boolean = r.nextBoolean()
        var destination: Int = 0
        if (up) destination = currentFloor + possibleDestination // will always give higher
        else destination = currentFloor - possibleDestination // will always give lower
        ElevatorRequest(
          Math.abs(r.nextInt(Enums.maxNumberOfTime)),
          currentFloor,
          up,
          destination
        )
      }
    }

    while (
      now < 101 || //we can have requests from 0 to 100
      this._requests.nonEmpty // we have pending requests
        || this._elevators // or the elevators are still moving
        .map(_.nextFloors.nonEmpty)
        .reduce(_ || _)
    ) {
      now += 1
      this._requests.++=:(buildRequests.filter(_.time == now)) // only adds requests on determined time
      this.moveAll
      System.out.println(s"Current Time is t + $now")
      this.printStatus
    }

  }

  /**
    * advances 1 unit of time
    */
  def moveAll: Unit = {
    this._elevators.foreach(_.move)
    var unsatisfiedRequest: mutable.ListBuffer[ElevatorRequest] = new mutable.ListBuffer[ElevatorRequest]
    // iterates the current list of requests
    this._requests.foreach(
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
    this._requests = unsatisfiedRequest
    System.out.println(s"This round unsatisfied requests : ${unsatisfiedRequest.length}")
  }

  /**
    * Adds a request
    *
    * @param req
    * @return
    */
  override def addRequest(req: ElevatorRequest): Try[Unit] = Try {
    this._requests += req
  }

  /**
    * Adds an elevator
    *
    * @param elevator
    * @return
    */
  override def addElevator(elevator: Elevator): Try[Unit] = Try {
    this._elevators += elevator
  }

  /**
    * Used to print the status of the Elevators and the queues
    */
  def printStatus: Unit = {
    for (e <- this._elevators) {
      if (e.currentStatus != Idle) {
        System.out.println(s"Found E on floor: ${e.currentFloor} going ${e.currentStatus}")
        System.out.println(s"Next floors are: ")
        for (n <- e.nextFloors) {
          System.out.print(s" $n ")
        }
        System.out.println("")
      }
    }
  }

  def elevators = this._elevators.clone()

  def requests = this._requests.clone()

  /**
    * Provides a list of available elevators that can serve the requests
    *
    * @param floor : floor to go
    * @return
    */
  private def availableElevators(floor: Int): ListBuffer[ElevatorOption] =
    this._elevators
      .map(e => ElevatorOption(e, e.effort(floor), e.nextFloors.size))
      .filter(e => e.effort >= 0)

}

object NonAKKAElevatorControlSystem extends NonAKKAElevatorControlSystem