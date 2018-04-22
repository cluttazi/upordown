package models

import scala.collection.mutable
import scala.util.Try

object ElevatorStatus {

  sealed trait EnumVal

  case object Idle extends EnumVal

  case object Up extends EnumVal

  case object Down extends EnumVal

  val elevatorStatus = Seq(Idle, Up, Down)
}

class Elevator(startingFloor: Int) {

  import ElevatorStatus._

  private var _currentFloor: Int = startingFloor
  private var _status: ElevatorStatus.EnumVal = Idle
  private var _nextFloors: mutable.SortedSet[Int] = new mutable.TreeSet

  def currentFloor: Int = this._currentFloor

  /**
    * Tells if an elevator is going up, down or is idle
    *
    * @return
    */
  def currentStatus: ElevatorStatus.EnumVal = this._status

  /**
    * inserts a floor or gives an error for invalid floors
    *
    * @param floor
    * @return
    */
  def goTo(floor: Int): Try[Unit] = Try {
    if (
      (this._status == Idle) ||
        (this.currentFloor < floor && this._status == Up) ||
        (this.currentFloor > floor && this._status == Down)
    )
      insertFloor(floor)
    else {
      throw new Exception("Cannot insert floor")
    }
  }

  /**
    * moves one unit at a time
    */
  def move: Unit = {
    if (this.nextFloors.nonEmpty) {
      if (this._status == Idle) {
        val destination: Int = this.nextFloors.firstKey
        // set the status
        if (this._currentFloor > destination) {
          this._status = Down
        } else {
          this._status = Up
        }
      }
      //move one unit towards destiny
      this.moveOne
    }
  }

  /**
    * Effort of going to the floor, it is minus 0 in case the floor is unreachable
    *
    * @param floor
    * @return
    */
  def effort(floor: Int): Int = {
    this.currentStatus match {
      case Idle => Math.abs(floor - this.currentFloor)
      case Up => floor - this.currentFloor
      case Down => this.currentFloor - floor
    }
  }

  /**
    * floors to reach
    *
    * @return
    */
  def nextFloors: mutable.SortedSet[Int] = this._nextFloors.clone

  /**
    * removes the current floor from the next floors
    */
  private def openDoors: Unit = this._nextFloors -= this._currentFloor

  /**
    * justs checks if it needs to set the status
    */
  private def closeDoors: Unit = if (this._nextFloors.isEmpty) this._status = Idle

  /**
    * will insert a floor as long as it is not the current one
    *
    * @param floor
    */
  private def insertFloor(floor: Int): Unit = {
    // when inserting a new floor we need to keep into account the following,
    // the floor is already validated in direction but we need to ask if its the current one, if it is then discard it
    if (this.currentFloor == floor) {
      //nothing to do}
    } else {
      //now, the current list is already ordered, so we just insert the floor
      this._nextFloors += floor
    }
  }

  /**
    * will move one floor towards target
    */
  private def moveOne: Unit = {
    this._status match {
      case Idle => {
        //nothing to do
      }
      case Up => {
        this._currentFloor += 1
      }
      case Down => {
        this._currentFloor -= 1
      }
    }
    val destination: Int = {
      if (this.currentStatus == Up) this.nextFloors.firstKey // we want the minimum element if we are going up
      else this.nextFloors.lastKey // we want the maximum if we are going down
    }
    if (destination == this.currentFloor) {
      // we got to destination
      // remove element
      this.openDoors
      this.closeDoors
    }
  }

}

