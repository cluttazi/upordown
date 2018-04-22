package models

import java.util.UUID

import models.ElevatorStatus.Idle

import scala.collection.mutable
import scala.util.Try

class NonAKKAElevator(
                       var _currentFloor: Int = 0,
                       var _currentShaft: String = UUID.randomUUID().toString.toUpperCase,
                       var _status: ElevatorStatus.EnumVal = Idle,
                       var _nextFloors: mutable.SortedSet[Int] = new mutable.TreeSet
                     ) extends Elevator {

  import ElevatorStatus._

  override def currentFloor: Int = this._currentFloor

  override def currentShaft: String = this._currentShaft

  /**
    * Tells if an elevator is going up, down or is idle
    *
    * @return
    */
  override def currentStatus: ElevatorStatus.EnumVal = this._status

  /**
    * inserts a floor or gives an error for invalid floors
    *
    * @param floor
    * @return
    */
  override def goTo(floor: Int): Try[Unit] = Try {
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
    * Effort of going to the floor, it is minus 0 in case the floor is unreachable
    *
    * @param floor
    * @return
    */
  override def effort(floor: Int): Int = {
    this.currentStatus match {
      case Idle => Math.abs(floor - this.currentFloor)
      case Up => floor - this.currentFloor
      case Down => this.currentFloor - floor
    }
  }

  /**
    * moves one unit at a time
    */
  override def move: Unit = {
    if (this.nextFloors.nonEmpty) {
      if (this._status == Idle) {
        val destination: Int = this._nextFloors.firstKey
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
    * floors to reach
    *
    * @return
    */
  override def nextFloors: Seq[Int] = this._nextFloors.clone().toSeq

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
      if (this.currentStatus == Up) this._nextFloors.firstKey // we want the minimum element if we are going up
      else this._nextFloors.lastKey // we want the maximum if we are going down
    }
    if (destination == this.currentFloor) {
      // we got to destination
      // remove element
      this.openDoors
      this.closeDoors
    }
  }

}

