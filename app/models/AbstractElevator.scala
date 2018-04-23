package models

import java.util.UUID

import models.ElevatorStatus.Idle

import scala.collection.mutable

abstract class AbstractElevator(var _currentFloor: Int = 0,
                                var _currentShaft: String = UUID.randomUUID().toString.toUpperCase,
                                var _status: ElevatorStatus.EnumVal = Idle,
                                var _nextFloors: mutable.SortedSet[Int] = new mutable.TreeSet
                               ) extends Elevator {

  import ElevatorStatus._

  //does not modify status, it can be abstract
  final override def currentFloor: Int = this._currentFloor

  //does not modify status, it can be abstract
  final override def currentShaft: String = this._currentShaft

  //does not modify status, it can be abstract
  /**
    * Tells if an elevator is going up, down or is idle
    *
    * @return
    */
  final override def currentStatus: ElevatorStatus.EnumVal = this._status

  //modifies status, can be abstract if the InsertFloor is encapsulated
  /**
    * inserts a floor or gives an error for invalid floors
    *
    * @param floor
    * @return
    */
  final override def goTo(floor: Int): Unit = {
    this._status match {
      case Up => {
        if (this._currentFloor < floor) insertFloor(floor)
      }
      case Down => {
        if (this._currentFloor > floor) insertFloor(floor)
      }
      case Idle => {
        insertFloor(floor)
        if (this._currentFloor < floor) setStatus(Up)
        else setStatus(Down)
      }
    }
  }

  //does not modify status, it can be abstract
  /**
    * Effort of going to the floor, it is minus 0 in case the floor is unreachable
    *
    * @param floor
    * @return
    */
  final override def effort(floor: Int): Int = {
    this._status match {
      case Idle => Math.abs(floor - this._currentFloor)
      case Up => floor - this._currentFloor
      case Down => this._currentFloor - floor
    }
  }

  // modifies status
  /**
    * moves one unit at a time
    */
  final override def move: Unit = {
    if (this._nextFloors.nonEmpty) {
      if (this._status == Idle) {
        // set the status
        if (this._currentFloor > this.destination) this.setStatus(Down)
        else this.setStatus(Up)
      }
      //move one unit towards destiny
      this.moveOne
    }
  }

  protected def setStatus(status: ElevatorStatus.EnumVal)

  //no status modification
  /**
    * floors to reach
    *
    * @return
    */
  final override def nextFloors: Seq[Int] = this._nextFloors.clone().toSeq

  // modifies status
  /**
    * removes the current floor from the next floors
    */
  protected def openDoors: Unit

  // no status modification
  /**
    * justs checks if it needs to set the status
    */
  private def closeDoors: Unit = if (this._nextFloors.isEmpty) this.setStatus(Idle)

  //modifies status, can remain abstract if insert floors is encapsulated
  /**
    * will insert a floor as long as it is not the current one
    *
    * @param floor
    */
  private def insertFloor(floor: Int): Unit = if (this.currentFloor != floor) newFloor(floor)


  // adds a floor
  protected def newFloor(floor: Int): Unit

  //modifies status
  /**
    * will move one floor towards target
    */
  private def moveOne: Unit = {
    this._status match {
      case Idle => {
        //nothing to do
      }
      case Up => {
        goUpOneFloor
      }
      case Down => {
        goDownOneFloor
      }
    }
    if (this.destination == this.currentFloor) {
      // we got to destination
      // remove element
      this.openDoors
      this.closeDoors
    }
  }

  protected def goDownOneFloor: Unit

  protected def goUpOneFloor: Unit

  protected def destination: Int

}

