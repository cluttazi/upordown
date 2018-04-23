package models.elevator

import java.util.UUID

import models.ElevatorStatus.Idle
import models.{AbstractElevator, ElevatorStatus}

class NonAKKAElevator(var initFloor: Int = 0,
                      var initShaft: String = UUID.randomUUID().toString.toUpperCase,
                      var initStatus: ElevatorStatus.EnumVal = Idle,
                     ) extends AbstractElevator(
  _currentFloor = initFloor,
  _currentShaft = initShaft,
  _status = initStatus) {

  import ElevatorStatus._

  override protected def setStatus(status: EnumVal): Unit = this._status = status

  override protected def openDoors: Unit = this._nextFloors -= this._currentFloor

  override protected def newFloor(floor: Int): Unit = this._nextFloors += floor

  override protected def goDownOneFloor: Unit = this._currentFloor -= 1

  override protected def goUpOneFloor: Unit = this._currentFloor += 1

  override protected def destination: Int = {
    val first: Int = this._nextFloors.firstKey
    val last: Int = this._nextFloors.lastKey
    this.currentStatus match {
      case Up => first
      case Down => last
      case Idle => {
        val effortFirst = this.effort(first)
        val effortLast = this.effort(last)
        if (effortFirst < effortLast) first
        else last
      }
    }
  }

}

