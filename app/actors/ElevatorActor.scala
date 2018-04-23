package actors

import java.util.UUID

import models.ElevatorStatus.Idle
import models.{Elevator, ElevatorStatus}

import scala.collection.mutable

class ElevatorActor(val _currentFloor: Int = 0,
                    val _currentShaft: String = UUID.randomUUID().toString.toUpperCase,
                    val _status: ElevatorStatus.EnumVal = Idle,
                    val _nextFloors: mutable.SortedSet[Int] = new mutable.TreeSet
                   ) extends Elevator {

  override def currentFloor: Int = this._currentFloor

  override def currentShaft: String = this._currentShaft

  override def currentStatus: ElevatorStatus.EnumVal = this._status

  override def goTo(floor: Int): Unit = ???

  override def effort(floor: Int): Int = ???

  override def move: Unit = ???

  override def nextFloors: Seq[Int] = this._nextFloors.clone().toSeq
}
