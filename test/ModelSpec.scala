import models.ElevatorStatus.{Down, Idle, Up}
import models.{Elevator, ElevatorRequest, ElevatorSystem}
import org.scalatestplus.play._

/**
  * Unit tests for models.
  */
class ModelSpec extends PlaySpec {

  "Elevator" should {
    val elevator: Elevator = new Elevator(0)

    "be 0 upon initialization" in {
      elevator.currentFloor must equal(0)
    }

    "be Idle upon initialization" in {
      elevator.currentStatus must equal(Idle)
    }

    "move one floor up" in {
      elevator.goTo(1)
      elevator.move
      elevator.currentFloor must equal(1)
      elevator.effort(1) must equal(0)
    }

    "be Idle upon reaching destiny" in {
      elevator.currentStatus must equal(Idle)
    }

    "move one floor down" in {
      elevator.goTo(-1)
      elevator.move
      elevator.currentFloor must equal(0)
      elevator.effort(1) must equal(-1)
    }

    "be Down until reaching destiny" in {
      elevator.currentStatus must equal(Down)
    }

    "be Idle upon reaching a new destiny" in {
      elevator.goTo(-1)
      elevator.move
      elevator.currentFloor must equal(-1)
      elevator.currentStatus must equal(Idle)
    }

    "remain in the same floor" in {
      elevator.move
      elevator.currentFloor must equal(-1)
      elevator.currentStatus must equal(Idle)
    }

    "go from -1 to 2" in {
      elevator.goTo(3)
      elevator.move // 0
      elevator.currentStatus must equal(Up)
      elevator.currentFloor must equal(0)
      elevator.effort(1) must equal(1)
      elevator.move // 1
      elevator.currentStatus must equal(Up)
      elevator.currentFloor must equal(1)
      elevator.effort(1) must equal(0)
      elevator.goTo(2)
      elevator.move // 2
      elevator.currentStatus must equal(Up)
      elevator.currentFloor must equal(2)
      elevator.effort(1) must equal(-1)
      elevator.move // 3
      elevator.currentFloor must equal(3)
      elevator.currentStatus must equal(Idle)
      elevator.effort(1) must equal(2)
    }
  }

  "ElevatorSystem" should {

    "add a new Request" in {
      val request = ElevatorRequest(1, 1, false, 1)
      ElevatorSystem.addRequest(req = request)
      ElevatorSystem.requests.size must equal(1)
    }

    "add a new Elevator" in {
      ElevatorSystem.addElevator(new Elevator(0))
      ElevatorSystem.elevators.size must equal(1)
    }

    "perform a simulation" in {
      ElevatorSystem.simulation
    }
  }

}
