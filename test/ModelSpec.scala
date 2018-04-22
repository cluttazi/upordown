import models.ElevatorStatus.{Down, Idle, Up}
import models.{Elevator, ElevatorRequest, NonAKKAElevator, NonAKKAElevatorControlSystem}
import org.scalatestplus.play._

/**
  * Unit tests for models.
  */
class ModelSpec extends PlaySpec {

  "Elevator" should {
    val elevator: NonAKKAElevator = new NonAKKAElevator(0)

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
      NonAKKAElevatorControlSystem.addRequest(req = request)
      NonAKKAElevatorControlSystem.requests.size must equal(1)
    }

    "add a new Elevator" in {
      NonAKKAElevatorControlSystem.addElevator(new NonAKKAElevator(0))
      NonAKKAElevatorControlSystem.elevators.size must equal(1)
    }

    "modify an Elevator" in {
      val elevator: Elevator = new NonAKKAElevator(_currentShaft = "random")
      NonAKKAElevatorControlSystem.addElevator(elevator)
      val anotherElevator: Elevator = new NonAKKAElevator(_currentShaft = "random")
      anotherElevator.goTo(5)
      anotherElevator.move //1
      anotherElevator.move //2
      anotherElevator.move //3
      anotherElevator.move //4
      NonAKKAElevatorControlSystem.update(anotherElevator)
      NonAKKAElevatorControlSystem.elevators.length == 1
      NonAKKAElevatorControlSystem.elevators.last.currentFloor must equal(4)
    }

    "perform a simulation" in {
      NonAKKAElevatorControlSystem.simulation
    }
  }

}
