# Up or Down? — Elevator System Simulator

[![CI](https://github.com/cluttazi/upordown/actions/workflows/ci.yml/badge.svg)](https://github.com/cluttazi/upordown/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Scala](https://img.shields.io/badge/Scala-2.13-red.svg)](https://www.scala-lang.org/)
[![Play](https://img.shields.io/badge/Play%20Framework-3.0-92d13d.svg)](https://www.playframework.com/)

An elevator control system simulator built with Scala and the Play Framework, designed around a pluggable scheduling interface with both a plain-Scala and an actor-based implementation.

## Overview

The control system models the four core responsibilities of an elevator scheduler:

1. **Querying state** – which floor each elevator is on and where it is heading
2. **Status updates** – receiving updates about an elevator's position and direction
3. **Pickup requests** – accepting floor/direction requests and assigning the best elevator
4. **Time-stepping** – advancing the simulation one tick at a time

When a pickup request arrives, the system evaluates each elevator as an `ElevatorOption` — scoring it by *effort* (distance/direction cost to serve the floor) and current *queue* size — and dispatches the best candidate. The simulation supports up to 16 elevators and 120 floors (see `models/utils/Enums.scala`).

## Design

```
app/
├── models/
│   ├── ElevatorControlSystem.scala     # Scheduler contract (query, update, pickup, step)
│   ├── Elevator.scala                  # Elevator contract
│   ├── AbstractElevator.scala          # Shared elevator state/behaviour
│   ├── elevator/NonAKKAElevator.scala  # Plain-Scala elevator implementation
│   └── elevatorcontrolsystem/
│       └── NonAKKAElevatorControlSystem.scala
├── actors/ElevatorActor.scala          # Actor-based elevator (in progress)
└── controllers/                        # Play HTTP endpoints
```

Two implementation strategies coexist behind the same interfaces:

- **Non-Akka** – synchronous, mutable-state implementation used by the simulation tests
- **Actors** – message-driven variant where each elevator is an actor (Apache Pekko, the open-source continuation of Akka)

## Tech Stack

- Scala 2.13
- Play Framework 3.0 (built on Apache Pekko) with Guice dependency injection
- Apache Pekko actors
- ScalaTest + [scalatestplus-play](https://github.com/playframework/scalatestplus-play) (unit, functional and headless-browser specs)
- sbt

## Getting Started

### Prerequisites

- JDK 17 or 21
- [sbt](https://www.scala-sbt.org/)

### Run the Tests

```bash
sbt clean test
```

The suite covers the elevator model, the control-system scheduling logic (including a full simulation run), the HTTP routes, and a headless browser smoke test.

### Run the Application

```bash
sbt run
```

Then browse to <http://localhost:9000>.

## Continuous Integration

Every push and pull request runs the full sbt test suite on JDK 21 via [GitHub Actions](.github/workflows/ci.yml).

## License

This project is licensed under the MIT License – see [LICENSE](LICENSE) for details.
