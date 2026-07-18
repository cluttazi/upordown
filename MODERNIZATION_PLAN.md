# Modernization Plan — upordown

Audit date: 2026-07-18. Environment: JDK 21.0.10 (only JDK available), sbt
launcher 1.11.7, Gradle 8.14.3.

## 1. What this repo is

A Play Framework starter app (Scala) hosting an elevator-control-system
simulator (`app/models/**`), with a plain-Scala scheduler implementation, a
stub Akka-actor variant, and ScalaTest/scalatestplus-play tests (unit,
model/simulation, functional routes, headless-browser smoke test).
~700 lines of Scala.

## 2. Which build is authoritative

**sbt is the authoritative build.** Evidence:

- `.github/workflows/ci.yml` runs only `sbt test`.
- README calls sbt the "primary build" (Gradle listed as an "alternative").
- The Gradle build uses the legacy *software-model* `play` plugin, which was
  **removed from Gradle core in Gradle 6.0** (2019). The wrapper pins
  Gradle 4.4 (2017), which cannot run on Java 21 (and its distribution
  download is blocked in this environment anyway). Verified: with system
  Gradle 8.14.3, `gradle check` fails immediately with
  `Plugin [id: 'play'] was not found`.
- `scripts/test-gradle`, `scripts/test-sbt`, `scripts/script-helper` are
  Travis-CI-era leftovers (reference `$TRAVIS_SCALA_VERSION`, Java 9
  workarounds); Travis config itself is already gone.

Conclusion: the Gradle build and Travis scripts are **clearly dead** and will
be removed.

## 3. Baseline build/test status (before)

| Command | Result |
|---|---|
| `sbt test` | **Won't run.** sbt 1.1.2 (pinned in `project/build.properties`) crashes at launch on Java 21: `UnsupportedOperationException: The Security Manager is deprecated` (Security Manager removed in JDK ≥ 18 semantics; sbt < 1.6 requires it). Even past launch, Scala 2.12.4 cannot compile on JDK 21 (JDK 21 support arrived in 2.12.18). |
| `./gradlew check` | **Won't run.** Wrapper needs Gradle 4.4 from services.gradle.org (blocked; and Gradle 4.4 doesn't support Java 21). |
| `gradle check` (system 8.14.3) | **Fails**: `Plugin [id: 'play'] was not found` — legacy Play plugin removed from Gradle ≥ 6. |

So there is no runnable baseline on Java 21; the toolchain upgrade is a
prerequisite for running any tests at all.

## 4. Dependency inventory (current → latest stable, cutoff Jan 2026)

| Dependency | Current | Latest stable | Notes |
|---|---|---|---|
| sbt | 1.1.2 (2018) | 1.11.7 | Current pin cannot launch on JDK ≥ 18. |
| Scala | 2.12.4 (cross 2.11.12) | 2.13.18 (2.x line) | 2.11 is EOL (2017). 2.12.4 predates JDK 21 support. Scala 3.x exists but Play supports 2.13 fully; staying on 2.13 is the conservative move. |
| Play Framework | 2.6.13 (`com.typesafe.play`, 2018, **EOL**) | 3.0.11 (`org.playframework`) | Play 2.6 is long EOL with multiple published CVEs fixed in later lines. Play 3.0 = Play 2.9 API with Apache Pekko instead of Akka (Akka ≥ 2.7 relicensed to BSL; Pekko is the open-source fork). Supports Java 11/17/21. |
| Akka (transitive via Play) | 2.5.x | Apache Pekko 1.x (via Play 3.0) | Import change `akka.*` → `org.apache.pekko.*` in 2 files. |
| scalatestplus-play | 3.1.2 | 7.0.2 | 7.0.x targets Play 3.0 / ScalaTest 3.2 / Selenium 4. |
| H2 | 1.4.196 (2017) | 2.4.240 | **Known CVEs** in 1.4.x: CVE-2021-42392 (RCE via JNDI), CVE-2022-23221, CVE-2021-23463. But the dependency is **unused**: no db configured (`conf/application.conf` db section fully commented), no code references. Remove it. |
| Gradle build (Play plugin, jcenter) | Gradle 4.4 / play software-model plugin | — | Dead (see §2); also uses `jcenter()`, shut down 2021. Remove. |

## 5. Prioritized checklist

1. [x] **Audit + this plan** (`docs:` commit).
2. [ ] **Toolchain**: sbt → 1.11.7; Scala → 2.13.18 (drop 2.11 cross-build);
   Play sbt-plugin → `org.playframework:sbt-plugin:3.0.11`; drop obsolete
   snapshot resolver.
3. [ ] **Dependencies**: scalatestplus-play → 7.0.2; remove unused,
   CVE-laden H2.
4. [ ] **Migration changes** (Play 2.6 → 3.0, Scala 2.12 → 2.13):
   `akka.actor` → `org.apache.pekko.actor` (AsyncController, UnitSpec);
   deprecated static `play.api.Logger` → instance logger
   (ApplicationTimer); `akka {}` config block → `pekko {}`; any Scala 2.13
   collection fallout. Verify with `sbt test`.
5. [ ] **Remove dead Gradle build + Travis scripts**: `build.gradle`,
   `gradlew`, `gradlew.bat`, `gradle/`, `scripts/`.
6. [ ] **Real bug fix + regression test**: in
   `NonAKKAElevatorControlSystem.simulation`, `r.nextInt(maxElevators)` can
   yield **0 elevators** on a fresh system, which either (a) infinite-loops
   (pending requests can never be served) or (b) crashes at t=101 with
   `UnsupportedOperationException: empty.reduce`. The existing test only
   passes because it runs against a shared singleton that already holds
   elevators from earlier test cases. Fix: always create ≥ 1 elevator and
   replace `.map(...).reduce(_ || _)` with `.exists(...)` (total on empty).
7. [ ] **README / .gitignore reality check**: drop Gradle/JDK 8 claims,
   update badges (Scala 2.13 / Play 3.0), document Java 21 requirement.
8. [ ] **Security notes**: hardcoded `play.http.secret.key` in
   `application.conf` — fine for a demo, but allow `APPLICATION_SECRET` env
   override and say so.
9. [ ] **CI**: setup-java temurin **21**, keep checkout@v4/setup-java@v4,
   sbt caching, `sbt test` — mirroring the locally verified command.
10. [ ] Final: update this plan with Done vs Deferred + summary.

## 6. Explicitly out of scope (deferred)

- **Scala 3 migration** — Play 3 supports it, but it is a larger rewrite
  with no behavioral payoff for this codebase.
- **Completing `actors/ElevatorActor`** — the actor variant is a stub of
  `???`s by design ("in progress" per README); giving it behavior is a
  feature, not modernization.
- **Simulation realism issues** (destinations can exceed the 120-floor
  bound, unseeded RNG in `simulation`) — behavior-preserving scope only;
  noted here for a future change.

## 7. Done vs Deferred

### Done

- **Toolchain**: sbt 1.1.2 → 1.11.7; Scala 2.12.4 → 2.13.18 (2.11
  cross-build dropped); Play 2.6.13 → 3.0.11 (`org.playframework`, Apache
  Pekko); obsolete snapshot resolver removed.
- **Dependencies**: scalatestplus-play 3.1.2 → 7.0.2; unused H2 1.4.196
  removed (known RCE CVEs, no datasource configured).
- **Migration**: `akka.actor` → `org.apache.pekko.actor` (AsyncController,
  UnitSpec); `akka {}` → `pekko {}` config block; deprecated static
  `play.api.Logger` → instance logger (ApplicationTimer); `.toSeq` for
  Scala 2.13 immutable-`Seq` in `queryStatus`; deprecated procedure syntax
  fixed in `AbstractElevator`.
- **Dead code removed**: legacy Gradle build (software-model `play` plugin,
  Gradle 4.4 wrapper, jcenter) and Travis-era `scripts/`.
- **Bug fix + regression test**: `simulation` on a fresh control system
  could infinite-loop or crash when the RNG drew zero elevators; now always
  creates ≥ 1 elevator and uses `exists` instead of `empty.reduce`. Seeded
  regression test added; verified it hangs without the fix.
- **Docs/config hygiene**: README matches reality (Play 3 / Scala 2.13 /
  JDK 17+/21, no Gradle); `.gitignore` Gradle entries dropped;
  `play.http.secret.key` overridable via `APPLICATION_SECRET`.
- **CI**: JDK 8 → 21 (checkout@v4 / setup-java@v4 temurin / sbt cache /
  sbt/setup-sbt@v1 already current); runs the locally verified `sbt test`.

Test status: **before** — nothing runnable on Java 21 (sbt launcher crash);
**after** — `sbt test`: 21/21 tests pass (20 original + 1 new regression
test), including the HtmlUnit browser spec.

### Deferred

- Scala 3 migration (larger rewrite, no behavioral payoff here).
- Implementing the `ElevatorActor` stub (feature work, not modernization).
- Simulation realism: destinations can exceed the 120-floor bound /
  go negative; `simulation`'s RNG is the unseeded global `scala.util.Random`
  (non-reproducible runs); `NonAKKAElevatorControlSystem` singleton shares
  mutable state across tests.
- Twirl welcome/index pages are still the stock Play scaffold pages.

## 8. Summary (PR-description style)

**What changed**

- Migrated the build from EOL Play 2.6 / Scala 2.12.4 / sbt 1.1.2 (unable
  to even launch on the only available JDK, 21) to Play 3.0.11 /
  Scala 2.13.18 / sbt 1.11.7, with the minimal source changes that requires
  (Pekko imports, instance logger, `.toSeq`, `pekko` config block).
- Removed the dead legacy Gradle build and Travis helper scripts; sbt is
  the single build, as CI and README already assumed.
- Dropped the unused H2 1.4.196 dependency (CVE-2021-42392 et al.).
- Fixed a real hang/crash bug in `NonAKKAElevatorControlSystem.simulation`
  (zero-elevator draw) with a seeded regression test.
- README/.gitignore/CI brought in line with the above; app secret is now
  overridable via `APPLICATION_SECRET`.

**Risks**

- Framework major-version jump (2.6 → 3.0): behavior of the Play runtime
  (server is now Pekko HTTP based instead of Netty by default) could differ
  at the edges; the full test suite (routes, controllers, browser smoke
  test, simulation) passes.
- `simulation` now guarantees ≥ 1 elevator per run — a deliberate,
  documented behavior change on a previously broken path.

**Untouched**

- All public model/controller APIs, routes, views, scheduling logic, and
  the `ElevatorActor` stub.
