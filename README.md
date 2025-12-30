![Erlang](https://img.shields.io/badge/Erlang-OTP-blue)
![Docker](https://img.shields.io/badge/Docker-enabled-blue)

# Erlang Cheat Sheet & Learning Notes

I started exploring Erlang mainly to better understand the actor model. Very quickly, I realized that in Erlang this model is so deeply integrated into the language and runtime that it is rarely explicit or “observable”, it simply is the way the system works.

Paradoxically, once I reached a point where I could start testing things, it felt like I had to learn Erlang backwards: many of the problems it solves are problems you only fully understand after you’ve already built large distributed systems in other ecosystems.

Over time, I began to see Erlang not just as a programming language, but as a set of well-established practices for building reliable, distributed systems, supported by a language and runtime designed specifically for that purpose.

## Erlang as a System, Not Just a Language

Erlang feels self-sufficient in a way few ecosystems do.

### Why self-sufficient?

Message passing and process isolation are built into the language and runtime. Many problems that would require external systems (for example, message brokers) in other stacks can often be solved directly within Erlang/OTP.

This doesn’t mean Erlang replaces tools like Kafka in all cases — but it does mean you should first ask why you need them if you are already using Erlang.

### Why powerful?

Erlang supports spawning millions of lightweight BEAM processes. These are not OS threads, but extremely cheap, isolated processes that can progress independently and fault independently, supervised by OTP.

### Why combine Erlang with other languages?

Erlang excels at coordination, routing, fault tolerance, and concurrency, but it is not always the most readable or ergonomic choice for complex parsing or heavy business logic.

For that reason, I personally see Erlang working best when kept focused and small:

* Erlang handles routing, supervision, distribution
* Another language (e.g. Java) handles complex parsing or domain logic
* Erlang orchestrates and supervises the whole system

## OTP as the Core Abstraction

To me, Erlang is best understood through OTP, which plays a role similar to frameworks like Spring in the Java world.

Typical OTP structure:

    * Application – manages the lifecycle of the system
    * Supervisor – monitors and restarts workers
    * Workers – gen_server, gen_statem, gen_event

Supervision in Erlang feels like Kubernetes-style resilience, but embedded directly into the language runtime instead of being delegated to infrastructure.

## Rough Analogy (with caution)

Reading Erlang from a Java background, I sometimes think of it as combining concerns that are usually split across:

    * Spark (distributed computation),
    * Kubernetes (process supervision),
    * Kafka (message-based interaction),

but inside a single coherent runtime model.

This analogy is imperfect, but it highlights why Erlang feels unusually complete as a platform.

## Learning Context & Honesty

All of this reflects a learning journey, not production experience.

I have not yet delivered a production system in Erlang. While following this ChatGPT-generated course, I ran into real issues, especially around library versions and integrations.
Those moments involved repeated debugging and research cycles, which can become tedious over time.

## Course Philosophy

This course is not about isolated Erlang examples.

Instead, it is structured as preparation for a production-like delivery:

* learning the language gradually
* building Dockerized workflows
* preparing Erlang releases
* simulating distributed deployments

The final goal is a distributed chat system built with OTP and Mnesia, deployed using Docker.

I intentionally avoided installing Erlang locally and worked entirely through Docker (erlang:27) to stay close to a production-style workflow.

Course Structure
* **Module 1**
  * Project template used across all modules.
  Includes Docker setup to build all chapters.

* **Module 2**
  * Application startup
  * Basic code & testing
  * Compilation via Docker
  * Introduction to rebar.config

* **Module 3**
  * Expanding basic Erlang knowledge.

* **Module 4**
  * Adding documentation (README) for build and run steps.

* **Module 5**
  * First encounter with Erlang concurrency and process communication.

* **Module 6**
  * Implementing a gen_server:
    * synchronous vs asynchronous calls

* **Module 7**
  * Supervisor + worker setup.
  * Testing crash and restart behavior.
  * Pure OTP resilience without external tools.

* **Module 8**
  * Introducing full Erlang releases and Dockerized deployments.

* **Module 9**
  * Distributed Erlang nodes forming a basic chat system (no persistence).

* **Module 10**
  * ETS & Mnesia:
    * ETS as fast in-memory storage
    * Mnesia adding transactions and disk persistence
  * module10.2 is the most complete iteration here.

* **Module 11**
  * REST API experiments.
  * Library compatibility challenges.
  * Personal conclusion: this is where Java feels more natural.

* **Module 12 (Final Project)**
    * Enhancing the chat system with:
      * Mnesia transactions
      * disk persistence
      * clearer separation between function calls and process handlers

### Final Architecture Overview

* chat_app – application lifecycle
* chat_sup – top-level supervisor
  * chat_store – Mnesia schema & tables
  * chat_registry – user management
  * chat_user_sup – supervisor for user sessions
    * chat_user_session – message handling
* chat_room – room broadcasting
* chat_user – external API

#### To execute all the tests run this command while in the root folder

./docker/scripts/run_tests_wsl.sh --dev

#### To execute a module without docker file

##### Start docker:
`docker run -it --rm -v $(pwd):/app -w /app erlang:27 bash`

##### Go to module directory compile it
`erlc -o . ./src/*.erl`

or this way for a single file: 

`erlc MyModule.erl`

##### Execute a function
`erl -pa . -s MyModule hello`

In the early chapters, functionality is validated through automated tests.
In the later stages, I chose to execute and observe the system manually to gain a deeper understanding of how the processes interact.

Although automated tests are usually the most effective learning tool, some uncertainty during development pushed me to rely more on hands-on experimentation.

## Final Thoughts

I’m satisfied with how far this project went.
From here, the sandbox is ready - future work is about refinement, tuning, and real-world experience. 