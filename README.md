# This codebase is retired.
A rewrite is being pursued.

This project started as a cool learning experiment, and the finished product was even kind of fun. However, the interface rework mentioned below turned into a massive rabbit hole, and the core logic is now supposed to be a C library. SBCL isn't very good at being called from C (it's documentation mentions that it is "sometimes possible, but extremely hackish"), so I decided to rewrite the whole thing in a language that is. (Plus, this gives me an excuse to learn a new language!)

The readme as it stood just before this decision is below. Some unfinished documentation is also available on the `seth/docs/working` branch.

# A competitive programmatic building game.
**Dgcl0** turns the engineering challenge of designing a robust machine into a competitive sport.

Dgcl0 is a top-down sandbox game, where players design and program autonomous war machines, then battle them against each other.
Vehicles are defined in Common Lisp, and players utilize the language to write thier vehicles' AI and hardware.

Vehicles split and break apart as they take damage. There are no health bars. Vehicles are declared destroyed when they are missing so many components that thier software crashes.

## About the Project

Dgcl0 is pre-1.0, ~~and currently in active development~~. The core logic is fully implemented, but a *lot* of work still needs to be done. Here's what the roadmap looks like right now:

* Interface:
After some playtesting, I realized the current API is very un-fun to work with. It's very restrictive, which leads to vehicles being written in a non-idiomatic way. I'm planning a major API rework, which might require more changes deeper within Dgcl0. I also want to add some sandboxing with this rework.

* Documentation:
Due to the nature of Dgcl0, writing vehicles requires frequent use of the API Reference. Until the documentation is complete, Dgcl0 is very frustrating to play.

* Presentation:
Presently, to display output, Dgcl0 just prints out it's worldstate on every frame. This is... bad. A GUI and/or TUI are planned to replace this behavior.
