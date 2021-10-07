# A competitive programmatic building game.
**Dgcl0** turns the engineering challenge of designing a robust machine into a competitive sport.

Dgcl0 is a top-down sandbox game, where players design and program autonomous war machines, then battle them against each other.
Vehicles are defined in Common Lisp, and players utilize the language to write thier vehicles' AI and hardware.

Vehicles split and break apart as they take damage. There are no health bars. Vehicles are declared destroyed when they are missing so many components that thier software crashes.

## About the Project

Dgcl0 is pre-1.0, and currently in active development. The core logic is fully implemented; The current focus is on documentation and presentation.

* Documentation:
Due to the nature of Dgcl0, writing vehicles requires frequent use of the API Reference. Until the documentation is complete, Dgcl0 is very frustrating to play.

* Presentation:
Presently, to display output, Dgcl0 just prints out it's worldstate on every frame. This is... bad. A GUI and TUI are planned to replace this behavior.
