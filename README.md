bindingModel
============

[Shiny](http://www.rstudio.com/shiny) web app that uses an ODE solver ([deSolve](http://cran.r-project.org/web/packages/deSolve/index.html)) to run a bimolecular binding simulation.

The simulation and key view elements are factored out in `model.R`, making it relatively easy to alter the core features of the application.

A quick tutorial for rna.wlu.edu users that walks through how you can fork and hack this simulation can be found [on the wiki](https://github.com/whitwort/bindingModel/wiki/Tutorial-for-rna.wlu.edu-users).


## Version

* 0.5   Greatly improved parameter/state interpolation (previously called the Summary tab, now the Simmulation tab)
* 0.4   Fixed divide by zero bugs
* 0.3   Improved model summary tab
* 0.2   Added a model summary feature
* 0.1   Intial release

## License

Copyright © 2016 Gregg Whitworth and licensed under [GPLv2](http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html).
