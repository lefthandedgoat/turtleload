Turtle load

This project is a work in progress.
The primary goal is to be a simple performance measuring and load generation tool.

Initial goal is to replace Fiddler (windows) and Charles (OSX) to replay requests for a website or api.
Other goals:
  * Work well on windows and *nix
  * Be a simple suave exe that hosts a website
  * Configurations are stored in simple files for easy versioning with source control
  * Results are also stored in simple files for the same results
  * Multiple users can run tests and share their results by commiting the files
  * Reporting, history, comparing multiple runs
  * Live results to the browser while the test runs
  * Support multiple workers and different scenarios like:
    * Try to find how much load the server can take before tipping over
    * Measure lower load over a longer period of time to find outliers
    * more?
