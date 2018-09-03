# Bucks #

Bucks is a clojurescript app that gives financial insights based on input from a csv file.

You maintain your data in a csv and bucks will draw some graphs and tell you some things like

* years to financial independence
* unitized performance of assets
* and many more things...

Latest version [here](https://sneakypeet.github.io/bucks/)

Hit the `?` for help and an example file

## Notes ##

* Salaries are assumed as monthly income. More than one salary row per month per person will yield weird results.

# DEV #


To run, Start figwheel in your repl of choice.

see example.cljs for an example of a pipe delimited csv file.
