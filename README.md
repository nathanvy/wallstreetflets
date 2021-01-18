# WALL STREET FLETS

wallstreetflets (wsf) is a utility library to compute the so-called Greeks and other properties of derivative securities (specifically options) using the Black-Scholes model.

## Usage

The meat and potatoes of the library are two functions:

* `compute-greeks` which takes certain parameters that one can scrape from most finance sites and computes the greeks for the specified option and returns them as a plist; and
* `print-greeks` which takes the plist and prints it in a readable form to stdout

## Risk-Free Rate

One of the inputs to the Black-Scholes model is the "risk-free rate" which is a theoretical value.  Customarily for US securities the interest rate on a US Treasury Bill with the same maturity is used as a proxy.  Wallstreetflets scrapes data from the US Treasury website to calculate the risk-free rate, but in the event the website cannot be reached or scraped, a fallback rate of 0.1% is used.

## Disclaimers

There might be bugs herein that might cost you money.  Before investing carry out your own due diligence, including on the source code of this library.

I develop on SBCL and may occasionally test this code on CCL.  No other Common Lisp implementations are tested.
