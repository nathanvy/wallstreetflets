# WALL STREET FLETS

wallstreetflets (wsf) is a utility library to compute the so-called Greeks and other properties of derivative securities (specifically options) using the [Black-Scholes model](https://en.wikipedia.org/wiki/Black%E2%80%93Scholes_model).

## Usage

The meat and potatoes of the library are two functions:

* `compute-greeks` which takes certain parameters that one can scrape from most finance sites and computes the greeks for the specified option and returns them as a plist; and
* `print-greeks` which takes the plist and prints it in a readable form to stdout

## Example

At the time of writing, MSFT is trading at $212.65 per share and the 19 Feb $190 Put has 33 days to expiry with an implied volatility of 35.19%.  To compute the Greeks for this put:

```
CL-USER> (wsf:compute-greeks 0.3519 190 212.65 33 t)
(:DELTA -0.13178630823047255d0 :GAMMA 0.009490863086237849d0 :VEGA
 0.13654531297316355d0 :THETA -0.0727376753241109d0 :RHO
 -0.026847670995063383d0)
 
CL-USER>
```

## Risk-Free Rate

One of the inputs to the Black-Scholes model is the "risk-free rate" which is a theoretical value.  Customarily for US securities the interest rate on a US Treasury Bill with the same maturity is used as a proxy.  Wallstreetflets scrapes data from the US Treasury website to calculate an appropriate risk-free rate, but in the event the website cannot be reached or scraped, a conservative fallback rate of 1.5% is used.

Users can configure this fallback rate with the exported symbol `*fallback-rate*`.

## Disclaimers

There might be bugs herein that might cost you money.  Before investing carry out your own due diligence, including on the source code of this library.

I develop primarily on/for SBCL and occasionally test this code on CCL.  No other Common Lisp implementations are tested but pull requests are welcome.
