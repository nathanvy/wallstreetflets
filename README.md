# WALL STREET FLETS

wallstreetflets (wsf) is a utility library to compute the so-called Greeks and other properties of derivative securities (specifically options) using the Black-Scholes model.

## Risk-Free Rate

One of the inputs to the Black-Scholes model is the "risk-free rate" which is a theoretical value.  Customarily for US securities the interest rate on a US Treasury Bill with the same maturity is used as a proxy.  Wallstreetflets scrapes data from the US Treasury website to calculate the risk-free rate, but in the event the website cannot be reached or scraped, a fallback rate of 0.1% is used.

## Disclaimer

There might be bugs herein that might cost you money.  Before investing carry out your own due diligence, including on the source code of this library.
