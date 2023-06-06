# Collatz Lines

An attempt to draw lines in a generative manner, by using the set of rules of the Collatz Conjecture.  
The idea comes from the reading of the [aRtsy project](https://cran.r-project.org/web/packages/aRtsy/readme/README.html).  
While I still didn't try to use the aRtsy package, their example of drawing lines following the Collatz Conjecture inspired this small project of mine. Especially, the core of the code, which changes the slope of the line depending on whether one element of the Collatz sequence is even or odd was directly inspired by the [respecitve function from the aRtsy package](https://github.com/koenderks/aRtsy/blob/development/src/canvas_collatz.cpp).