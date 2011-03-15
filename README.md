Not Production Ready
====================

These are notes for the author rather than usage notes
------------------------------------------------------

### The relevant tags are
- **polygon** and **polyline** are basically the same. They're a string of `x, y` pairs in the `points` attribute. Just get them, round them (they can be floats in SVG, but seem to be restricted to ints for area tags) and output as a comma separated list (In SVG, they're `points="x,y x,y x,y..."`, in area tags they're `coords="x,y,x,y,x,y..."`
- **rect** is slightly more complicated. Four relevant attributes: `x`, `y`, `width`, `height` in SVG. The area tags require two `x,y` pairs (top left and bottom right). In other words, `x,y (x+width),(y+height)`
- **circle** has the same three attributes in SVG and area tags. The coordinates of the center and the radius. They're stored differently though; SVG stores it as three attributes (`cx`, `cy`, `r`) area tags represent it as a comma separated list in the `coords` property
- **path** is basically a stripped down implementation of PS in a tag. Right now, it gets parsed and treated the same way as **polygon** and **polyline**, but that'll mishandle the H and V directives (because they each provide single points instead of point pairs). Other than that, it actually works fine.

## TODO

- implement real **path** support. Really, this just means checking if a given directive is marked `H` or `V` and generating an appropriate point for it. Generating the second coordinate could concievably be done at write-time, but I'd still need to track input to make sure that they got stored as single numbers rather than paired randomly.

- improve parser performance. I'm using regexes all over the place, which isn't the best idea speed-wise, but it was good to get it running. This is actually enough for my personal purposes, but it takes about 4 seconds just to parse [this](http://en.wikipedia.org/wiki/File:North_america98.svg), which I get the feeling I can improve.

## Other notes

- The `readers.lisp` file is currently unused, and I've yet to put together a system definition
