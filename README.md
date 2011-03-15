Not Production Ready
====================

## System Goals

### runtime performance (for a change)
It's not ready, but my intention is to expose this as a web-app at some point. That implies that I'll want a short parse/write time for a given map. It hasn't happened yet (I get the feeling it's mainly because of my liberal regex use, but I haven't whipped out the profiler yet. We'll see). It'll be kind of hard to optimize output since that involves mapping over the entire list of tags, so this basically translates to "very fast read time".

### end user flexibility
This is one reason I don't use an XML-parser like [others](http://davidlynch.org/blog/2008/03/creating-an-image-map-from-svg/) who have attempted an svg to imagemap converter (actually, he seems to be the only one)
- Firstly because deserializing XML seems like it would be slower than parsing for relevant tags 
- Second beccause I can get away with it (all but one of the tags relevant to conversion are self-terminating, and I only need the opening tag of that one)
- Third because it seems like it would be more difficult to get these tags out through a xpath expressions rather than through straight search (tags can be arbitrarily grouped, which means I can't just get all children of `svg/`)
- Finally because XML parsers tend to barf when some jackass from marketing gives you an RTF file instead of an SVG (or a partial copy of an SVG). I would prefer this not barf if it can get any data at all out of the input.

These are notes for the author rather than usage notes
------------------------------------------------------

## The relevant tags are
- **polygon** and **polyline** are basically the same. They're a string of `x, y` pairs in the `points` attribute. Just get them, round them (they can be floats in SVG, but seem to be restricted to ints for area tags) and output as a comma separated list (In SVG, they're `points="x,y x,y x,y..."`, in area tags they're `coords="x,y,x,y,x,y..."`
- **rect** is slightly more complicated. Four relevant attributes: `x`, `y`, `width`, `height` in SVG. The area tags require two `x,y` pairs (top left and bottom right). In other words, `x,y (x+width),(y+height)`
- **circle** has the same three attributes in SVG and area tags. The coordinates of the center and the radius. They're stored differently though; SVG stores it as three attributes (`cx`, `cy`, `r`) area tags represent it as a comma separated list in the `coords` property
- **path** is basically a stripped down implementation of PS in a tag. Right now, it gets parsed and treated the same way as **polygon** and **polyline**, but that'll mishandle the H and V directives (because they each provide single points instead of point pairs). Other than that, it actually works fine.

## TODO

- implement real **path** support. Really, this just means checking if a given directive is marked `H` or `V` and generating an appropriate point for it. Generating the second coordinate could concievably be done at write-time, but I'd still need to track input to make sure that they got stored as single numbers rather than paired randomly.

- improve parser performance. I'm using regexes all over the place, which isn't the best idea speed-wise, but it was good to get it running. This is actually enough for my personal purposes, but it takes about 4 seconds just to parse [this](http://en.wikipedia.org/wiki/File:North_america98.svg), which I get the feeling I can improve.

## Other notes

- The `readers.lisp` file is currently unused, and I've yet to put together a system definition
