# Dram

**Not ready yet.  Do not use this.**

Clojure templating that won't make you drink.

Dram is a Clojure templating library for Django/Jinja-like templates.

**Not ready yet.  Do not use this.**

Usage
-----

Don't yet.

Why?
----

Because I wasn't happy with any of the existing Clojure templating libraries.

Clostache and friends implement Mustache, which is a crippled templating
library.  It doesn't have template inheritance (without which I'd go crazy in
the real world).

It also takes a very strict approach to having logic in templates (basically:
"don't").  But sometimes you *want* logic in the templates, if it belongs there!

Hiccup is useless once you hire a frontend developer.

> Frontend Dev: "Okay I'm ready to start, where's the HTML?"
> 
> Backend Dev: "It's produced by these files full of s-expressions, vectors, and
> hashes.  There are even some API docs!"
> 
> Frontend Dev: "Did you just tell me to go fuck myself?"
> 
> Backend Dev: "I believe I did, Bob."

Enlive pushes logic into the Clojure layer where frontend developers can't
effectively get at it.  From my limited experience it also seems like you need
to write a lot of custom Clojure code when you're using it, even for things as
simple as for loops.

It might be great for teams where everyone knows Clojure.  I dunno.  It's pretty
crazy.  It's certainly not something your frontend developer is going to pick up
in a week.

Django/Jinja-style templates are easy to learn for frontend developers and
provide a good mix of power and simplicity.

License
-------

Copyright 2012 Steve Losh and contributors.

Distributed under the MIT/X11 license.
