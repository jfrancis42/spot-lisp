# spot-lisp
A Common Lisp client for reading location data from the public findmespot.com API.

Fill in the value for \*glld\* (which is supplied by the Spot web page when you create a shared page), the value for \*feedpassword\* (if you supplied one for the shared page), and the Google Geocoder API key (if you want to be able to do reverse geocoding).  If you don't have a password for your shared location page, set the \*feedpassword\* to nil (not "nil").

Start the reader thread with (start-spot).  This will update the \*locations\* variable with a list of the last fifty location objects, updating the list every 2.5 minutes (the maximum update rate allowed by the free API).  There's a mutex lock for \*locations\* called \*locations-lock\*, which it's wise to use when accessing \*locations\* (but technically not mandatory).

If you want to sit and watch the position data roll in, run (show-spots).  This will start an endless loop that updates every time new data shows up.

That's it.  Plain and simple.  Obviously, you can do far more with the information than simply display it, but this solves my specific problem quickly and easily.  It could probably do with more error detection (for network errors, or for lat/lon combinations that fail to reverse geocode properly), but it's good enough for now.

Feel free to use and adapt.

