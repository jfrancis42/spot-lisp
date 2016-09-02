# spot-lisp
A Common Lisp client for reading location data from the public
findmespot.com API.

This library is capable of running in two different modes. It can
fetch Spot data on demand from the official Spot API, or it can start
a thread that keeps a local copy of your Spot data and updates it
every 150 seconds (the shortest update rate allowed by Spot). The
advantages of using the local cached data include quicker results and
not having to manage repeated lookups to obtain new/current data. The
down side is that the thread code is not yet 100% reliable, and the
thread dies if there is any interruption in network connectivity. The
code that pulls directly from the API, however, works perfectly. This
is on the ToDo list to fix (or at least handle the error properly and
continue).

The publicly available calls in this client are as follows:

* (start-spot ...)
* (get-all-spots-api ...)
* (get-newest-spot-api ...)
* (get-all-spots-local)
* (get-newest-spot-local)
* (lat-lon-to-street-address ...)
* (spot-street-address ...)

(start-spot ...) starts the thread that fetches all of the latest data
from the Spot API automatically at 150 second intervals, and caches
that data locally. Once started, this thread continues indefinitely,
and in theory, should loop forever. This function requires one
argument, with a second optional argument. The first argument is the
GLLD, which is supplied by Spot. The GLLD is created when you create a
page to share your Spots. The second argument is the optional password
for that page. If you created the page with no password, then provide
only the GLLD to this function. Example:

```
(spot:start-spot "1btCzzrfUvKHdylmX36hAqM178VDVy72i" "SecretSquirrel")
```

There are two functions to pull data from this local cache:
(get-all-spots-local), which returns a list of all known Spot
locations, and (get-newest-spot-local), which returns only the newest
Spot location. Neither call requires any additional arguments.

Data can also be pulled directly from the Spot API on demand, rather
than caching it locally. The two functions which do this are
(get-all-spots-api ...) and (get-newest-spot-api ...). Both calls
require the feed's GLLD and password (assuming a password was created
on the Spot page).

If you have an API key to do reverse geocoding lookups with the Google
Geocoding Server (available for free from Google), you can also turn a
spot location into an address (assuming an address is available for
the given location) by calling (spot-street-address ...) with a single
location and your Google API key.  Example:

```
(spot-street-address (get-newest-spot-local) "AIzaSyDjZGEULwh6xyChPxSnVPEg9Sg9MUumTL8")
```

You can also look up the address of any arbitrary lat/lon using
(lat-lon-to-street-address ...) by supplying a latitude, longitude,
and your Google API key as arguments.

The fields in each location object match those described in the Spot
API documentation, with one exception: a Unix time_t timestamp is
added for convenience. The available fields are: id, messenger-id,
unix-time, message-type, latitude, longitude, model-id,
show-custom-msg, date-time, battery-state, and hidden. Example:

```
CL-USER> (spot:latitude (get-newest-spot-local))
47.79126
CL-USER>
```

Last, but not least, (pp ...) is a method that will return a nice
one-line summary string of the current object. Example:

```
CL-USER> (spot:pp (get-newest-spot-local))
"lat:42.49026 lon:-102.14314 type:TRACK batt:GOOD time:2016-08-30T07:42:30.000000-07:00"
CL-USER>
```
