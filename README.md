# compost

A small message board for small groups.


## Build and Run

If you have sbcl installed you can just

    sbcl --load build.lisp 
    
This will produce a `compost` binary, which you can run like:

    compost 5005 https://foo.bar.com 
    
The first argument is the localhost port (assumes you're running
behind a reverse proxy) and the second is the root host name, used for
link generation. 

The first time you run the compost, it will look for a file called
`$HOME/compost.conf` that should contain usernames and passwords for
at least one user.  This file can be deleted after the initial boot.

Here is an example:

    ;;;; compost.conf
    
    (:users
     ("alice" "secret1")
     ("bob" "secret2"))
    

Perhaps further configuration parameters settings will be added in the
future (like port, site name, whether or not to start swank, etc).

## License

The compost code is released under AGPLv3.

Licences for dependences must also be provided with stand-alone
binaries of this app.  They're mostly MIT and BSD licenses. Look at
the `compost.asd` file in this repository for details about
dependencies.
