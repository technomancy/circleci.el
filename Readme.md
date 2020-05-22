# CircleCI client for Emacs

Running `M-x circleci` gives you a buffer with a list builds steps for
any given project.  Press enter on a build to see steps, or press `g`
to refresh.  Use `C-u M-x circleci` to have it prompt for a specific
project and branch instead of infering from your `.git`
directory. `M-x circleci-latest` takes you directly to the latest
build for a project.

It will prompt you for a CircleCI token and can save that token
in `~/.authinfo.gpg` if you have your GPG key set up properly.

You will need to [create a CircleCI token](https://circleci.com/account/api).

Note that this is not officially supported by CircleCI.

To install, place it on your load-path and add an autoload:

    (add-to-list 'load-path "~/src/circleci.el")
    (autoload 'circleci "circleci" "List CircleCI builds" t)
    (autoload 'circleci-latest "circleci" "Show CircleCI build output" t)

No dependencies outside Emacs are required.

## Copyright

Copyright © 2017-2018 Phil Hagelberg and contributors

Licensed under the same license as Emacs (GPL v3 or later)
