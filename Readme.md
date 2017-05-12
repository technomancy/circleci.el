# CircleCI for Emacs

Gives you a buffer with a list build steps for any given build when
you run `M-x circleci`. Press enter on a step to expand. Use `C-u M-x
circleci` to have it prompt for a specific project and branch instead
of infering from your `.git` directory.

It will prompt you for a CircleCI token and can save that token
in `~/.authinfo.gpg` if you have your GPG key set up properly.

You will need to [create a CircleCI token](https://circleci.com/account/api).

Note that this is not officially supported by CircleCI.

To install, place it on your load-path and add an autoload:

    (add-to-list 'load-path "~/src/circleci.el")
    (autoload 'circleci "circleci" "Show CI build output" t)

No dependencies outside Emacs are required.

## Copyright

Copyright © 2017 Phil Hagelberg and contributors

Licensed under the same license as Emacs (GPL v3 or later)
