# On MacOS:

- Emacs 27 from brew https://github.com/d12frosted/homebrew-emacs-plus
  `brew install emacs-plus --with-emacs-27-branch --with-no-titlebar --HEAD`

# On Linux:

- Regolith
- Requires all-the-icons.el from Emacs
- gtk theme Material-Black themes/icons from `https://www.pling.com/p/1333360/` and `https://www.gnome-look.org/p/1316887/`

## Customizing regolith top bar

- for additional customizable icons, use `apt` to install i3xrocks-XXX (temp, weather, etc).
- Alternative for the above, from source `https://github.com/regolith-linux/regolith-i3xrocks-config`
- also, add those configurations to the `/.config/regolith/i3xrocks/conf.d/` folder

## Adding the focused-window-name add-on

- This requires i3ipc, a python package
- The script for `focused-window-name` has a shebang with `/usr/bin/python3`, which blows up if you have Conda installed
- Install `python3-pip` via `apt`, and then you can run `/usr/bin/python3 -m pip install i3ipc`

## Freshpaint specific updates

### Embedded postgres tests

When running tests locally, the embedded Postgres database doesn't accept the default
`LC_ALL=C.UTF-8` environment variable from Doom emacs. This variable is set at Doom start time

Open up the doom launch script:
`vim ~/.emacs.d/bin/doom`

Then modify the LC_ALL variable: `LC_ALL=en_US.UTF-8`.
After the modification, you should be able to launch a test and start Embedded postgres from Emacs.

### Freshpaint test binding (testify suite compatibility)

Because we use Testify in some places, we need a custom test binding/runner that can be executed
interactively. In order to run the custom Freshpaint test binding, we need to autoload
`+go--run-tests`

Navigate to the `modules/lang/go/autoload.el` file and add an autoload heading to the function
in order to have the custom test runner loaded at start time.
