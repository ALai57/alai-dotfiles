
# On MacOS:
- Yabai (window manager) and SKDH (hotkeys)
- Limelight (highlighting windows)
- Spacebar (nicely formatted top toolbar)
- Emacs 27 from brew https://github.com/d12frosted/homebrew-emacs-plus
`brew install emacs-plus --with-emacs-27-branch --with-no-titlebar --HEAD`

# On Linux:
- Regolith
- Requires all-the-icons.el from Emacs
- gtk theme Material-Black themes/icons from `https://github.com/rtlewis88/rtl88-Themes/tree/material-black-COLORS`

## Customizing regolith top bar
- for additional customizable icons, use `apt` to install i3xrocks-XXX (temp, weather, etc).
- Alternative for the above, from source  `https://github.com/regolith-linux/regolith-i3xrocks-config`
- also, add those configurations to the `/.config/regolith/i3xrocks/conf.d/` folder

## Adding the focused-window-name add-on
- This requires i3ipc, a python package
- The script for `focused-window-name` has a shebang with `/usr/bin/python3`, which blows up if you have Conda installed
- Install `python3-pip` via `apt`, and then you can run `/usr/bin/python3 -m pip install i3ipc`
