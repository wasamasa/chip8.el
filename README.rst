chip8.el
========

.. image:: https://raw.github.com/wasamasa/chip8.el/master/img/zero.gif

About
-----

An emulator for the `CHIP-8 <https://en.wikipedia.org/wiki/CHIP-8>`_
VM, a target for many classic games.

Installation
------------

Install manually or via `Quelpa <https://github.com/quelpa/quelpa>`_
for the time being.

Usage
-----

You may want to customize ``chip8-keys`` to a keymap fitting your
keyboard.  Given the default value of ``chip8-keys-qwerty``, the hex
keypad is mapped to the following keys:

============= =============
Key           Value
============= =============
1 / 2 / 3 / 4 1 / 2 / 3 / C
q / w / e / r 4 / 5 / 6 / D
a / s / d / f 7 / 8 / 9 / E
z / x / c / v A / 0 / B / F
============= =============

Other customizables of interest are ``chip8-speed-factor`` (amount of
cycles executed on each timer run) and ``chip8-key-timeout`` (number
of seconds a key is considered pressed after an event).

To boot up a ROM, type ``M-x chip8-emulate RET <path/to/rom> RET``.

There's a basic set of controls for the emulator, ``p`` toggles
between play/pause, ``Q`` pauses before quitting the window and ``g``
resets the currently playing ROM.

Sound emulation
---------------

Unfortunately it's hard to play sounds in Emacs in a cross-platform
and non-blocking way, for this reason this emulator defaults to
printing a "beep" message whenever the game sounds the buzzer.  The
following hack allows you to play a sound, provided you are on a Linux
system with ``mpv`` installed:

.. code:: elisp

    (defvar my-chip8-mpv-fifo (expand-file-name "/path/to/mpv-fifo")
    (defvar my-chip8-beep-file (expand-file-name "/path/to/beep.ogg"))

    (defun my-chip8-make-fifo ()
      (shell-command-to-string (concat "mkfifo " my-chip8-mpv-fifo)))

    (defun my-chip8-write-to-fifo (string)
      (with-temp-buffer
        (insert string)
        (write-region (point-min) (point-max) my-chip8-mpv-fifo nil 'nomessage)))

    (defun my-chip8-beep-start ()
      (my-chip8-write-to-fifo "set pause no\n"))

    (defun my-chip8-beep-stop ()
      (my-chip8-write-to-fifo "set pause yes\n"))

    (defvar my-chip8-mpv-process nil)

    (defun my-chip8-start-mpv ()
      (setq my-chip8-mpv-process
            (start-process "mpv" "*mpv*" "mpv"
                           "--loop" "--pause" "--input-file"
                           my-chip8-mpv-fifo my-chip8-beep-file)))

    (defun my-chip8-mode-hook ()
      (when my-chip8-mpv-process
        (stop-process my-chip8-mpv-process))
      (my-chip8-make-fifo)
      (my-chip8-start-mpv))

    (add-hook 'chip8-mode-hook 'my-chip8-mode-hook)

    (setq chip8-beep-start-function 'my-chip8-beep-start)
    (setq chip8-beep-stop-function 'my-chip8-beep-stop)

Debugging
---------

Customize ``chip8-debug`` to enable logging of currently executed
commands to a ``*CHIP-8 debug*`` buffer.

Contributing
------------

See `CONTRIBUTING.rst
<https://github.com/wasamasa/chip8.el/blob/master/CONTRIBUTING.rst>`_.
