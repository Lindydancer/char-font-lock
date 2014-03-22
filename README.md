# char-font-lock - Highlight bad whitespace and out-of-place characters

*Author:* Anders Lindgren<br>
*Version:* 0.0.0<br>
*URL:* [https://github.com/Lindydancer/char-font-lock](https://github.com/Lindydancer/char-font-lock)<br>


*Char Font Lock* is an Emacs package that highlight bad whitespace
and out-of-place characters.

Char Font Lock is implemented as two minor modes:
`char-font-lock-mode`, which can be applied to an individual
buffer, and `char-font-lock-global-mode` which automatically
enables the mode in all existing and future buffers.

## What is highlighted

Currently, the following are highlighted:

* Correct tab characters are highlighted using a neutral color, to
  indicate that they are there
* Incorrect tabs, i.e. tabs not first on the line or any tab in
  case `indent-tabs-mode` isn't active
* Empty lines at the end of the buffer
* The last line, if it ends without a newline
* End of line whitespace. (Technically, this is not highlighted using
  a font-lock keyword, instead the built-in feature
  `show-trailing-whitespace` is used.)

## Usage

Place the source file in a directory in the load path. Add the
following lines to an appropriate init file:

       (require 'char-font-lock)

Activate this package by Customize, or by placing the following line
into the appropriate init file:

       (char-font-lock-global-mode 1)

This package use Font Lock mode, so `font-lock-mode` or
`global-font-lock-mode` must be enabled (which it is by default).

### Customization

The following variables can be modified to fine-tune Char Font Lock:

* `char-font-lock-modes` -- For major modes that are members of
this list, or are derived from members in this list, Char Font Lock
Global mode will be enabled. As `prog-mode` and `text-mode`
initially are members, this mean that Char Font Lock is enabled for
most major modes, with the exception of special major modes like
`help-mode`. Some ancient third-party major modes are not derived
from the base modes provided by Emacs, in this case you can
explicitly add them to this list (alternatively, politely ask their
authors to take a step into the modern world).
* `char-font-lock-enabled-features-list` -- Control which highlight
rules should be applied to a major mode. For example, with the
default settings, end-of-file whitespace for
`lisp-interaction-mode` (used by the scratch buffer) is not
highlighted.

## Example

Below is a screenshot of a sample file, demonstrating the effect of
this package:

![See doc/demo.png for screenshot of Char Font Lock mode](doc/demo.png)



---
Converted from `char-font-lock.el` by [*el2markup*](https://github.com/Lindydancer/el2markdown).
