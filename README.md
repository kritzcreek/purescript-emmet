purescript-emmet
==

A tool for [emmet-like](https://docs.emmet.io/abbreviations/) html abbreviations
and generators in PureScript (Right now it only does Halogen, but it's easy to
write a renderer for all kinds of HTML DSLs).

<img src="http://i.imgur.com/xs7NZl1.gif" width="500px"></img>

## Installing

`npm i -g purescript-emmet`

## Usage

`purescript-emmet` expects an abbreviation on stdin and outputs generated
halogen markup on stdout. As an example of how to use this in Emacs, add this to
your `init.el`:

```emacs-lisp
(defun purescript-emmet ()
  (interactive)
  (let ((start (point))
        (end (save-excursion (beginning-of-line-text) (point))))
    (call-process-region start end "purescript-emmet" t t)))
(global-set-key (kbd "C-c C-e") 'purescript-emmet)
```

(Choose whatever keybinding you prefer)

## Usage as a Library

Check `Emmet.Halogen.emmetHalogen` to see an example usage of the library and
how you'd go about writing your own renderer.
LICENSE

## LICENSE

Copyright 2017 Christoph Hegemann

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

See the LICENSE file for further details.
