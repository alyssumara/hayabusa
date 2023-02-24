# hayabusa

## Installation

1. Install Emacs 28 and above versions
2. Clone or download this repository (path of the folder is the `<path-to-hayabusa>` used below).
3. Add following code in your ~/.emacs.d/init.el:

```elisp
(add-to-list 'load-path "<path-to-hayabusa>")

(require 'hayabusa)
(hayabusa-mode-enable)

(setq haybusa-keys-alist
'(("j" . "-")
("J" . "_")))
```

