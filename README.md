# Shen for Emacs Lisp

[Shen](https://shenlanguage.org) running natively in GNU Emacs (29.1+).
It currently tracks kernel **43.6**.

The KLambda kernel is compiled ahead of time into plain Emacs Lisp `defun`s.
That code is then byte-compiled and, where Emacs supports it, native-compiled.
There is no interpreter or trampoline at run time.

- Shen function `f` becomes `shen/f`.
- Shen global `*g*` becomes the variable `shen/*g*`.
- Self tail calls compile to `while` loops.
- Hot primitives are open-coded.

## Layout

| Path | Contents |
|---|---|
| `kernel/` | Git submodule: [Shen-Language/shen-sources](https://github.com/Shen-Language/shen-sources) @ `shen-43.6` |
| `klambda/` | KL generated from `kernel/` (`make klambda SHEN=...`); `klambda/SOURCE` records the commit |
| `src/shen-elisp-compiler.el` | KL reader and KL → Emacs Lisp compiler |
| `src/shen-elisp-primitives.el` | KL primitives and runtime |
| `src/shen-elisp-overwrite.el` | Native replacements for hot kernel functions |
| `src/shen-elisp.el` | Boot, launcher, Emacs commands |
| `compiled/` | Generated kernel `.el`/`.elc` (build output) |
| `bin/shen` | Command-line launcher |

## Building

```sh
git clone --recurse-submodules <repo> shen-elisp
cd shen-el
make            # klambda -> compiled/*.el, then byte + native compile
make test       # kernel test suite (kernel/tests/runme.shen)
```

`SHEN_ELISP_NATIVE=0 make` skips native compilation. The build needs only
Emacs. Regenerating `klambda/` from the submodule needs some working Shen:

```sh
make klambda SHEN=/path/to/shen
```

## Loading it with use-package

```elisp
(use-package shen-el
  :load-path "~/src/shen-el/src"
  :commands (shen-repl shen-elisp-eval shen-elisp-eval-region shen-elisp-load-file)
  :custom
  (shen-elisp-max-lisp-eval-depth 200000))
```

Run `make` first, so that `compiled/` exists. The kernel loads the first
time you use a command (`shen-elisp-boot`); this takes about 0.2 s.

## Commands and interaction

| Command | What it does |
|---|---|
| `M-x shen-repl` | Shen REPL in a comint buffer (runs `bin/shen repl`) |
| `M-x shen-elisp-eval-region` | Evaluate the region in this Emacs; result in the echo area |
| `M-x shen-elisp-load-file` | `load` a `.shen` file into this Emacs |
| `(shen-elisp-eval "(+ 1 2)")` | Evaluate a string from Lisp; returns the Shen value (here `3`) |

Shen values are ordinary Lisp data: lists are lists, symbols are symbols,
and `true`/`false` are the symbols `true` and `false`. That means you can
call Shen from Lisp directly:

```elisp
(shen-elisp-eval "(define double X -> (* 2 X))")
(shen/double 21)                       ;=> 42
(shen/map (shen/fn 'double) '(1 2 3))  ;=> (2 4 6)
```

From Shen, `(elisp.F A B)` calls the Emacs Lisp function `F`, and
`(elisp. "(form)")` embeds a literal Emacs Lisp form:

```shen
(elisp.message "hello from %s" "Shen")
(elisp.buffer-name)
```

Inside Emacs, `output` and `print` go to `standard-output` (the echo area or
`*Messages*`). Use `shen-repl` when you want a terminal-style session.

## Command line

```sh
bin/shen-el                        # REPL
bin/shen-el eval -e '(+ 1 2)'
bin/shen-el script file.shen
bin/shen-el --help
```

Leave the REPL with `(shen-elisp.exit)`; `(shen-elisp.exit N)` sets the exit
status.

## License

BSD 3-Clause. The kernel sources and generated code are © Mark Tarver; see
`kernel/LICENSE.txt`.
