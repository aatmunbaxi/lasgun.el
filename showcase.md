`lasgun.el` By Examples
=======================
Here we describe some example uses of the `lasgun.el` to give users an idea of how one could adapt it to their use case.

Recall that a flow for `lasgun.el` is:

-   Mark several points in the buffer with  `lasgun-mark-*` functions
-   Perform action on each buffer position with `lasgun-action-*`

By default, `lasgun.el` comes with 2 actions defined: `lasgun-embark-act-all` and `lasgun-make-multiple-cursors`, which depend on `embark` and `multiple-cursors`, respectively.
It is up to the user to define other actions beyond this, which we describe a process to do here.


<a id="orgf18b59d"></a>

# Structure of a `lasgun` action

Like `avy`, a `lasgun` action visits the point(s) you have marked in the buffer, performs whatever function you want at that position, then restores your point to its original position. See [Avy Can Do Anything](https://karthinks.com/software/avy-can-do-anything/) for a comprehensive overview of defining these actions.

The yoga of `lasgun` actions is largely the same, with actions performed at each position newest to oldest, in order of how they were added.


<a id="orgf8d7b6c"></a>

# Simple actions with `define-lasgun-action`

If we have an interactive function `foo` that we want to perform at each position with no other bells and whistles, the macro `define-lasgun-action` should suffice.
For these types of functions, think stuff like `kill-*`, `upcase-*/downcase-*` where `*` could be one of `word`, `char`, `sexp`, `line` etc.

The behavior of these actions is self-explanatory: it just calls said function `foo` at each position, retaining the position of your `point`

There another thing you must specify to define an action via this macro: to persist the `lasgun-mark-ring` or not by default.
**By "persist" we mean retaining of marked positions in `lasgun-mark-ring` after the action is performed.**
This can be useful if you want to compose several simple actions together.
More on this later (spoiler: you can reverse this behavior interactively).

Some example uses are below

    ;; (define-lasgun-action NAME PERSIST FUN &optional FUN-ARGS)
    (define-lasgun-action lasgun-action-kill-word nil kill-word)
    (define-lasgun-action lasgun-action-kill-sexp nil kill-sexp)
    (define-lasgun-action lasgun-action-kill-whole-line nil kill-sexp)
    
    (define-lasgun-action lasgun-action-upcase-word t upcase-word)
    
    (define-lasgun-action lasgun-action-comment-line nil comment-line)

Note that persistence makes little sense on actions that kill text, since the marked positions would point nowhere important after text is killed.


https://github.com/aatmunbaxi/lasgun.el/assets/130934815/a87fe7b9-a522-4a2d-a9f7-7e67429ba84b



<a id="org1ad2936"></a>

# Spell Correction

The previous actions might not see much use; here's one I use often: spell correction with `jinx`.
This might seem like a retread of `jinx-correct-all`, but notice that we can target specific words to correct instead of being dropped into an interactive checker for the whole buffer.
Jinx *does* let you restrict your checking to a specific region (see `jinx-correct` docstring), but it requires you to activate a region beforehand, which I find can interrupt my train of thought.



https://github.com/aatmunbaxi/lasgun.el/assets/130934815/b15e7b2c-bfdf-434a-93ba-503f1d1a0d48


<a id="orgc1dfa42"></a>

## Toggling LaTeX Math Delimiters

Sometimes when editing LaTeX/Org documents with math typesetting, I realize that a particular math snippet I wrote in inline math (respectively, display math) would work better if written in display math (respectively, inline math).
At the same time, I use the [math-delimiters](https://github.com/oantolin/math-delimiters) package as a simple API to insert and manipulate math delimiters (overkill to have a whole package, I know).
In the package is a function that can toggle between inline math and display math delimiters when your point is immediately after the closing delimiter.
This can hook into `lasgun` easily:

    (defun toggle-math-delims ()
      (interactive)
      (forward-latex-math)
      (math-delimiters-insert))
    
    (define-lasgun-action lasgun-action-toggle-math-delims nil toggle-math-delims)

Here, the `forward-latex-delims` function is defined in [this](https://tex.stackexchange.com/a/52798) TeX stack exchange answer.



https://github.com/aatmunbaxi/lasgun.el/assets/130934815/701bc1bc-2919-46a6-9817-62f08d14d685


<a id="org6576e63"></a>

# More Complex Actions


<a id="orgbfab779"></a>

## Teleportation and Copying

"Teleportation" in avy-speak refers to killing a faraway sexp and yanking it to the current point.
With multiple selections, it's not sensible to yank them all at the current point without some processing, lest we end up with garbled compound words.
For this example, we our teleportation will yank the killed sexps from the `kill-ring` with a separator which the user is prompted for.

    (defun lasgun-action-teleport-sexps (ARG)
      "Kill sexps at lasgun selections and place them at point, with separator."
      (interactive "p")
      (let ((size (ring-length lasgun-mark-ring))
            (lasgun-list (ring-elements lasgun-mark-ring)))
            (save-excursion
              (dolist (pos lasgun-list)
                (goto-char pos)
                (backward-sexp)
                (kill-sexp ARG)))
              ;; killed sexps now in `kill-ring'
            (let ((separator (read-from-minibuffer "Separator: " nil nil nil nil " ")))
              (dotimes (i size)
                (insert (substring-no-properties (nth i kill-ring)))
                (unless (eq i (1- size))
                  (insert separator))))))

You'll notice that we can make the function support prefix arguments.

In a similar vein, we can choose not to kill the text and just copy it to the current point with a separator:

    (defun lasgun-action-copy-separated-sexps (ARG)
      "Kill words at lasgun selections and place them at point."
      (interactive "p")
      (let ((size (ring-length lasgun-mark-ring))
            (lasgun-list (ring-elements lasgun-mark-ring)))
        (save-excursion
          (dolist (pos lasgun-list)
            (let ((end nil)))
            (goto-char pos)
            (forward-sexp ARG)
            (setq end (point))
            (kill-new (buffer-substring pos end))))
        ;;  sexps now in `kill-ring'
        (let ((separator (read-from-minibuffer "Separator: " nil nil nil nil " ")))
          (dotimes (i size)
            (insert (substring-no-properties (nth i kill-ring)))
            (unless (eq i (1- size))
              (insert separator))))))

Hopefully with these two function a pattern emerges for writing `lasgun` actions.
Roughly, you just need to loop through the `lasgun-mark-ring`, visit each position, do whatever it is you want to do at that position, then clear up the ring if needed.


https://github.com/aatmunbaxi/lasgun.el/assets/130934815/6b1539c7-f47a-4069-b983-6639ba7a213b



<a id="org3650fa7"></a>

# A Compromise for the Indecisive

It's unreasonable to expect the user to define a `lasgun` action for *every possible action* they might want to use, especially if certain actions see less frequent usage.
We can write a function to prompt the user for the name of a function that they want to run which hasn't been bound to a function, which can work very simple functions if we're willing to give up support for numeric and function arguments.

    (defun lasgun-prompt-action ()
      (interactive)
      (let ((command (read-from-minibuffer "Command: ")))
        (unwind-protect
            (save-excursion
              (dolist (pos (ring-elements lasgun-mark-ring))
                (goto-char pos)
                (call-interactively (intern command) t)))
          (user-error "%s" "Error running command")
          (lasgun-clear-lasgun-mark-ring))))

This way, you can use a function at any time so long as you don't need arguments and the like.
I foresee more creative hackers seeing methods to improve this substantially.


https://github.com/aatmunbaxi/lasgun.el/assets/130934815/87ef6ed7-5f16-4e71-85c6-7e98eee84310


<a id="org5b82edf"></a>

# A Note on Persistence

Persistence of the `lasgun` mark ring might not be a feature the user wishes to change very often.
Support is provided by default for those actions defined by `define-lasgun-action`, and supporting Such a feature in your own functions is up to you to include.
This can be changed globally (see `lasgun-persist-lasgun-mark-ring`), or on-the-fly using a user-customizable numeric prefix argument (see `lasgun-persist-negation-prefix-arg`).
The interactive behavior is preferred over the globally defined variables.

Consider the following configuration:

    (setq lasgun-persist-lasgun-mark-ring nil)
    (define-lasgun-action lasgun-action-upcase-word t kill-word)

The global behavior is to not persist the mark ring after acting, but the function `lasgun-action-upcase-word` locally asks to persist the mark ring, so it will persist.
If the user wants the `lasgun-mark-ring` cleared, but only realizes this right before calling `lasgun-action-upcase-word` (say, to get right into marking other positions), they may call `lasgun-action-upcase-word` with numeric prefix equal to `lasgun-persist-negation-prefix-arg` (default: `0`).

For inspiration on how to achieve this behavior in your own `lasgun-actions`, see the source of `define-lasgun-action` and `lasgun--safe-clear-mark-ring`.

