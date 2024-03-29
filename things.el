;;; things.el --- Extensions to thingatpt            -*- lexical-binding: t; -*-

;; Author: Fox Kiester <noct@posteo.net>
;; URL: https://github.com/noctuid/things.el
;; Created: September 8, 2018
;; Keywords: convenience, text-object
;; Package-Requires: ((cl-lib "0.5") (emacs "24.4") (mmt "0.2.0") (avy "0.4.0"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package is an extension of thingatpt with support for the following:
;; - composite things
;; - nestable things
;; - seeking when no thing at point
;; - getting next/previous thing
;; - getting thing bounds with overlays (optional dependency on avy)
;; - bounds/region extension/expansion
;; - thing bounds adjustment (e.g. to get the bounds of an "inner list")

;; The goal is to provide a library that makes implementing things with this
;; functionality as simple as possible. Furthermore, things implemented by this
;; library should be usable not only by evil but by any package (there is no
;; evil dependency).

;; For more information see the README in the repository.

;;; Code:
(require 'cl-lib)
(require 'thingatpt)
(require 'mmt)
;; avy must be loaded at compile time so `avy-do-windows' can be expanded avy is
;; not necessary when loading/evaling until `things-remote-bounds' is called
(cl-eval-when (compile)
  (require 'avy))

(defgroup things nil
  "Provides extensions to thingatpt."
  :group 'convenience
  :prefix "things-")

;; * General Helpers
(defun things--altered-thing-p (thing)
  "Return whether THING is an altered thing.
Altered things are things with adjustments and/or contraints (e.g. '(paren
:adjustment inner)). Return nil if THING is a list of things or a thing without
any alterations. Specifically, check if THING is a list containing any
keywords."
  (and (listp thing)
       (cl-some #'keywordp thing)))

;; TODO make part of API
(defun things--base-thing (thing)
  "Return the base thing in THING.
If THING is an altered thing, discard the alterations and just return the
thing (e.g. '(paren :adjustment inner ...) -> 'paren)."
  (if (things--altered-thing-p thing)
      (car thing)
    thing))

;; TODO make part of API?
(defun things--alteration (thing alteration)
  "Return THING's value for ALTERATION if it exists."
  (when (things--altered-thing-p thing)
    (plist-get (cdr thing) alteration)))

(defun things--get (thing prop)
  "Call `get' with the base thing in THING and PROP.
See `things--base-thing' also."
  (get (things--base-thing thing) prop))

(defun things--make-things-list (things)
  "Return THINGS as a list.
If THINGS is already a list of things, return it as-is. If it is a single
thing (with or without an adjustment), return a new list with it as the only
item."
  (if (and (listp things)
           (not (things--altered-thing-p things)))
      things
    (list things)))

;; TODO this isn't sufficient if not using symbol plist
(defun things-clone-thing (new-thing old-thing)
  "Create NEW-THING as a copy of OLD-THING.
This just sets the symbol plist of NEW-THING to the symbol plist of OLD-THING.
This is similar to `defalias', but the intent is that NEW-THING be further
modified after calling this."
  (setplist new-thing (symbol-plist old-thing)))

(defun things--thing/bounds-p (val)
  "Return whether VAL is in the form (thing . (beg . end))."
  (and (consp val)
       (symbolp (car val))
       (consp (cdr val))
       (numberp (cadr val))
       (numberp (cddr val))))

;; * Motion Helpers
;; TODO better name?
(defmacro things-return-point-if-changed (&rest body)
  "Run the forms BODY and return the `point' if it has changed."
  (declare (indent 0))
  (let ((orig-pos (cl-gensym))
        (final-pos (cl-gensym)))
    `(let ((,orig-pos (point)))
       ,@body
       (let ((,final-pos (point)))
         (unless (= ,final-pos ,orig-pos)
           ,final-pos)))))

(cl-defmacro things--run-op-or (thing (op-sym &rest args) &rest body)
  "If THING has an OP-SYM property, run it with ARGS.
Otherwise run BODY."
  (declare (indent 2))
  (let ((op (cl-gensym)))
    `(let ((,op (things--get ,thing ,op-sym)))
       (if ,op
           (apply ,op (list ,@args))
         ,@body))))

(cl-defmacro things--run-motion-op-or (thing (op-sym &rest args) &rest body)
  "If THING has an OP-SYM property, run it with ARGS.
Otherwise run BODY. If the point moves, return the new position. Otherwise
return nil."
  (declare (indent 2))
  `(things-return-point-if-changed
     (things--run-op-or ,thing (,op-sym ,@args)
       ,@body)))

;; TODO look at `evil-motion-loop'
;; TODO at some point may need to know the number of succesful iterations, so
;; consider returning that instead of the point (could, for example, build
;; another function on top of that fails if not able to move exactly count
;; times)
(defmacro things-move-with-count (count &rest body)
  "While COUNT is positive, run BODY.
COUNT defaults to 1 if nil. If the point does not change after an iteration of
body, stop there and leave the point as-is. This means that the BODY should move
the point on success and keep the point as-is on failure. If the point moves at
least once, return the new point. Otherwise return nil."
  (declare (indent 1) (debug t))
  (let ((start-pos (cl-gensym))
        (_ (cl-gensym)))
    `(things-return-point-if-changed
       (cl-dotimes (,_ (or ,count 1))
         (let ((,start-pos (point)))
           ,@body
           (when (= ,start-pos (point))
             (cl-return)))))))

;; TODO use more? name better?
(defmacro things--reset-pos-when-nil (&rest body)
  (declare (indent 0) (debug t))
  (let ((orig-pos (cl-gensym))
        (ret (cl-gensym)))
    `(let* ((,orig-pos (point))
            (,ret (progn ,@body)))
       (if ,ret
           ,ret
         (goto-char ,orig-pos)
         nil))))

(defmacro things-move-while-not (predicate &rest body)
  "Run BODY until PREDICATE is met or BODY doesn't move the point.
When PREDICATE returns non-nil for a position, return that position. Otherwise
return nil and don't move the point."
  (declare (indent 1) (debug t))
  (let ((success (cl-gensym)))
    `(things--reset-pos-when-nil
       (let (,success)
         (while (and (things-return-point-if-changed
                       ,@body)
                     ;; need to know if predicate ever succesful
                     (not (setq ,success ,predicate))))
         ,success))))

;; * Constraint Helpers
(defvar things--narrowed-bounds nil
  "The bouns of the current narrowing.")

(defmacro things--with-narrowing (bounds &rest body)
  "Narrow buffer to BOUNDS while running BODY."
  (declare (indent 1) (debug t))
  `(save-restriction
     (save-excursion
       (narrow-to-region (car ,bounds) (cdr ,bounds))
       (let ((things--narrowed-bounds ,bounds))
         ,@body))))

;; ** :constraint
(defmacro things-with-constraint (things require &rest body)
  "When inside THINGS, run BODY with the buffer narrowed to the bounds.
The point is not consider to be inside a thing if it is on the edge of it. If
THINGS is nil, run BODY without narrowing. If REQUIRE is non-nil, do nothing if
there are no THINGS at the point. Otherwise run BODY without narrowing if no
THINGS at the point. If REQUIRE is nil and BODY returns nil, try running BODY
again without narrowing."
  (declare (indent 2) (debug t))
  (let ((bounds (cl-gensym)))
    `(if (null ,things)
         ;; so can use unconditionally even if no constraining things
         (progn ,@body)
       (let ((,bounds (cdr (things-bounds ,things))))
         (if (and ,bounds
                  ;; TODO are there any situations where the boundary
                  ;; of a thing should still be considered inside it?
                  ;; if so, add a new property that the thing
                  ;; definition should set
                  (things--point-inside-p (point) ,bounds))
             (or (things--with-narrowing ,bounds
                   ,@body)
                 (unless ,require
                   ,@body))
           (unless ,require
             ,@body))))))

;; ** :ignore
;; TODO this is an initial naive/awful implementation for simplicity sake; map
;; points in a temp buffer that has ignored things removed to point in original
;; buffer
;; TODO only set this text property for necessary positions or use a different
;; solution using markers (e.g. could record where text is deleted); text
;; properties are very much not ideal (e.g. special handling for point max)
(defun things--orig-buffer-pos (&optional pos)
  "Return the value for 'things-orig-buffer-pos at POS or the point.
If there is no property for 'things-orig-buffer-pos at POS, return POS."
  (unless pos
    (setq pos (point)))
  (if (= pos (point-max))
      ;; TODO this will fail if point-max = point-min
      (1+ (things--orig-buffer-pos (1- pos)))
    (or (get-text-property pos 'things-orig-buffer-pos)
        pos)))

(defun things--offset-bounds (bounds)
  "Return BOUNDS adjusted to match positions in original buffer.
This function will not alter the original BOUNDS. BOUNDS can either be in the
form (beg . end) or in the form (thing . (beg . end)). See also
`things--orig-buffer-pos'. If BOUNDS is nil, return nil."
  (when bounds
    (let* ((thing/bounds-p (things--thing/bounds-p bounds))
           (thing (when thing/bounds-p
                    (car bounds)))
           (new-bounds (cl-copy-list
                        (if thing/bounds-p
                            (cdr bounds)
                          bounds))))
      (when new-bounds
        (setf (car new-bounds) (things--orig-buffer-pos (car new-bounds)))
        (setf (cdr new-bounds) (things--orig-buffer-pos (cdr new-bounds))))
      (if thing/bounds-p
          (cons thing new-bounds)
        new-bounds))))

(defun things--point-buffer (&optional point-min-offset)
  "Set 'things-orig-buffer-text-property for every position in the buffer.
This only works in a temporary buffer copy before making any changes. If
POINT-MIN-OFFSET is non-nil, start with 1+ that number instead of 1."
  (save-excursion
    (goto-char (point-min))
    (while (not (= (point) (point-max)))
      (put-text-property (point) (1+ (point))
                         'things-orig-buffer-pos
                         (+ (point) point-min-offset))
      (forward-char))))

(defmacro things--with-temp-buffer-copy (&rest body)
  "Run BODY in a temporary buffer that is a copy of the current one.
Prior to running body, set the 'things-orig-buffer-pos text property for each
position in the temporary buffer."
  (declare (indent 0))
  (let ((old-buffer (cl-gensym))
        (old-mode (cl-gensym))
        (old-point (cl-gensym))
        (point-min-offset (cl-gensym)))
    `(let ((,old-buffer (current-buffer))
           (,old-mode major-mode)
           ;; handle narrowing
           (,old-point (point))
           (,point-min-offset (1- (point-min))))
       (with-temp-buffer
         (insert-buffer ,old-buffer)
         ;; TODO will this always work?
         (funcall ,old-mode)
         (things--point-buffer ,point-min-offset)
         (goto-char (- ,old-point ,point-min-offset))
         ,@body))))

(defun things--remove-things (things &optional keep-if-current)
  "Remove the text for all THINGS in the current buffer.
If KEEP-IF-CURRENT is non-nil, don't delete the a thing that the point is
inside."
  (save-excursion
    ;; TODO could maybe use a marker and ignore the positions in the original
    ;; buffer
    (let ((orig-pos (things--orig-buffer-pos (point)))
          thing/bounds)
      (goto-char (point-min))
      (while (and (setq bounds (cdr (things-seeking-bounds things)))
                  (if (and keep-if-current
                           ;; can't just check if inside thing because may be
                           ;; inside a thing with an unmatched paren, for
                           ;; example; in that case would want to try again
                           ;; without narrowing and remove string with unmatched
                           ;; paren even though the point is in it
                           things--narrowed-bounds
                           (equal (things--offset-bounds bounds)
                                  things--narrowed-bounds))
                      (things-seek-forward things 1 nil)
                    (delete-region (car bounds) (cdr bounds))
                    t))))))

;; TODO this will be unusably slow without a :constraint
;; potential for faster alternative (so don't have to go through entire buffers)
;; - get bounds of thing without removing ignored things
;; - check if any ignored things inside or at edges of bounds and remove them
;;   and try again until no things inside
(defmacro things-with-removed-things (things keep-if-current &rest body)
  "With a temporary buffer that has THINGS removed, run BODY.
If BODY returns a bounds cons (beg . end), adjust the bounds to match those in
the original buffer. If THINGS is nil, just run BODY. If KEEP-IF-CURRENT is
non-nil, don't remove any THINGS that the point is inside of (if the point is at
the boundary of a thing, it is not considered inside it)."
  (declare (indent 2) (debug t))
  (let ((bounds (cl-gensym))
        (non-things (cl-gensym)))
    `(if (null ,things)
         ;; so can use unconditionally even if no constraining things
         (progn ,@body)
       (things--with-temp-buffer-copy
         (things--remove-things ,things ,keep-if-current)
         (things--offset-bounds ,@body)))))

;; * Default Thingatpt Compatability Layer
(defun things-base-bounds (thing)
  "Call `bounds-of-thing-at-point' with THING.
This will ignore :adjustment alterations but will handle constraint
alterations."
  (let ((outer (things--alteration thing :constraint))
        (outer-optional (things--alteration thing :optional-constraint))
        (inner (things--alteration thing :ignore)))
    ;; TODO better names; :fence?
    ;; TODO just use :predicate for "always" ignore functionality
    ;; nesting `things-with-removed-things' won't really work
    (things-with-constraint outer t
      (things-with-constraint outer-optional nil
        (things-with-removed-things inner t
          (things--run-op-or thing ('things-bounds-of-thing-at-point)
            (bounds-of-thing-at-point (things--base-thing thing))))))))

(defun things-forward (thing &optional count)
  "Move to the next THING end COUNT times.
With a negative COUNT, move to the previous THING beginning COUNT times. Unlike
`forward-thing', this function has a well-defined behavior on failure. If able
to move at least once, return the new position. Otherwise return nil."
  (unless count
    (setq count 1))
  (things-move-with-count count
    (things-move-while-not
        (let ((bounds
               (save-excursion
                 ;; may have moved to a point in between two things, so move
                 ;; backwards one character
                 ;; TODO ensure this is sufficient or repurpose
                 ;; `things--after-seek-bounds' to be usable here
                 (unless (or (cl-minusp count)
                             (= (point) (point-min)))
                   (backward-char))
                 (things-base-bounds thing))))
          (and bounds
               (= (point) (if (cl-plusp count)
                              (cdr bounds)
                            (car bounds)))))
      (forward-thing (things--base-thing thing)))))

(defun things-backward (thing &optional count)
  "Move to the previous THING beginning COUNT times.
With a negative COUNT, move to the next THING end COUNT times. Unlike
`forward-thing', this function has a well-defined behavior on failure. If able
to move at least once, return the new position. Otherwise return nil."
  (unless count
    (setq count 1))
  (things-forward thing (- count)))

;; * Seeking
;; TODO remove this entirely?
(defun things-bound (&optional backward)
  "Return the bound to be used when seeking forward or backward.
BACKWARD specifies that the bound should be for seeking backward. This function
is used by bounds functions that use seeking. This default function restricts
seeking to between the beginning and end of the buffer. A custom function can be
used by changing the `things-bound' variable."
  (if backward
      (point-min)
    (point-max)))

(defun things--seeks-to-end-p (thing &optional backward)
  "Return whether seeking forward for THING will go to the thing end.
If BACKWARD is non-nil, return whether seeking backward for THING goes to the
thing end."
  (if backward
      (things--get thing 'things-seeks-backward-end)
    (not (things--get thing 'things-seeks-forward-begin))))

(defun things--move-back (thing &optional backward)
  "Move backward if seeking for THING moved to the thing end.
BACKWARD and THING are passed to `things--seeks-to-end-p' to determine this."
  (when (things--seeks-to-end-p thing backward)
    (backward-char)))

(defun things--after-seek-bounds (thing &optional backward)
  "Return the bounds of the THING at point.
This function is meant to be called after successfully seeking. Since, for
example, moving to the end of a thing may either move the point beyond a thing
or to the border of two things, `bounds-of-thing-at-point' cannot always be used
directly. If at no thing or at two things (e.g. at the end of a one character
thing like the evil-word \"-\" in \"word-|word\" or a the end of a list
\")|)\"), and seeking moved the point to the end of THING (as determined by
`things--seeks-to-end-p' called with THING and BACKWARD), return the bounds
corresponding to the thing that the point is at the end of."
  (let* ((bounds (things-base-bounds thing))
         (end (things--seeks-to-end-p thing backward))
         (bounds-at-previous-char
          (when (and end
                     (or
                      ;; (maybe) just after bounds
                      (not bounds)
                      ;; e.g. foo|-bar
                      (= (point) (car bounds))
                      ;; e.g. )|)
                      (= (point) (cdr bounds))))
            (save-excursion
              ;; shouldn't be at point-min, but this function shouldn't fail
              (unless (= (point) (point-min))
                (backward-char))
              (let ((bounds (things-base-bounds thing)))
                ;; e.g. should not use previous bounds for (|(; this can only
                ;; happen when things-seeks-forward-begin is not set correctly
                (when (and bounds (or (= (point) (cdr bounds))
                                      ;; e.g. list thing
                                      (= (1+ (point)) (cdr bounds))))
                  bounds))))))
    (or bounds-at-previous-char bounds)))

(defun things--try-seek (thing &optional count)
  "Call THING's things-seek-op or `things-forward' with COUNT.
Return the new position if able to seek at least once. Otherwise return nil."
  (unless count
    (setq count 1))
  (things--run-motion-op-or thing ('things-seek-op thing count)
    (things-forward thing count)))

(defun things--min (&rest numbers)
  "Same as `min' but remove nils from NUMBERS."
  (apply #'min (remove nil numbers)))

(cl-defun things--seek-forward (thing &optional count bound)
  "Seek forward to THING COUNT times.
Seek to the next thing if there is an existing thing at the point. If BOUND is
non-nil, do not seek beyond BOUND. If successful, move the point and return the
new position. Otherwise return nil."
  (or count (setq count 1))
  (when (cl-minusp count)
    (cl-return-from things--seek-forward
      (things--seek-backward thing count bound)))
  (setq bound (things--min bound (point-max)))
  (let ((orig-pos (point))
        (initial-bounds (things-base-bounds thing))
        seek-start-pos)
    ;; TODO can fail early here
    ;; go to the end of the current or next thing
    (things--try-seek thing)
    ;; using `things--after-seek-bounds' because may have moved to the end of a
    ;; thing that is also at the next thing (e.g. evil-word: "thing|-" or for
    ;; list thing ")|)")
    (if (equal (things--after-seek-bounds thing) initial-bounds)
        ;; haven't reached new thing yet
        (setq seek-start-pos (point))
      (setq seek-start-pos orig-pos)
      (cl-decf count))
    (cl-dotimes (_ count)
      (let ((pos (point)))
        (things--try-seek thing)
        (when (or (= (point) pos)
                  (> (point) bound))
          (goto-char pos)
          (cl-return))))
    (if (or (= (point) seek-start-pos)
            (> (point) bound))
        ;; failed to seek
        (goto-char orig-pos)
      (things--move-back thing))
    (unless (= (point) orig-pos)
      (point))))

(defun things--max (&rest numbers)
  "Same as `max' but remove nils from NUMBERS."
  (apply #'max (remove nil numbers)))

(cl-defun things--seek-backward (thing &optional count bound)
  "Seek backward to THING COUNT times.
Seek to the previous thing if there is an existing thing at the point. If BOUND
is non-nil, do not seek before BOUND. If successful, move the point and return
the new position. Otherwise return nil."
  (setq count (or count 1))
  (when (cl-minusp count)
    (cl-return-from things--seek-backward
      (things--seek-forward thing count bound)))
  (setq bound (things--max bound (point-min)))
  (let ((orig-pos (point))
        (initial-bounds (things-base-bounds thing))
        seek-start-pos)
    ;; go to the current or previous thing beginning
    (things--try-seek thing (- count))
    (if (equal (things--after-seek-bounds thing t) initial-bounds)
        (setq seek-start-pos (point))
      (setq seek-start-pos orig-pos)
      (cl-decf count))
    (cl-dotimes (_ count)
      (let ((pos (point)))
        (things--try-seek thing -1)
        (when (or (= (point) pos)
                  (< (point) bound))
          (goto-char pos)
          (cl-return))))
    (if (or (= (point) seek-start-pos)
            (< (point) bound))
        (goto-char orig-pos)
      (things--move-back thing t))
    (unless (= (point) orig-pos)
      (point))))

(defun things--distance (pos1 pos2)
  "Return the distance between POS1 and POS2."
  (abs (- pos1 pos2)))

;; TODO document that BOUND-FUNCTION can be nil (same for COUNT below); consider
;; going all keyword or all optional instead
(cl-defun things--seek (things bound-function &key
                               forward-only
                               backward-only
                               prefer-backward
                               prefer-closest)
  "Seek to a thing in THINGS.
See `things-seek' for more information on BOUND-FUNCTION, FORWARD-ONLY,
BACKWARD-ONLY, PREFER-BACKWARD, and PREFER-CLOSEST. The only difference is that
`things-seek' additionally supports a count."
  (setq things (things--make-things-list things))
  (unless bound-function
    (setq bound-function (lambda (&rest _))))
  (let* ((forward-positions
          (unless backward-only
            (remove nil
                    (mapcar (lambda (thing)
                              (save-excursion
                                (when (things--seek-forward
                                       thing 1 (funcall bound-function))
                                  (cons thing (point)))))
                            things))))
         (sorted-forward-positions (cl-sort forward-positions #'< :key #'cdr))
         (closest-forward-thing/pos (car sorted-forward-positions))

         (backward-positions
          (unless forward-only
            (remove nil
                    (mapcar (lambda (thing)
                              (save-excursion
                                (when (things--seek-backward
                                       thing 1 (funcall bound-function t))
                                  (cons thing (point)))))
                            things))))
         (sorted-backward-positions (cl-sort backward-positions #'> :key #'cdr))
         (closest-backward-thing/pos (car sorted-backward-positions))

         (goto-thing/pos
          (cond (forward-only
                 closest-forward-thing/pos)
                (backward-only
                 closest-backward-thing/pos)
                ((not (and closest-forward-thing/pos closest-backward-thing/pos))
                 (or closest-forward-thing/pos closest-backward-thing/pos))
                (prefer-closest
                 (let ((forward-distance
                        (things--distance (point)
                                          (cdr closest-forward-thing/pos)))
                       (backward-distance
                        (things--distance (point)
                                          (cdr closest-backward-thing/pos))))
                   (if (= forward-distance backward-distance)
                       (if prefer-backward
                           closest-backward-thing/pos
                         closest-forward-thing/pos)
                     (if (< forward-distance backward-distance)
                         closest-forward-thing/pos
                       closest-backward-thing/pos))))
                (prefer-backward
                 closest-backward-thing/pos)
                ;; explicit condition for clarity
                ((not prefer-backward)
                 closest-forward-thing/pos))))
    (when goto-thing/pos
      (goto-char (cdr goto-thing/pos))
      goto-thing/pos)))

(cl-defun things-seek (things count bound-function &key
                              forward-only
                              backward-only
                              prefer-backward
                              prefer-closest)
  "Seek to closest thing in THINGS COUNT times as bounded by BOUND-FUNCTION.

See `things-bound' for an example bound function.

If FORWARD-ONLY is non-nil, only attempt to seek forward. If BACKWARD-ONLY is
non-nil, only attempt to seek backward. When seeking in both directions, prefer
seeking forward if possible unless PREFER-BACKWARD is non-nil. When
PREFER-CLOSEST is non-nil prefer the closest position regardless of direction.
When the closest positions in both directions are the same distance from the
point, choose based on PREFER-BACKWARD.

If successful, move the point and return a cons of the form (thing . new
position). Otherwise return nil."
  (let (thing/pos)
    (cl-dotimes (_ count)
      ;; (apply #'things--seek things bound-function kargs)
      (let ((current-thing/pos (things--seek things bound-function
                                             :forward-only forward-only
                                             :backward-only backward-only
                                             :prefer-backward prefer-backward
                                             :prefer-closest prefer-closest)))
        (if current-thing/pos
            (setq thing/pos current-thing/pos)
          (cl-return))))
    thing/pos))

(defun things-seek-forward (&rest args)
  "Call `things-seek' with ARGS and \":forward-only t\"."
  (apply #'things-seek (append args (list :forward-only t))))

(defun things-seek-backward (&rest args)
  "Call `things-seek' with ARGS and \":backward-only t\"."
  (apply #'things-seek (append args (list :backward-only t))))

;; * Extra Motions
(defun things-alt-forward (thing &optional count)
  "Move to the next THING beginning COUNT times.
With a negative COUNT, move to the previous THING end COUNT times. If THING has
a things-alt-forward-op property, call it with COUNT. Otherwise use the default
implementation built on top of `things-forward'/`forward-thing'. If able to move
at least once, return the new position. Otherwise return nil."
  (unless count
    (setq count 1))
  (setq thing (things--base-thing thing))
  (things--run-motion-op-or thing ('things-alt-forward-op count)
    (when (things--seek-forward thing count)
      (let ((bounds (things-base-bounds thing)))
        ;; with properly implemented underlying functions, seeking should
        ;; always move the point to a thing
        (when bounds
          (goto-char (if (cl-plusp count)
                         (car bounds)
                       (cdr bounds))))))))

(defun things-alt-backward (thing &optional count)
  "Move to the previous THING end COUNT times.
With a negative COUNT, move to the next THING beginning COUNT times. If THING
has a things-alt-forward-op property, call it with an inverted COUNT. Otherwise
use the default implementation built on top of `things-forward'/`forward-thing'.
If able to move at least once, return the new position. Otherwise return nil."
  (unless count
    (setq count 1))
  (things-alt-forward thing (- count)))

(defun things-forward-begin (thing &optional count)
  "Move to the next THING beginning COUNT times.
With a negative COUNT, move to the previous THING beginning COUNT times. If able
to move at least once, return the new position. Otherwise return nil."
  (unless count
    (setq count 1))
  (if (cl-plusp count)
      (things-alt-forward thing count)
    (things-backward thing count)))

(defun things-backward-begin (thing &optional count)
  "Move to the previous THING beginning COUNT times.
With a negative COUNT, move to the next THING beginning COUNT times. If able to
move at least once, return the new position. Otherwise return nil."
  (unless count
    (setq count 1))
  (things-forward-begin thing (- count)))

(defun things-forward-end (thing &optional count)
  "Move to the next THING end COUNT times.
With a negative COUNT, move to the previous THING end COUNT times. If able to
move at least once, return the new position. Otherwise return nil."
  (unless count
    (setq count 1))
  (if (cl-plusp count)
      (things-forward thing count)
    (things-alt-forward thing count)))

(defun things-backward-end (thing &optional count)
  "Move to the previous THING end COUNT times.
With a negative COUNT, move to the next THING end COUNT times. If able to move
at least once, return the new position. Otherwise return nil."
  (unless count
    (setq count 1))
  (things-forward-end thing (- count)))

;; * Bounds Adjustment
(defun things-compose (&rest fns)
  "Return a function composed of FNS.
FNS will be called right to left."
  (let ((fn1 (car (last fns)))
        (fns (butlast fns)))
    (lambda (&rest args)
      (cl-reduce #'funcall fns
                 :from-end t
                 :initial-value (apply fn1 args)))))

(defun things--apply-adjustment (thing adjustment)
  "Return THING with a new ADJUSTMENT value."
  (let ((alterations (if (things--altered-thing-p thing)
                         (cl-copy-list (cdr thing))
                       (list))))
    (cons (things--base-thing thing)
          (plist-put alterations :adjustment adjustment))))

(defun things-apply-adjustment (things adjustment)
  "Return THINGS with new ADJUSTMENT values."
  (if (and (listp things)
           (not (things--altered-thing-p things)))
      (mapcar (lambda (thing)
                (things--apply-adjustment thing adjustment))
              things)
    (things--apply-adjustment things adjustment)))

(defun things-get-adjust-function (thing adjustment)
  "Return the function associated with THING to perform ADJUSTMENT.
If the thing does not have an adjustment function for the specified ADJUSTMENT,
return the default adjustment function if there is one."
  (let ((adjust-function
         (or (get thing (intern (format "things-get-%s" adjustment)))
             (intern (format "things--get-%s" adjustment)))))
    (when (functionp adjust-function)
      adjust-function)))

(defun things-shrink-by-1 (thing/bounds)
  "Shrink the bounds in THING/BOUNDS by 1 character on each side."
  (setq thing/bounds (cl-copy-list thing/bounds))
  (let ((bounds (cdr thing/bounds)))
    (cl-incf (car bounds))
    (cl-decf (cdr bounds))
    thing/bounds))

(defun things-shrink-by-space-then-newlines (thing/bounds)
  "Shrink the bounds in THING/BOUNDS to exclude spaces/tabs and then newlines.
Whitespace after newlines at the start of the bounds and whitespace before
newlines at the end of the bounds will not be excluded."
  (save-excursion
    (setq thing/bounds (cl-copy-list thing/bounds))
    (let ((bounds (cdr thing/bounds)))
      (goto-char (car bounds))
      (skip-chars-forward " \t")
      (skip-chars-forward "\n\r")
      (setf (car bounds) (point))
      (goto-char (cdr bounds))
      (skip-chars-backward " \t")
      (skip-chars-backward "\n\r")
      (setf (cdr bounds) (point))
      thing/bounds)))

(defun things-shrink-by-regexp (thing/bounds left right)
  "Shrink the bounds in THING/BOUNDS using the regexps LEFT and RIGHT.
When LEFT matches the start of the bounds, set the bounds start to the end of
the match. When RIGHT matches the end of the bounds (to or before beginning of
the bounds), set the bounds end to the beginning of the match. If LEFT or RIGHT
is nil, don't attempt to shrink on the corresponding side."
  (save-excursion
    (save-match-data
      (setq thing/bounds (cl-copy-list thing/bounds))
      (let ((bounds (cdr thing/bounds)))
        (when left
          (goto-char (car bounds))
          (when (looking-at left)
            (setf (car bounds) (match-end 0))))
        (when right
          (goto-char (cdr bounds))
          ;; TODO doesn't work with +; e.g. won't work for python docstring
          (when (looking-back right (car bounds))
            (setf (cdr bounds) (match-beginning 0))))
        thing/bounds))))

(defun things-shrink-by-newlines (thing/bounds)
  "Shrink the bounds in THING/BOUNDS by newlines on either side."
  (things-shrink-by-regexp thing/bounds "\n" "\n"))

(defun things-grow-by-space-one-side (thing/bounds)
  "Grow the bounds in THING/BOUNDS to include whitespace after or before it.
Whitespace after the bounds is prioritized."
  (save-excursion
    (setq thing/bounds (cl-copy-list thing/bounds))
    (let ((bounds (cdr thing/bounds)))
      (goto-char (cdr bounds))
      (cond ((= (skip-chars-forward " \t") 0)
             (goto-char (car bounds))
             (skip-chars-backward " \t")
             (setf (car bounds) (point)))
            (t
             (setf (cdr bounds) (point))))
      thing/bounds)))

(defun things--get-inside (thing/bounds)
  "Shrink THING/BOUNDS using the \"inner\" adjustment and then by whitespace.
If the thing has an inner adjustment, perform that first, and then shrink the
bounds further to exclude spaces/tabs then newlines."
  (let ((adjust-function (things-get-adjust-function
                          (things--base-thing (car thing/bounds))
                          'inner)))
    (when adjust-function
      (setq thing/bounds (funcall adjust-function thing/bounds))))
  (things-shrink-by-space-then-newlines thing/bounds))

(defalias 'things--get-a #'things-grow-by-space-one-side)

(defalias 'things--get-around #'things-grow-by-space-one-side)

(defun things--get-linewise (thing/bounds)
  "Grow the bounds in THING/BOUNDS to encompass only whole lines."
  (save-excursion
    (setq thing/bounds (cl-copy-list thing/bounds))
    (let ((bounds (cdr thing/bounds)))
      (goto-char (car bounds))
      (setf (car bounds) (line-beginning-position))
      (goto-char (cdr bounds))
      (setf (cdr bounds) (line-end-position))
      thing/bounds)))

(defun things--adjusted-bounds (thing/bounds)
  "Adjust and return THING/BOUNDS.
If the thing in THING/BOUNDS specifies an adjustment (e.g. '(comment :adjustment
inner)), use the thing's corresponding adjustment function to alter the bounds
in THING/BOUNDS. If the thing does not have a corresponding ADJUSTMENT function
defined, fall back to the default one if it exists. If there is no specified
adjustment or there is no available function for the specified adjustment, just
return THING/BOUNDS."
  (let* ((thing (car thing/bounds))
         (base-thing (things--base-thing thing))
         (adjustment (things--alteration thing :adjustment))
         (adjust-function
          (when adjustment
            (things-get-adjust-function base-thing adjustment))))
    (if adjust-function
        (funcall adjust-function thing/bounds)
      thing/bounds)))

;; * Bounds at Point
;; TODO make public?
(defun things--point-inside-p (pos bounds)
  "Return whether POS or the point is inside BOUNDS.
POS is considered to be inside BOUNDS if it is in between BOUNDS but not
exactly the beginning or end position."
  (unless pos
    (setq pos (point)))
  (< (car bounds) pos (cdr bounds)))

(defun things--bounds-inside-p (current-bounds bounds)
  "Return whether CURRENT-BOUNDS is inside and not exactly BOUNDS."
  (and (not (equal bounds current-bounds))
       (<= (car bounds) (car current-bounds))
       (>= (cdr bounds) (cdr current-bounds))))

(defun things--bounds-size (bounds)
  "Return the distance between the beginning and end of BOUNDS."
  (- (cdr bounds) (car bounds)))

(defun things--bounds-< (bounds1 bounds2)
  "Return whether the size of BOUNDS1 is less than BOUNDS2.
If they are equal, return whether BOUNDS1 starts sooner."
  (let ((size1 (things--bounds-size bounds1))
        (size2 (things--bounds-size bounds2)))
    (if (= size1 size2)
        (< (car bounds1) (car bounds2))
      (< size1 size2))))

(defun things--all-bounds (things)
  "Get the bounds of all the THINGS at the point.
Return a list of conses of the form (thing . bounds) or nil if unsuccessful."
  (remove nil (mapcar (lambda (thing)
                        (let ((bounds (things-base-bounds thing)))
                          (when bounds
                            (things--adjusted-bounds (cons thing bounds)))))
                      things)))

(defun things--all-base-bounds (things)
  "Get the base bounds for all the THINGS at the point.
Return a list of conses of the form (thing . bounds) or nil if unsuccessful."
  (remove nil (mapcar (lambda (thing)
                        (let ((bounds (things-base-bounds thing)))
                          (when bounds
                            (cons thing bounds))))
                      things)))

;; TODO (progn (foo)(ba|r)(baz))
;; how to get progn bounds? keep skipping forward or backward if start/end the same?
(defun things--all-outer-bounds (things/bounds)
  "Get the bounds for all things in THINGS/BOUNDS outside the current bounds.
For every existing thing/bounds, try the corresponding bound function just
before or after the existing bounds. Return a list of conses of the form (thing
. bounds) or nil if unsuccessful."
  (remove nil (mapcar (lambda (thing/bounds)
                        (let* ((thing (car thing/bounds))
                               (bounds (cdr thing/bounds))
                               (beg (car bounds))
                               (end (cdr bounds)))
                          (save-excursion
                            (if (= beg (point-min))
                                (goto-char (1+ end))
                              (goto-char (1- beg)))
                            (let ((bounds (things-base-bounds thing)))
                              (when bounds
                                (things--adjusted-bounds (cons thing bounds)))))))
                      things/bounds)))

(defun things--all-expanded-bounds (things current-bounds)
  "Get all bounds of THINGS that encompass CURRENT-BOUNDS.
Return a list of conses of the form (thing . bounds) or nil if unsuccesful."
  ;; if inside a nested list, for example, this function should not return the
  ;; same bounds as CURRENT-BOUNDS, so bounds are checked just before or after
  ;; the base bounds for all things; checking just before/ctfer CURRENT-BOUNDS
  ;; is not sufficient because the current bounds could be for an inner thing
  (let* ((things/bounds (things--all-bounds things))
         (base-things/bounds (things--all-base-bounds things))
         (outer-things/bounds (things--all-outer-bounds base-things/bounds))
         (all-things/bounds (cl-union things/bounds outer-things/bounds
                                      :test #'equal))
         (encompassing-things/bounds (cl-remove-if-not
                                      (apply-partially #'things--bounds-inside-p
                                                       current-bounds)
                                      all-things/bounds
                                      :key #'cdr)))
    encompassing-things/bounds))

(defun things--smallest-bounds (all-things/bounds)
  "Return the smallest thing/bounds in ALL-THINGS/BOUNDS.
If there are multiple smallest bounds of the same size, return the one that
starts first."
  (car (cl-sort all-things/bounds #'things--bounds-< :key #'cdr)))

(defun things--largest-bounds (all-things/bounds)
  "Return the largest thing/bounds in ALL-THINGS/BOUNDS.
If there are multiple largest bounds of the same size, return the one that
starts first."
  (car (cl-sort all-things/bounds
                (lambda (&rest args)
                  (not (apply #'things--bounds-< args)))
                :key #'cdr)))

(defun things-bounds (things &optional current-bounds)
  "Get the smallest bounds of a thing at point in THINGS.
If CURRENT-BOUNDS is non-nil, only consider bounds that encompass the current
bounds. If successful, return a cons of the form (thing . bounds). Otherwise, if
there are no THINGS at the point or none of them have bounds greater than
CURRENT-BOUNDS, return nil."
  ;; no bound or checks based on window beginning/end; always should return the
  ;; same bounds for the same point
  (setq things (things--make-things-list things))
  (let* ((all-bounds (if current-bounds
                         (things--all-expanded-bounds things current-bounds)
                       (things--all-bounds things))))
    (things--smallest-bounds all-bounds)))

;; * Bounds Growing
(defun things-expanded-bounds (things current-bounds count)
  "Using the bounds of THINGS, expand CURRENT-BOUNDS COUNT times.
If successful, return a cons of the form (thing . bounds). Otherwise return nil.
Regardless of COUNT, as long as CURRENT-BOUNDS can be expanded at least once,
expansion is considered successful."
  (when current-bounds
    (let (final-thing/bounds)
      (cl-dotimes (_ count)
        (let ((expanded-thing/bounds (things-bounds things current-bounds)))
          (unless expanded-thing/bounds
            (cl-return))
          (setq final-thing/bounds expanded-thing/bounds
                current-bounds (cdr expanded-thing/bounds))))
      final-thing/bounds)))

(defun things--extended-bounds-forward (thing current-bounds count)
  "For THING, extend CURRENT-BOUNDS forward COUNT times.
Extend CURRENT-BOUNDS by moving from its end to the next THING end COUNT times.
Return the new bounds if successful. Otherwise return nil. Regardless of COUNT,
as long as CURRENT-BOUNDS can be extended at least once, extension is considered
successful."
  (when current-bounds
    (let ((end (cdr current-bounds)))
      (save-excursion
        (goto-char end)
        (things-forward thing count)
        (unless (= (point) end)
          (cons (car current-bounds) (point)))))))

(defun things--extended-bounds-backward (thing current-bounds count)
  "For THING, extend CURRENT-BOUNDS backward COUNT times.
Extend CURRENT-BOUNDS by moving from its beginning to the previous THING
beginning COUNT times. Return the new bounds if successful. Otherwise return
nil. Regardless of COUNT, as long as CURRENT-BOUNDS can be extended at least
once, extension is considered successful."
  (when current-bounds
    (let ((beg (car current-bounds)))
      (save-excursion
        (goto-char beg)
        (things-forward thing (- count))
        (unless (= (point) beg)
          (cons (point) (cdr current-bounds)))))))

(defun things-extended-bounds (thing current-bounds count &optional backward)
  "Extend the bounds of THING starting with CURRENT-BOUNDS COUNT times.
Extend CURRENT-BOUNDS by moving from its end to the next THING end COUNT times.
IF BACKWARD is non-nil, extend CURRENT-BOUND by moving from its beginning to the
previous thing beginning COUNT times. If successful, return a cons of the
form (thing . bounds). Otherwise return nil. Regardless of COUNT, as long as
CURRENT-BOUNDS can be extended at least once, extension is considered
successful."
  (unless (things--get thing 'things-no-extend)
    (let ((extended-bounds
           (if backward
               (things--extended-bounds-backward thing current-bounds count)
             (things--extended-bounds-forward thing current-bounds count))))
      (when extended-bounds
        (cons thing extended-bounds)))))

(defun things-growing-bounds (things count &optional current-bounds)
  "Get the smallest bounds of a thing at point in THINGS.
Then expand or extend the bounds COUNT - 1 times. If CURRENT-BOUNDS is non-nil,
expand/extend immediately. If successful, return a cons of the form (thing .
bounds). Otherwise return nil. Regardless of COUNT, as long as CURRENT-BOUNDS
can be grown at least once, growing is considered successful."
  (setq things (things--make-things-list things))
  (let* ((first-thing/bounds (unless current-bounds
                               (cl-decf count)
                               (things-bounds things)))
         (orig-bounds (or current-bounds (cdr first-thing/bounds)))
         (expanded-thing/bounds (things-expanded-bounds things orig-bounds
                                                        count))
         ;; TODO does multi-thing extension make sense?
         (extended-thing/bounds (when (and (not expanded-thing/bounds)
                                           (= (length things) 1))
                                  (things-extended-bounds (car things)
                                                          orig-bounds count))))
    (or extended-thing/bounds expanded-thing/bounds first-thing/bounds)))

;; * Bounds with Seeking
;; TODO bounding seeking to window bounds is an issue;
;; should be unbdounded by default
;; TODO add arg to only seek in one direction
(defun things-seeking-bounds (things &optional current-bounds bound-function)
  "Get the smallest bounds of a thing in THINGS.
It is recommended to use `things-growing-or-seeking-bounds' instead unless you
explicitly do not need/want to support region expansion/extension.

If CURRENT-BOUNDS is non-nil, only consider bounds that encompass
CURRENT-BOUNDS. If no thing at point or no thing at point with bounds greater
than CURRENT-BOUNDS, seek using `things-seek' and ignore CURRENT-BOUNDS. Seeking
is bounded by BOUND-FUNCTION which defaults to `things-bound'. If successful,
return a cons of the form (thing . bounds). Otherwise return nil."
  (or (things-bounds things current-bounds)
      (save-excursion
        (when (things-seek things 1 (or bound-function #'things-bound))
          (things-bounds things)))))

(defun things-growing-or-seeking-bounds (things count &optional current-bounds
                                                bound-function)
  "Get the smallest bounds of a thing in THINGS.
Then expand/extend bounds COUNT - 1 times (or COUNT times if CURRENT-BOUNDS is
non-nil). If no thing at point or no thing at point with bounds greater than
CURRENT-BOUNDS, seek using `things-seek' and ignore COUNT and CURRENT-BOUNDS.
Seeking is bounded by BOUND-FUNCTION which defaults to `things-bound'. If
successful in finding a thing bounds, return a cons of the form (thing .
bounds). Otherwise return nil."
  (or (things-growing-bounds things count current-bounds)
      ;; it is possible for growing to fail but there to still be a thing just
      ;; after the region, e.g. ~...|(foo); check here to prevent seeking past
      ;; it; require that the end be after the current bounds, so that this
      ;; doesn't pick up other bounds that were rejected in
      ;; `things-growing-bounds'
      (when current-bounds
        (let* ((before-thing/bounds
                (save-excursion
                  (goto-char (car current-bounds))
                  (things-bounds things)))
               (after-thing/bounds
                (save-excursion
                  (goto-char (cdr current-bounds))
                  (things-bounds things)))
               (before-bounds (cdr before-thing/bounds))
               (after-bounds (cdr after-thing/bounds)))
          (cond ((and after-bounds
                      (> (cdr after-bounds) (cdr current-bounds)))
                 after-thing/bounds)
                ((and before-bounds
                      (< (car before-bounds) (car current-bounds)))
                 before-thing/bounds))))
      (save-excursion
        (when (things-seek things 1 (or bound-function #'things-bound))
          ;; do not attempt to grow with a count after seeking (count
          ;; potentially growing and seeking would be confusing and not very
          ;; useful; remote thing selection would be preferable)
          (things-bounds things)))))

;; * Next/Previous Bounds
(defun things-next-bounds (things &optional count bound-function)
  "Seek to the next thing in THINGS COUNT times and get its bounds.
Don't seek past the BOUND returned by BOUND-FUNCTION. Return a cons of the
form (thing . bounds) or nil."
  (unless count
    (setq count 1))
  (save-excursion
    (things-seek-forward things count bound-function)
    (things-bounds things)))

(defun things-previous-bounds (things &optional count bound-function)
  "Seek to the previous thing in THINGS COUNT times and get its bounds.
Don't seek before the bound returned by BOUND-FUNCTION. Return a cons of the
form (thing . bounds) or nil."
  (unless count
    (setq count 1))
  (save-excursion
    (things-seek-backward things count bound-function)
    (things-bounds things)))

;; * Remote Bounds
(defun things--overlay-position (thing)
  "Return the position to display an overlay for the current THING.
Generally, this will be the beginning of the thing. This function assumes that
the point is on a THING."
  (let ((overlay-op (things--get thing 'things-overlay-position)))
    (if overlay-op
        (save-excursion
          (funcall overlay-op))
      (car (things-base-bounds thing)))))

(defun things--check-predicate (thing predicate)
  "Move to the beginning of THING and return the result of calling PREDICATE.
Return non-nil if PREDICATE is nil. When PREDICATE is non-nil, and there is no
thing at the point, return nil."
  (if predicate
      (save-excursion
        (let ((bounds (things-base-bounds thing)))
          (when bounds
            (goto-char (car bounds))
            (funcall predicate thing))))
    t))

(cl-defun things-letter-predicate-prompt (n &optional prompt)
  "Return a predicate function to check the point against characters read in.
N is the number of characters to read. When it is 0 or less, use `read-string'
to read as many characters as the user inputs. PROMPT can be specified to
override the default prompt. Return nil if the user aborts."
  (let (string)
    (if (<= n 0)
        (unless (setq string (read-string (or prompt "chars: ")
                                          nil nil nil t))
          (cl-return-from things-letter-predicate-prompt))
      (while (cl-plusp n)
        (let ((char (read-char (or prompt
                                   (format "chars (%s more): %s" n
                                           (concat (reverse string))))
                               t)))
          (unless char
            (cl-return-from things-letter-predicate-prompt))
          (push char string)
          (cl-decf n)))
      (setq string (concat (nreverse string))))
    (lambda (&rest _) (looking-at (regexp-quote string)))))

(defun things--valid-overlay-position-p (overlay-position bound-beg bound-end)
  "Return whether OVERLAY-POSITION is visible and within the given bounds.
BEG-BOUND is inclusive, and END-BOUND is exclusive."
  (and (<= bound-beg overlay-position)
       (< overlay-position bound-end)
       (not (get-char-property overlay-position 'invisible))))

;; NOTE these are not necessary as avy is loaded at compile time (because
;; avy-dowindows is a macro), but they silence flycheck
(declare-function avy-dowindows "avy")
(declare-function avy--find-visible-regions "avy")
(defun things--collect-visible-things (things &optional bound-function predicate)
  "Collect all locations of visible THINGS.
Only search within the range returned by calling BOUND-FUNCTION for both
directions. See `things-bound' for an example implementation. When PREDICATE is
non-nil, only consider things for which the PREDICATE function returns non-nil
at the beginning of. See `things--check-predicate' for information on what
arguments are passed to the PREDICATE."
  (setq things (things--make-things-list things))
  (let (thing-positions)
    (avy-dowindows current-prefix-arg
      (let ((bound-beg (if bound-function
                           (funcall bound-function t)
                         (point-min)))
            (bound-end (if bound-function
                           (funcall bound-function)
                         (point-max))))
        (save-excursion
          ;; TODO when seeking goes to the thing end, this can exclude things
          ;; that start before but end after the window end; need to check only
          ;; the overlay position
          (dolist (visible-region (avy--find-visible-regions bound-beg
                                                             bound-end))
            (goto-char (car visible-region))
            (let ((current-window (get-buffer-window))
                  thing/pos
                  overlay-pos
                  region-thing-positions)
              ;; add a text object at the beginning of the visible region
              ;; as the eol of an invisible line can be visible in org buffers,
              ;; don't do this if the point is at the eol
              (when (and (not (looking-at (rx eol)))
                         (setq thing/pos (things-bounds things))
                         (setq overlay-pos
                               (things--overlay-position (car thing/pos)))
                         ;; overlay-pos could be in a different visible region
                         ;; from the current one
                         (things--valid-overlay-position-p overlay-pos
                                                           bound-beg
                                                           bound-end)
                         (things--check-predicate (car thing/pos) predicate))
                (push overlay-pos region-thing-positions))
              (while (and (setq thing/pos
                                (things-seek-forward things 1
                                                     (lambda ()
                                                       (cdr visible-region))))
                          (setq overlay-pos
                                (things--overlay-position (car thing/pos)))
                          (things--valid-overlay-position-p overlay-pos
                                                            bound-beg
                                                            bound-end)
                          (things--check-predicate (car thing/pos) predicate))
                (push overlay-pos region-thing-positions))
              (setq thing-positions
                    (append thing-positions
                            (mapcar (lambda (x) (cons x current-window))
                                    (sort (delete-dups region-thing-positions)
                                          #'<)))))))))
    thing-positions))

(defvar avy-style)
(declare-function avy--process "avy")
(declare-function avy--style-fn "avy")
(defun things-avy-seek (things &optional bound-function predicate)
  "Seek to a thing in THINGS using avy.
Only consider things within the range returned by calling BOUND-FUNCTION for
both directions. See `things-bound' (the default) for an example implementation.
When PREDICATE is non-nil, only consider things for which the PREDICATE function
returns non-nil at the beginning of. See `things--check-predicate' for
information on what arguments are passed to the PREDICATE."
  (if (require 'avy nil t)
      (let ((positions (things--collect-visible-things
                        things
                        (or bound-function #'things-bound)
                        predicate)))
        (if (not positions)
            (progn (message "No things found.")
                   nil)
          (avy--process positions (avy--style-fn avy-style))))
    (error "Avy must be installed to use this functionality")))

(defun things-remote-bounds (things &optional bound-function predicate)
  "Get the smallest bounds of a thing in THINGS.
Use avy to select the location of the thing. Only consider things within the
range returned by calling BOUND-FUNCTION for both directions. See
`things-bound' (the default) for an example implementation. When PREDICATE is
non-nil, only consider things for which the PREDICATE function returns non-nil
at the beginning of. See `things--check-predicate' for information on what
arguments are passed to the PREDICATE. If successful, return a cons of the
form (thing . bounds). Otherwise return nil."
  (save-excursion
    (if (numberp (things-avy-seek things bound-function predicate))
        (things-bounds things)
      ;; get rid of overlays if necessary
      (keyboard-quit)
      nil)))

;; * Things
;; ** Helpers
;; TODO this should be part of core Emacs or at least a separate library
;; TODO escape syntax character
;; https://github.com/dgutov/highlight-escape-sequences/blob/master/highlight-escape-sequences.el
(defvar things-escape-alist
  ;; TODO ?( is a character but `up-list', syntax higlighting, etc. don't
  ;; consider ? alone an escape character (needs to be ?\<char>)
  ;; TODO how to handle ?\\
  '((t . "\\\\")))

;; TODO use buffer-local variable instead?
;; TODO see `sp-char-is-escaped-t'

;; TODO is using syntax table preferable
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html#Syntax-Class-Table
(defun things--at-escaped-character-p ()
  "Check if the character after the point is escaped."
  (let ((escape-regexp
         (or (cdr (assq major-mode things-escape-alist))
             (cdr (assq t things-escape-alist)))))
    ;; TODO handle case like \\\\\\); unlikely but possible
    ;; for example, while not even count do this
    ;; (string= (string (char-syntax (char-before (point))))
    ;;          "\\")
    (save-match-data
      (things--looking-back escape-regexp))))

;; ** Comment
;; TODO most of this should be part of core Emacs (e.g. newcomment.el) or at
;; least a separate library; ideally there shouldn't be a need to rely on font
;; faces...
(defun things--in-comment-p ()
  "Return whether the point is in a comment.
If the point is in a comment, return its start position. Return nil if the point
is at/in a comment starter."
  (let ((ps (syntax-ppss)))
    (when (nth 4 ps)
      (nth 8 ps))))

(defun things--at-comment-start-p ()
  "Return whether the point is at the beginning of a comment."
  (save-excursion
    ;; \\s< is unreliable
    (and (looking-at comment-start-skip)
         (goto-char (match-end 0))
         (things--in-comment-p))))

(defun things--at-block-comment-p ()
  "Return whether the point is at the beginning of a block comment.
This function assumes that the point is at the beginning of some type of
comment."
  (let ((comment-beg (point)))
    ;; (and (things--at-comment-start-p) ...)
    (or
     ;; one-line block comment
     (save-excursion
       (end-of-line)
       (not (things--in-comment-p)))
     ;; unnecessary with next check
     ;; (save-excursion
     ;;   (forward-comment 1)
     ;;   (> (- (count-lines comment-beg (point))
     ;;         (if (bolp) 0 1))
     ;;      1))
     ;; multi-line block comment
     (save-excursion
       (forward-line)
       (equal (things--in-comment-p) comment-beg)))))

(defun things--comment-beginning ()
  "Go to the beginning of the current comment.
If no comment at the point or at a comment beginning or just after a comment
end, return nil and do not move the point. This works in the middle of a comment
starter."
  (let ((comment-beg
         (save-excursion
           ;; (end-of-line)
           (when (eq (get-text-property (point) 'face)
                     'font-lock-comment-delimiter-face)
             (let ((non-comment-starter-pos
                    (next-single-property-change (point) 'face)))
               (when non-comment-starter-pos
                 (goto-char non-comment-starter-pos))))
           (comment-beginning))))
    (when (and comment-beg
               ;; should return nil at first comment starter character
               (< comment-beg (point)))
      (goto-char comment-beg))))

(defsubst things--looking-back (regexp)
  "Forward to (`looking-back' REGEXP (line-beginning-position))."
  (looking-back regexp (line-beginning-position)))

(defun things--backward-aggregated-comment-begin (count)
  "Move to the previous aggregated comment beginning COUNT times.
Line comments that start at the same column and have no code in between them are
considered to be one \"aggregated\" comment. Block comments are not aggregated.
The idea is to aggregate adjacent line comments that could be one sentence and
contain delimiter pairs."
  (things-move-with-count count
    ;; unfortunately, `comment-end-skip' and `block-comment-end' are not always
    ;; defined, and `comment-end' applies inconsistently to line or block
    ;; comments; `syntax-ppss' returns nil in the middle of a comment starter;
    ;; newcomment.el and evilnc both check faces
    (let ((comment-beg (or (things--comment-beginning)
                           (things--reset-pos-when-nil
                             ;; TODO this supposedly does not work in all cases
                             ;; where a comment starter appears in a comment
                             (comment-search-backward nil t))))
          col)
      (when comment-beg
        ;; comment search functions skip past comment beginning
        (goto-char comment-beg)
        (setq col (current-column))
        ;; extend comment
        (when (not (things--at-block-comment-p))
          (while (and
                  ;; old comment should not be separated from new comment by
                  ;; code
                  (things--looking-back "^[[:space:]]*")
                  ;; `forward-comment' will work to move the point to a line
                  ;; comment on the previous line; it will also return nil at
                  ;; bobp (no extra check necessary)
                  (things--reset-pos-when-nil
                    (forward-comment -1))
                  ;; `forward-comment' still moves the point on failure
                  (things--at-comment-start-p)
                  ;; TODO pull this, col check, and maybe space check into
                  ;; helper for readability the new comment should be on the
                  ;; previous line
                  (= (- (count-lines (point) comment-beg)
                        (if (bolp) 0 1))
                     1)
                  ;; the new comment should start at the same column
                  (= (current-column) col)
                  ;; the new comment should not be a block comment (one line
                  ;; block comments are rare but possible)
                  (not (things--at-block-comment-p)))
            (setq comment-beg (point))))
        (goto-char comment-beg)))))

(defun things--forward-aggregated-comment-end (count)
  "Move to the next aggregated comment end COUNT times.
Line comments that start at the same column and have no code in between them are
considered to be one \"aggregated\" comment. Block comments are not aggregated.
The idea is to aggregate adjacent line comments that could be one sentence and
contain delimiter pairs."
  (things-move-with-count count
    (let ((comment-beg (or (things--comment-beginning)
                           (things--reset-pos-when-nil
                             (comment-search-forward nil t))))
          new-comment-beg
          col)
      (when comment-beg
        (goto-char comment-beg)
        (setq col (current-column))
        ;; extend comment
        (when (not (things--at-block-comment-p))
          (while (and
                  ;; go to comment end (should be guaranteed to work at comment beginning)
                  (forward-comment 1)
                  ;; go to next comment beginning
                  (things--reset-pos-when-nil
                    (setq new-comment-beg (comment-search-forward nil t)))
                  (goto-char new-comment-beg)
                  ;; old comment should not be separated from new comment by
                  ;; code
                  (things--looking-back "^[[:space:]]*")
                  ;; old comment should not be a multi-line comment or separated
                  ;; from new comment by multiple lines
                  (= (- (count-lines comment-beg (point))
                        (if (bolp) 0 1))
                     1)
                  ;; new comment should start at the same column
                  (= (current-column) col)
                  ;; the new comment should not be a block comment
                  (not (things--at-block-comment-p)))
            (setq comment-beg new-comment-beg)))
        (goto-char comment-beg)
        (things--reset-pos-when-nil
          (forward-comment 1))))))

(defun things-forward-aggregated-comment (&optional count)
  "Move forward across an aggregated comment COUNT times.
Line comments that start at the same column and have no code in between them are
considered to be one \"aggregated\" comment. Block comments are not aggregated."
  (unless count
    (setq count 1))
  (save-match-data
    (if (cl-plusp count)
        (things--forward-aggregated-comment-end count)
      (things--backward-aggregated-comment-begin (- count)))))
(put 'things-aggregated-comment 'forward-op #'things-forward-aggregated-comment)

(defun things-get-inner-aggregated-comment (thing/bounds)
  "Shrink THING/BOUNDS to exclude `comment-start-skip' and `comment-end-skip'."
  (things-shrink-by-regexp thing/bounds comment-start-skip comment-end-skip))

(put 'things-aggregated-comment 'things-get-inner
     #'things-get-inner-aggregated-comment)
(put 'things-aggregated-comment 'things-get-a #'identity)

;; ** String
(defconst things-string-regexp (rx (1+ (or (syntax string-quote)
                                           (syntax string-delimiter))))
  "Regexp to potentially match a string start or end character.")

;; TODO save-match-data? (used by pair definer but doesn't seem to cause an
;; issue with next-open etc.)
(defun things--bounds-string ()
  "Return the bounds of the string at the point or nil.
This is a slightly modified `lispy--bounds-string'. It will return the bounds
even if the point is at the very beginning of end of those bounds (inclusive)."
  ;; TODO does this check just slow things down? remove probably
  (unless (things--in-comment-p)
    (let ((beg (or (nth 8 (syntax-ppss))
                   ;; (and (looking-at things-string-regexp)
                   ;;      (not (things--at-escaped-character-p))
                   ;;      (point))
                   (when (looking-at things-string-regexp)
                     (save-excursion
                       (forward-char)
                       (nth 8 (syntax-ppss))))
                   (when (things--looking-back things-string-regexp)
                     (save-excursion
                       (backward-char)
                       (nth 8 (syntax-ppss)))))))
      (when (and beg (not (comment-only-p beg (1+ (point)))))
        (ignore-errors
          (cons beg (save-excursion
                      (goto-char beg)
                      (forward-sexp)
                      (point))))))))
(put 'things-string 'bounds-of-thing-at-point #'things--bounds-string)

(defun things--backward-string-begin (count)
  "Move to the previous string beginning COUNT times."
  (things-move-with-count count
    (let ((start-pos (point))
          bounds)
      (while (and (re-search-backward things-string-regexp nil t)
                  (not (setq bounds (things--bounds-string)))))
      (if bounds
          (goto-char (car bounds))
        (goto-char start-pos)))))

(defun things--forward-string-end (count)
  "Move the next string end COUNT times."
  (things-move-with-count count
    (let ((start-pos (point))
          bounds)
      (while (and (re-search-forward things-string-regexp nil t)
                  (not (setq bounds (things--bounds-string)))))
      (if bounds
          (goto-char (cdr bounds))
        (goto-char start-pos)))))

(defun things-forward-string (&optional count)
  "Move forward across a string COUNT times."
  (unless count
    (setq count 1))
  (if (cl-plusp count)
      (things--forward-string-end count)
    (things--backward-string-begin (- count))))
(put 'things-string 'forward-op #'things-forward-string)

(defun things-get-inner-string (thing/bounds)
  "Shrink THING/BOUNDS to exclude `things-string-regxep' on both sides."
  (things-shrink-by-regexp thing/bounds
                           things-string-regexp
                           things-string-regexp))

(put 'things-string 'things-get-inner #'things-get-inner-string)
(put 'things-string 'things-get-a #'identity)

;; ** Line
(put 'things-line 'forward-op #'forward-line)

(put 'things-line 'things-get-inner #'things-shrink-by-newlines)
(put 'things-line 'things-get-a #'identity)

;; ** Function
(defun things-forward-function (&optional count)
  "Move to the next function end COUNT times.
With a negative COUNT, move to the previous function beginning."
  (unless count
    (setq count 1))
  (if (cl-plusp count)
      (things-move-with-count count
        (end-of-defun))
    (things-move-with-count (- count)
      (beginning-of-defun))))
(put 'things-function 'forward-op #'things-forward-function)
(put 'things-function 'things-no-extend t)

(put 'things-function 'things-get-inner #'things-shrink-by-newlines)
(put 'things-function 'things-get-a #'identity)

;; * Thing Type Definers
;; These make it easy to automatically implement all necessary functions for
;; types of things (e.g. things bounded by regexps).

;; ** Helpers
(cl-defun things-next-regexp (regexp &key backward move skip-past-point
                                     predicate bound)
  "Return the match data of the next position matched by REGEXP.
If BACKWARD is non-nil, search backward instead of forward. If MOVE is non-nil,
move to the match instead of returning the match data. By default, move to the
match end when searching forward and to the match beginning when searching
backward. MOVE can also be 'begin or 'end to explicitly choose which part of the
match to move to. When SKIP-PAST-POINT is non-nil, move past an occurence of
REGEXP that begins at the point before searching. Call PREDICATE (if specified)
at each match to confirm that the point is at a valid occurrence. For example,
the predicate could return nil if the point is preceded by an escape character.
The match data of the each occurence is available for use in PREDICATE (with
`match-data', `match-end', etc.). It is also fine for the PREDICATE to move the
point to move the point (e.g. to skip past an invalid region). If there are no
valid matches, return nil. BOUND, if specified, is a buffer position to bound
the search."
  ;; TODO based on the function name I would expect quoting to happen outside
  (setq regexp (regexp-quote regexp))
  (save-match-data
    (let ((start-pos (point))
          successp)
      (when (and skip-past-point
                 (if backward
                     (things--looking-back regexp)
                   (looking-at regexp)))
        (goto-char (if backward
                       (match-beginning 0)
                     (match-end 0))))
      (while (and (if backward
                      (re-search-backward regexp bound t)
                    (re-search-forward regexp bound t))
                  (setq successp t)
                  (when predicate
                    (not (funcall predicate))))
        (setq successp nil))
      (cond (successp
             (if move
                 (goto-char (cl-case move
                              (begin (match-beginning 0))
                              (end (match-end 0))
                              (t (if backward
                                     (match-beginning 0)
                                   (match-end 0)))))
               (goto-char start-pos))
             (match-data))
            (t
             (goto-char start-pos)
             nil)))))

;; ** Pairs
(defmacro things--with-adjusted-syntax-table (open close &rest body)
  "With OPEN and CLOSE set to parens in an empty syntax table, run BODY.
Also set `forward-sexp-function' to nil while running BODY."
  (declare (indent 2))
  ;; won't be necessary without "abuse" , but use once-only for consistency
  (mmt-once-only (open close)
    `(let (forward-sexp-function)
       (with-syntax-table (make-syntax-table
                           (make-char-table 'things-pair-table))
         (modify-syntax-entry (string-to-char ,open) (format "(%s" ,close))
         (modify-syntax-entry (string-to-char ,close) (format ")%s" ,open))
         ,@body))))

;; https://github.com/emacs-mirror/emacs/commit/76e297c15f6312a83599aab216be0396e9aac5c5#diff-8636894786ef40e6db7d629928e663ee
;; don't want enclosing list: https://github.com/emacs-mirror/emacs/commit/1e3b3fa6159db837fca2f2d564e51b01048a906f#diff-8636894786ef40e6db7d629928e663ee
(defun things--old-thing-at-point-bounds-of-list-at-point ()
  "Return the bounds of the list at point.
\[Internal function used by `bounds-of-thing-at-point'.]"
  (save-excursion
    (let* ((st (parse-partial-sexp (point-min) (point)))
           (beg (or (and (eq 4 (car (syntax-after (point))))
                         (not (nth 8 st))
                         (point))
                    (nth 1 st))))
      (when beg
        (goto-char beg)
        (forward-sexp)
        (cons beg (point))))))

(defun things--bounds-of-char-pair-at-point (open close)
  "Return the bounds of a pair at point bounded by OPEN and CLOSE."
  ;; NOTE this correctly handles edge situations like (foo (bar)|) for things'
  ;; spec (should return the bounds of foo list)
  (things--with-adjusted-syntax-table open close
    (ignore-errors (things--old-thing-at-point-bounds-of-list-at-point))))

(defun things--forward-char-pair-begin (open close)
  "Go to the next valid beginning of an OPEN CLOSE pair."
  (things-next-regexp open
                      :move 'begin
                      :skip-past-point t
                      :predicate
                      (lambda ()
                        ;; TODO check whether bounds function should change
                        ;; match data and whether they currently do
                        (save-excursion
                          (save-match-data
                            (goto-char (match-beginning 0))
                            (equal (car (things--bounds-of-char-pair-at-point
                                         open close))
                                   (point)))))))

(defun things--forward-char-pair-end (open close)
  "Go to the next valid end of an OPEN CLOSE pair."
  (things-next-regexp close
                      :move 'end
                      :predicate
                      (lambda ()
                        (save-match-data
                          (equal (cdr (things--bounds-of-char-pair-at-point
                                       open close))
                                 (point))))))

(defun things--backward-char-pair-begin (open close)
  "Go to the previous valid beginning of an OPEN CLOSE pair."
  (things-next-regexp open
                      :backward t
                      :move 'begin
                      :predicate
                      (lambda ()
                        (save-match-data
                          (equal (car (things--bounds-of-char-pair-at-point
                                       open close))
                                 (point))))))

(defun things--backward-char-pair-end (open close)
  "Go to the previous valid end of an OPEN CLOSE pair."
  (things-next-regexp close
                      :backward t
                      :move 'end
                      :skip-past-point t
                      :predicate
                      (lambda ()
                        (save-excursion
                          (save-match-data
                            (goto-char (match-end 0))
                            (equal (cdr (things--bounds-of-char-pair-at-point
                                         open close))
                                   (point)))))))

(defun things--forward-char-pair (open close &optional count)
  "Go to the next valid end of an OPEN CLOSE pair COUNT times.
With a negative count, go to the previous valid beginning of the pair."
  (unless count
    (setq count 1))
  (if (cl-plusp count)
      (things-move-with-count count
        (things--forward-char-pair-end open close))
    (things-move-with-count (- count)
      (things--backward-char-pair-begin open close))))

(defun things--alt-forward-char-pair (open close &optional count)
  "Go to the next valid beginning of an OPEN CLOSE pair COUNT times.
With a negative count, go to the previous valid end of the pair."
  (unless count
    (setq count 1))
  (if (cl-plusp count)
      (things-move-with-count count
        (things--forward-char-pair-begin open close))
    (things-move-with-count (- count)
      (things--backward-char-pair-end open close))))

(defun things--seek-char-pair (open close &optional count)
  "Go to the next valid beginning of an OPEN CLOSE pair COUNT times.
With a negative count, go to the previous valid beginning of the pair."
  (unless count
    (setq count 1))
  (if (cl-plusp count)
      (things-move-with-count count
        (things--forward-char-pair-begin open close))
    (things-move-with-count (- count)
      (things--backward-char-pair-begin open close))))

(defun things-define-pair (name open close)
  "Create a pair thing called NAME bounded by OPEN and CLOSE."
  (when (> (length open) 1)
    (error "Only single character pairs are currently supported"))
  (when (> (length close) 1)
    (error "Only single character pairs are currently supported"))
  (put name 'bounds-of-thing-at-point
       (lambda ()
         (things--bounds-of-char-pair-at-point open close)))
  (put name 'forward-op
       (lambda (&optional count)
         (things--forward-char-pair open close count)))
  (put name 'things-alt-forward-op
       (lambda (_thing &optional count)
         (things--alt-forward-char-pair open close count)))
  (put name 'things-seek-op
       (lambda (thing &optional count)
         ;; TODO this should be easier on the implementor
         (things-move-with-count count
           (things-move-while-not
               (let ((bounds (things-base-bounds thing)))
                 (and bounds (= (point) (car bounds))))
             (things--seek-char-pair open close count)))))
  (put name 'things-seeks-forward-begin t)
  (put name 'things-no-extend t)
  (put name 'things-overlay-position #'point)
  (put name 'things-get-inner
       (lambda (thing/bounds)
         (things-shrink-by-regexp thing/bounds
                                  (regexp-quote open)
                                  (regexp-quote close))))
  (put name 'things-get-a #'identity)
  name)

;; ** Separators
;; (defun things--bounds-of-separator-at-point (separator)
;;   (save-excursion
;;     (let (beg end)
;;       (when (looking-at separator)
;;         (setq beg (point))
;;         (goto-char (match-end 0)))
;;       (setq end (when (re-search-forward separator nil t)
;;                   (goto-char (match-beginning 0))))
;;       (unless beg
;;         (setq beg (re-search-backward separator nil t)))
;;       (when (and beg end)
;;         (cons beg end)))))

(defun things--forward-separator (separator &optional count
                                            include-buffer-bounds)
  "Move to the end of the next SEPARATOR up to COUNT times.
With a negative COUNT, move to the previous SEPARATOR up to COUNT times. By
default, a SEPARATOR can only be a closing SEPARATOR if there is one prior in
the buffer. For example, with a buffer \"|foo, bar, baz\",
calling (things--forward-separator \",\" 1) would result in \"foo, bar|, baz\".
Similarly, with \"foo, bar, baz|\", (things--forward-separator \",\" -1) would
result in \"foo|, bar, baz\". To allow the buffer beginning and end to be the
start and end of a separator thing respectively, INCLUDE-BUFFER-BOUNDS can be
specified as non-nil. If able to move to a valid separator at least once, return
the point. Otherwise return nil."
  (unless count
    (setq count 1))
  (unless include-buffer-bounds
    ;; check if in already between separators
    (unless (save-excursion
              (if (cl-plusp count)
                  (or (looking-at separator)
                      (re-search-backward separator nil t))
                (re-search-forward separator nil t)))
      (if (cl-plusp count)
          (cl-incf count)
        (cl-decf count))))
  (cond ((cl-plusp count)
         (when (looking-at separator)
           (goto-char (match-end 0)))
         (when include-buffer-bounds
           (setq separator (rx-to-string `(or buffer-end ,separator))))
         (when (things-move-with-count count
                 ;; use `things-move-with-count' since `re-search-forward' will
                 ;; not move the point if there are less than COUNT matches
                 (re-search-forward separator nil t))
           (goto-char (match-beginning 0))))
        (t
         (when include-buffer-bounds
           (setq separator (rx-to-string `(or buffer-start ,separator))))
         (things-move-with-count (- count)
           (re-search-backward separator nil t)))))

(defun things-define-separator (name separator &optional include-buffer-bounds)
  "Create a separator thing called NAME bounded by SEPARATOR.
When INCLUDE-BUFFER-BOUNDS is non-nil, a separator thing can start at the buffer
beginning or end at the buffer end."
  (put name 'forward-op
       (lambda (&optional count)
         (things--forward-separator separator count include-buffer-bounds)))
  (put name 'things-bounds-of-thing-at-point
       (lambda ()
         (let ((bounds (bounds-of-thing-at-point name)))
           (if include-buffer-bounds
               (when (and bounds
                          ;; require at least one separator
                          (or (save-excursion
                                (goto-char (car bounds))
                                (looking-at separator))
                              (save-excursion
                                (goto-char (cdr bounds))
                                (looking-at separator))))
                 bounds)
             bounds))))
  (put name 'things-seek-op #'things--seek-separator)
  (put name 'things-get-inner
       (lambda (thing/bounds)
         (things-shrink-by-regexp thing/bounds separator nil)))
  (put name 'things-get-a #'identity))

(provide 'things)
;;; things.el ends here
