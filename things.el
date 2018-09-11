;;; things.el --- Extensions to thingatpt            -*- lexical-binding: t; -*-

;; Author: Fox Kiester <noct@posteo.net>
;; URL: https://github.com/noctuid/things.el
;; Created: September 8, 2018
;; Keywords: convenience, text-object
;; Package-Requires: ((cl-lib "0.5") (emacs "24.4") (avy "0.4.0"))
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

;; For more information see the README in the repository.

;;; Code:
(require 'cl-lib)
(require 'thingatpt)
;; avy must be loaded at compile time so `avy-do-windows' can be expanded
;; avy is not necessary at runtime until `things-remote-bounds' is called
(eval-when-compile
  (require 'avy))

(defgroup things nil
  "Provides extensions to thingatpt."
  :group 'convenience
  :prefix "things-")

;; * Seeking
(defun things-bound (&optional backwards)
  "Return the bound to be used when seeking forwards or backwards.
BACKWARDS specifies that the bound should be for seeking backwards. This
function is used by bounds functions that use seeking. This default function
restricts seeking to between the beginning and end of the window. A custom
function can be used by changing the `things-bound' variable."
  (if backwards
      (window-start)
    (window-end)))

(defun things--seeks-to-end-p (thing &optional backward)
  "Return whether seeking forward for THING will go to the thing end.
If BACKWARD is non-nil, return whether seeking backward for THING goes to the
thing end."
  (if backward
      (get thing 'things-seeks-backward-end)
    (not (get thing 'things-seeks-forward-begin))))

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
  (let* ((bounds (bounds-of-thing-at-point thing))
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
              (backward-char)
              (let ((bounds (bounds-of-thing-at-point thing)))
                ;; e.g. should not use previous bounds for (|(; this can only
                ;; happen when things-seeks-forward-begin is not set correctly
                (when (and bounds (or (= (point) (cdr bounds))
                                      ;; e.g. list thing
                                      (= (1+ (point)) (cdr bounds))))
                  bounds))))))
    (or bounds-at-previous-char bounds)))

(defun things--forward (thing &optional count)
  "Call THING's things-seek-op or `forward-thing' with COUNT."
  (let ((forward-op (get thing 'things-seek-op)))
    (if forward-op
        (funcall forward-op count)
      (forward-thing thing count))))

(defun things--min (&rest numbers)
  "Same as `min' but remove nils from NUMBERS."
  (apply #'min (remove nil numbers)))

(cl-defun things--seek-forward (thing &optional count bound)
  "Seek forward to THING COUNT times.
Seek to the next thing if there is an existing thing at the point. If BOUND is
non-nil, do not seek beyond BOUND. If successful, move the point and return the
new position. Otherwise return nil."
  (or count (setq count 1))
  (when (< count 0)
    (things--seek-backward thing count bound)
    (cl-return-from things--seek-forward))
  (setq bound (things--min bound (point-max)))

  (let ((orig-pos (point))
        (initial-bounds (bounds-of-thing-at-point thing))
        seek-start-pos)
    ;; go to the end of the current or next thing
    (things--forward thing)
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
        (things--forward thing)
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
  (when (< count 0)
    (things--seek-forward thing count bound)
    (cl-return-from things--seek-backward))
  (setq bound (things--max bound (point-min)))
  (let ((orig-pos (point))
        (initial-bounds (bounds-of-thing-at-point thing))
        seek-start-pos)
    ;; go to the current or previous thing beginning
    (things--forward thing (- count))
    (if (equal (things--after-seek-bounds thing t) initial-bounds)
        (setq seek-start-pos (point))
      (setq seek-start-pos orig-pos)
      (cl-decf count))
    (cl-dotimes (_ count)
      (let ((pos (point)))
        (things--forward thing -1)
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

(cl-defun things--seek (things bound-function &key
                               forward-only
                               backward-only
                               prefer-backward
                               prefer-closest)
  "Seek to a thing in THINGS.
See `things-seek' for more information on BOUND-FUNCTION, FORWARD-ONLY,
BACKWARD-ONLY, PREFER-BACKWARD, and PREFER-CLOSEST. The only difference is that
`things-seek' additionally supports a count."
  (unless (listp things)
    (setq things (list things)))
  (let* ((forward-positions
          (unless backward-only
            (mapcar (lambda (thing)
                      (save-excursion
                        (when (things--seek-forward thing 1
                                                    (funcall bound-function))
                          (cons thing (point)))))
                    things)))
         (sorted-forward-positions (cl-sort forward-positions #'< :key #'cdr))
         (closest-forward-thing/pos (car sorted-forward-positions))

         (backward-positions
          (unless forward-only
            (mapcar (lambda (thing)
                      (save-excursion
                        (when (things--seek-backward thing 1
                                                     (funcall bound-function t))
                          (cons thing (point)))))
                    things)))
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
  (let ((orig-pos (point)))
    (cl-dotimes (_ count)
      ;; (apply #'things--seek things bound-function kargs)
      (unless (things--seek things bound-function
                            :forward-only forward-only
                            :backward-only backward-only
                            :prefer-backward prefer-backward
                            :prefer-closest prefer-closest)
        (cl-return)))
    (unless (= (point) orig-pos)
      (point))))

(defun things-seek-forward (&rest args)
  "Call `things-seek' with ARGS and \":forward-only t\"."
  (apply #'things-seek (append args (list :forward-only t))))

(defun things-seek-backward (&rest args)
  "Call `things-seek' with ARGS and \":backward-only t\"."
  (apply #'things-seek (append args (list :backward-only t))))

;; * Bounds at Point
(defun things--bounds-inside-p (current-bounds bounds)
  "Return whether CURRENT-BOUNDS is inside and not exactly BOUNDS."
  (and (not (equal bounds current-bounds))
       (<= (car bounds) (car current-bounds))
       (>= (cdr bounds) (cdr current-bounds))))

(defun things--bounds-size (bounds)
  "Return the distance between the beginning and end of BOUNDS."
  (- (cdr bounds) (car bounds)))

(defun things--bounds-< (bounds1 bounds2)
  "Return whether the size of BOUNDS1 is less than BOUNDS2."
  (< (things--bounds-size bounds1)
     (things--bounds-size bounds2)))

(defun things--bounds (things)
  "Get all bounds for THINGS at point.
Return a list of conses of the form (thing . bounds) or nil if unsuccessful."
  (mapcar (lambda (thing)
            (let ((bounds (bounds-of-thing-at-point thing)))
              (when bounds
                (cons thing bounds))))
          things))

(defun things--expanded-bounds (things current-bounds)
  "Get all bounds of THINGS that encompass CURRENT-BOUNDS.
Return a list of conses of the form (thing . bounds) or nil if unsuccesful."
  ;; if inside a nested list, for example, this function should not return the
  ;; same bounds as CURRENT-BOUNDS, so bounds are checked just before and after
  ;; CURRENT-BOUNDS (positions where `bounds-of-thing-at-point' cannot return
  ;; the same bounds assuming the thing is correctly implemented); the case
  ;; where CURRENT-BOUNDS starts at the buffer beginning or ends at the buffer
  ;; end (making it impossible to move before or after CURRENT-BOUNDS) is
  ;; handled by `things--bounds-inside-p'
  (let* ((before-things/bounds (save-excursion
                                 (goto-char (1- (car current-bounds)))
                                 (things--bounds things)))
         (after-things/bounds (save-excursion
                                (goto-char (cdr current-bounds))
                                (things--bounds things)))
         (all-things/bounds (cl-union before-things/bounds after-things/bounds
                                      :test #'equal))
         (encompassing-things/bounds (cl-remove-if (apply-partially
                                                    #'things--bounds-inside-p
                                                    current-bounds)
                                                   all-things/bounds
                                                   :key #'cdr)))
    encompassing-things/bounds))

(defun things-bounds (things &optional current-bounds)
  "Get the smallest bounds of a thing at point in THINGS.
If CURRENT-BOUNDS is non-nil, only consider bounds that encompass the current
bounds. Return a cons of the form (thing . bounds) or nil if unsuccessful."
  ;; no bound or checks based on window beginning/end; always should return the
  ;; same bounds for the same point
  (unless (listp things)
    (setq things (list things)))
  (let* ((all-bounds (if current-bounds
                         (things--expanded-bounds things current-bounds)
                       (things--bounds things)))
         (sorted-bounds (cl-sort all-bounds
                                 ;; favor the smallest bounds
                                 #'things--bounds-< :key #'cdr))
         (smallest-size (things--bounds-size (cdar sorted-bounds)))
         (smallest-bounds
          ;; take while same size
          (cl-loop for thing-bounds in sorted-bounds
                   while (= (things--bounds-size (cdr thing-bounds))
                            smallest-size)
                   collect thing-bounds)))
    (if (<= (length smallest-bounds) 1)
        (car smallest-bounds)
      ;; if multiple bounds of same size, favor the one that starts first
      (car (cl-sort smallest-bounds #'< :key #'cadr)))))

;; * Bounds Growing
(defun things-expanded-bounds (things current-bounds count)
  "Using the bounds of THINGS, expand CURRENT-BOUNDS COUNT times.
If successful, return a cons of the form (thing . bounds). Otherwise return.
Regardless of COUNT, as long as CURRENT-BOUNDS can be expanded at least once,
expansion is considered successful."
  (let (final-thing/bounds)
    (cl-dotimes (_ count)
      (let ((expanded-thing/bounds (things-bounds things current-bounds)))
        (unless expanded-thing/bounds
          (cl-return))
        (setq final-thing/bounds expanded-thing/bounds
              current-bounds (cdr expanded-thing/bounds))))
    final-thing/bounds))

(defun things--extended-bounds-forward (thing current-bounds count)
  "For THING, extend CURRENT-BOUNDS forwards COUNT times.
Extend CURRENT-BOUNDS by moving from its end to the next THING end COUNT times.
Return the new bounds if successful. Otherwise return nil. Regardless of COUNT,
as long as CURRENT-BOUNDS can be extended at least once, extension is considered
successful."
  (let ((end (cdr current-bounds)))
    (save-excursion
      (goto-char end)
      (forward-thing thing count)
      (unless (= (point) end)
        (cons (car current-bounds) (point))))))

(defun things--extended-bounds-backward (thing current-bounds count)
  "For THING, extend CURRENT-BOUNDS backwards COUNT times.
Extend CURRENT-BOUNDS by moving from its beginning to the previous THING
beginning COUNT times. Return the new bounds if successful. Otherwise return
nil. Regardless of COUNT, as long as CURRENT-BOUNDS can be extended at least
once, extension is considered successful."
  (let ((beg (car current-bounds)))
    (save-excursion
      (goto-char beg)
      (forward-thing thing (- count))
      (unless (= (point) beg)
        (cons (point) (cdr current-bounds))))))

(defun things-extended-bounds (thing current-bounds count &optional backwards)
  "Extend the bounds of THING starting with CURRENT-BOUNDS COUNT times.
Extend CURRENT-BOUNDS by moving from its end to the next THING end COUNT times.
IF BACKWARDS is non-nil, extend CURRENT-BOUND by moving from its beginning to
the previous thing beginning COUNT times. If successful, return a cons of the
form (thing . bounds). Otherwise return nil. Regardless of COUNT, as long as
CURRENT-BOUNDS can be extended at least once, extension is considered
successful."
  (let ((extended-bounds
         (if backwards
             (things--extended-bounds-backward thing current-bounds count)
           (things--extended-bounds-forward thing current-bounds count))))
    (when extended-bounds
      (cons thing extended-bounds))))

(defun things-growing-bounds (things count &optional current-bounds)
  "Get the smallest bounds of a thing at point in THINGS.
Then expand or extend the bounds COUNT - 1 times. If CURRENT-BOUNDS is non-nil,
expand/extend immediately. If successful, return a cons of the form (thing .
bounds). Otherwise return nil. Regardless of COUNT, as long as CURRENT-BOUNDS
can be grown at least once, growing is considered successful."
  (let* ((first-thing/bounds (unless current-bounds
                               (cl-decf count)
                               (things-bounds things)))
         (orig-bounds (or current-bounds (cdr first-thing/bounds)))
         (expanded-thing/bounds (things-expanded-bounds things orig-bounds
                                                        count))
         (extended-thing/bounds (when (and (not expanded-thing/bounds)
                                           (= (length things) 1))
                                  (things-extended-bounds (car things)
                                                          orig-bounds count))))
    (or extended-thing/bounds expanded-thing/bounds first-thing/bounds)))

;; * Bounds with Seeking
(defun things-seeking-bounds (things &optional current-bounds bound-function)
  "Get the smallest bounds of a thing in THINGS.
It is recommended to use `things-seeking-growing-bounds' instead unless you
explicitly do not need/want to support region expansion/extension.

If CURRENT-BOUNDS is non-nil, only consider bounds that encompass
CURRENT-BOUNDS. If no thing at point, seek using `things-seek' and ignore
CURRENT-BOUNDS. Seeking is bounded by BOUND-FUNCTION which defaults to
`things-bound'. If successful, return a cons of the form (thing . bounds).
Otherwise return nil."
  (or (things-bounds things current-bounds)
      (save-excursion
        (when (things-seek things 1 (or bound-function #'things-bound))
          (things-bounds things)))))

(defun things-seeking-growing-bounds (things count &optional current-bounds
                                             bound-function)
  "Get the smallest bounds of a thing in THINGS.
Then expand/extend bounds COUNT - 1 times (or COUNT times if CURRENT-BOUNDS is
non-nil). If no thing at point, seek using `things-seek' and ignore COUNT and
CURRENT-BOUNDS. Seeking is bounded by BOUND-FUNCTION which defaults to
`things-bound'. If successful in finding a thing bounds, return a cons of the
form (thing . bounds). Otherwise return nil."
  (or (things-growing-bounds things count current-bounds)
      (save-excursion
        (when (things-seek things 1 (or bound-function #'things-bound))
          ;; do not attempt to grow with a count after seeking (count
          ;; potentially growing and seeking would be confusing and not very
          ;; useful; remote thing selection would be preferable)
          (things-bound things)))))

;; * Next/Previous Bounds
(defun things-next-bounds (things count bound)
  "Seek to the next thing in THINGS COUNT times and get its bounds.
Don't seek past BOUND. Return a cons of the form (thing . bounds) or nil."
  (things-seek-forward things count bound)
  (things-bounds things))

(defun things-previous-bounds (things count bound)
  "Seek to the previous thing in THINGS COUNT times and get its bounds.
Don't seek before BOUND. Return a cons of the form (thing . bounds) or nil."
  (things-seek-backward things count bound)
  (things-bounds things))

;; * Remote Bounds
(defun things--overlay-position (thing)
  "Return the position to display an overlay for the current THING.
Generally, this will be the beginning of the thing. This function assumes that
the point is on a THING."
  (let ((overlay-op (get thing 'targets-overlay-position)))
    (if overlay-op
        (save-excursion
          (funcall overlay-op)
          (point))
      (car (bounds-of-thing-at-point thing)))))

(defun things--check-predicate (thing predicate)
  "Go to the beginning of THING and return the result of calling PREDICATE.
Return non-nil if PREDICATE is nil. When PREDICATE is non-nil, and there is no
thing at the point, return nil."
  (if predicate
      (save-excursion
        (let ((bounds (bounds-of-thing-at-point thing)))
          (when bounds
            (goto-char (car bounds))
            (funcall predicate))))
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
      (while (> n 0)
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
  (unless (listp things)
    (setq things (list things)))
  (let (thing-positions)
    (avy-dowindows current-prefix-arg
      (save-excursion
        (dolist (visible-region (avy--find-visible-regions
                                 (funcall bound-function t)
                                 (funcall bound-function)))
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
                       ;; overlay position must be visible
                       (< (car visible-region)
                          overlay-pos
                          (cdr visible-region))
                       (things--check-predicate (car thing/pos) predicate))
              (push overlay-pos region-thing-positions))
            (while (and (setq thing/pos
                              (things-seek-forward things 1
                                                   (lambda ()
                                                     (cdr visible-region))))
                        (setq overlay-pos
                              (things--overlay-position (car thing/pos)))
                        ;; overlay position must be visible
                        (< (car visible-region)
                           overlay-pos
                           (cdr visible-region))
                        (things--check-predicate (car thing/pos) predicate))
              (push overlay-pos region-thing-positions))
            (setq thing-positions
                  (append thing-positions
                          (mapcar (lambda (x) (cons x current-window))
                                  (sort (delete-dups region-thing-positions)
                                        #'<))))))))
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

;; * Final Bounds Adjustment
(defun things--get-inner (thing/bounds)
  "Shrink the bounds in THING/BOUNDS by 1 character on each side."
  (let ((bounds (cdr thing/bounds)))
    (cl-incf (car bounds))
    (cl-decf (cdr bounds))
    thing/bounds))

(defun things--get-inside (thing/bounds)
  "Shrink the bounds in THING/BOUNDS to exclude whitespace and then newlines."
  (let ((bounds (cdr thing/bounds)))
    (goto-char (car bounds))
    (skip-chars-forward " \t")
    (skip-chars-forward "\n")
    (setf (car bounds) (point))
    (goto-char (cdr bounds))
    (skip-chars-backward " \t")
    (skip-chars-backward "\n")
    (setf (cdr bounds) (point))
    thing/bounds))

(defun things--get-around (thing/bounds)
  "Grow the bounds in THING/BOUNDS to include whitespace after or before it."
  (let ((bounds (cdr thing/bounds)))
    (goto-char (cdr bounds))
    (skip-chars-forward " \t")
    (cond ((= (point) (cdr bounds))
           (goto-char (car bounds))
           (skip-chars-backward " \t")
           (setf (car bounds) (point)))
          (t
           (setf (cdr bounds) (point))))
    thing/bounds))

(defun things-get (adjustment thing/bounds)
  "Return the result of calling the specified ADJUSTMENT on THING/BOUNDS.
If the thing does not have a corresponding ADJUSTMENT function defined, fallback
to the default one if it exists. If there is no available function, just return
THING/BOUNDS."
  (let ((adjust-function (or (get (intern (format "things-get-%s" adjustment))
                                  (car thing/bounds))
                             (intern (format "things--get-%s" adjustment)))))
    (if (functionp adjust-function)
        (funcall adjust-function thing/bounds)
      thing/bounds)))

;; * Pairs
;; * Quotes
;; * Separators
;; * Evil Integration
(declare-function evil-range "evil-common")
(defun things-evil-range (thing/bounds type)
  "Call `evil-range' with the bounds from THING/BOUNDS and TYPE."
  (let ((bounds (cdr thing/bounds)))
    (evil-range (car bounds)
                (cdr bounds)
                type
                :expanded t)))

(provide 'things)
;;; things.el ends here