;;; things-evil.el --- Evil integration for things.el  -*- lexical-binding: t; -*-

;; URL: https://github.com/noctuid/things.el
;; Keywords: evil text-object convenience
;; Package-Requires: ((things "0.1") (cl-lib "0.5") (evil "1.1.0"))
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
;; This package is like a combination of the targets, TextObjectify, anyblock,
;; and expand-region vim plugins.

;; For more information see the README in the github repo.

;;; Code:
;; TODO have not added/decided to add the following targets features (need to
;; decide on best way to implement these and if they should be)
;; ("User-customizable Functions" heading):
;; - customizable push-jump-p
;; - customizable bound function
;; - :let keyword (`targets-settings-alist' equivalent?)
;; TODO I and A handling with visual block (but in key-agnostic way)
;; TODO allow disabling forward, backward, or all seeking for regular TOs
;; TODO use avy-with and add avy settings (see `targets--avy-seek')
;; TODO support all text objects from targets
;; TODO gensyms/once-only and debug declarations

(require 'things)
(require 'cl-lib)
(require 'evil)

;; * Settings
(defgroup things-evil nil
  "Provides extensions to evil's text objects."
  :group 'evil
  :prefix "things-evil-")

(defcustom things-evil-default-text-object nil
  "The default text object to use for `things-evil-last-text-object'.
This is used whenever there is no last text object stored for the current
state (operator or visual). Note that the last text object for visual state is
cleared after exiting visual state."
  :group 'things-evil
  :type '(choice function (const nil)))

(defvar things-evil--last-visual-text-object
  "Holds the last text object used in visual state.")

(defvar things-evil--last-operator-text-object
  "Holds the last text object used in operator state.")

;; * Jump List Updating
(defvar things-evil--push-jump nil)

(defun things-evil-push-jump-p (old-pos new-pos)
  "Whether OLD-POS is far enough away from NEW-POS to push it to the jump list.
This function is called after an evil operator has finished (and after any
possible point resetting). When this function returns non-nil (and there is no
visual selection), targets will push the old position to the jump list. This
default function will return non-nil when the positions are not on the same
line."
  ;; A custom function can be used by changing the `things-evil-push-jump-p'
  ;; variable.
  (not (= (line-number-at-pos old-pos) (line-number-at-pos new-pos))))

(defun things-evil--update-jump-list ()
  "Called after any things-evil text object to update the evil jump list.
The old position is not added to the jump list if there is a selection or if the
new point is not far enough away from the original point as determined by
`things-push-jump-p'."
  (when (and things-evil--push-jump
             (funcall #'things-evil-push-jump-p things-evil--push-jump (point)))
    ;; `evil-set-jump' already checks if region is active
    (evil-set-jump things-evil--push-jump))
  (setq things-evil--push-jump nil))

;; * Position/Window Resetting
(defvar things-evil--reset-position nil)
(defvar things-evil--reset-window nil)

(defvar things-evil-no-reset-operators nil
  "A list corresponding to evil operators that should move the point.
Things will not reset the point after next, last, or remote text object when
the operator is in this list (or in `evil-change-commands').")

;; TODO optionally reset after change operator when entering normal state
;; see https://github.com/noctuid/targets.el/issues/26
(defun things-evil--reset-after ()
  "Save the point and window so they can be restored after the current command.
Do save/reset the point/window if the current evil operator is in
`evil-change-commands' or `things-evil-no-reset-operators'."
  (unless (or (memq evil-this-operator evil-change-commands)
              (memq evil-this-operator things-evil-no-reset-operators))
    (point-to-register 'things-evil--reset-position)
    (setq things-evil--reset-position t
          things-evil--reset-window (get-buffer-window))))

(defun things--reset-position ()
  "Reset the point and window to the previously store point and window.
The point and window are not restored if there is a selection or if there is no
stored point/window."
  (unless (region-active-p)
    (when things-evil--reset-position
      (jump-to-register 'things-evil--reset-position))
    (when things-evil--reset-window
      (let* ((window things-evil--reset-window)
             (frame (window-frame window)))
        (unless (equal frame (selected-frame))
          (select-frame-set-input-focus frame))
        (select-window window))))
  (setq things-evil--reset-window nil)
  (setq things-evil--reset-position nil))

;; * Post Command
(defun things-evil--post-command ()
  "When necessary, update the jump list and/or restore the point/window.
Position resetting happens first in order to prevent unwanted positions from
being added to the jump list when using operators with next/last/remote text
objects. This function is meant to be added to `post-command-hook'."
  (things-evil--reset-position)
  (things-evil--update-jump-list))

;; * "Last" Text Object
(defun things-evil--set-last-text-object (to)
  "Helper to set the last text object to TO."
  (if (evil-visual-state-p)
      (setq things-evil--last-visual-text-object to)
    (setq things-evil--last-operator-text-object to)))

(defun things-evil--clear-last-visual-text-object ()
  "Helper to clear `things-evil--last-visual-text-object'."
  (setq things-evil--last-visual-text-object nil))

;;;###autoload
(defun things-evil-last-text-object ()
  "Run the last text object or fall back to `things-evil-default-text-object'."
  (interactive)
  (let ((to (or (if (evil-visual-state-p)
                    things-evil--last-visual-text-object
                  things-evil--last-operator-text-object)
                things-evil-default-text-object)))
    (when to
      ;; (targets--let targets-last-text-object nil
      ;;   ((call-interactively to)))
      (call-interactively to))))

;; * Existing Evil Thing Integration
;; ** Word/WORD
(put 'things-evil-word 'forward-op #'forward-evil-word)
(put 'things-evil-WORD 'forward-op #'forward-evil-WORD)

;; ** Symbol
(put 'things-evil-symbol 'forward-op #'forward-evil-symbol)

;; ** Sentence
(put 'things-evil-sentence 'forward-op #'forward-evil-sentence)

;; ** Paragraph
(put 'things-evil-paragraph 'forward-op #'forward-evil-paragraph)

;; * Text Object Definition/Binding
(defun things-evil-range (thing/bounds type)
  "Call `evil-range' with the bounds from THING/BOUNDS and TYPE."
  (when thing/bounds
    (let ((bounds (cdr thing/bounds)))
      (evil-range (car bounds)
                  (cdr bounds)
                  type
                  :expanded t))))

(defmacro things-evil--define-text-object (name docstring &rest body)
  "A wrapper for `evil-define-text-object'.
NAME and DOCSTRING are the name and docstring for the text object. The last
things-evil text object will be set to NAME. BODY is functions to run; the last
should return an evil range. Return the function that is created (eventually
expands to `evil-define-command', which returns the function)."
  (declare (indent defun))
  `(evil-define-text-object ,name (count &optional beg end type)
     ,docstring
     (things-evil--set-last-text-object #',name)
     (setq things--push-jump (point-marker))
     ,@body))

(defun things-evil--maybe-define-keys (hooks prefix infix keys def)
  "Helper function used to bind keys.
When HOOKS is non-nil, add a function to each hook to bind the keys locally.
Otherwise bind the keys in the global visual and operator state maps. The keys
will be constructed by combining PREFIX, INFIX, and entries in KEYS. If PREFIX
or INFIX is nil, no keybindings will be made. If INFIX is non-nil but not a
string, it will be excluded. All keys will be bound to DEF."
  (when (stringp keys)
    (setq keys (list keys)))
  (when (and prefix infix)
    (while keys
      (let ((key (concat prefix
                         (if (stringp infix)
                             infix
                           nil)
                         (pop keys))))
        (if hooks
            (dolist (hook hooks)
              (add-hook
               hook
               `(lambda ()
                  (define-key evil-visual-state-local-map ,key #',def)
                  (define-key evil-operator-state-local-map ,key #',def))
               t))
          (define-key evil-operator-state-map key def)
          (define-key evil-visual-state-map key def))))))

;; TODO refactor to reduce code duplication
(cl-defmacro things-evil--define-regular-text-object (name adjustment things
                                                           &key (prefix "things-evil-"))
  ""
  (let ((full-name (intern (format "%s%s-%s" prefix adjustment name)))
        (docstring (format "Select %s %s." adjustment things))
        (adjusted-things (things-apply-adjustment things adjustment)))
    `(things-evil--define-text-object ,full-name
       ,docstring
       (things-evil-range
        (things-growing-or-seeking-bounds ',adjusted-things
                                          count
                                          (when (evil-visual-state-p)
                                            (evil-visual-expand-region)
                                            (car (region-bounds))))
        type))))

(cl-defmacro things-evil--define-next-text-object (name adjustment things
                                                        &key (prefix "things-evil-"))
  ""
  (let ((full-name (intern (format "%s%s-next-%s" prefix adjustment name)))
        (docstring (format "Select next %s %s." adjustment things))
        (adjusted-things (things-apply-adjustment things adjustment)))
    `(things-evil--define-text-object ,full-name
       ,docstring
       (things-evil--reset-after)
       (things-evil-range
        (things-next-bounds ',adjusted-things count)
        type))))

(cl-defmacro things-evil--define-last-text-object (name adjustment things
                                                        &key (prefix "things-evil-"))
  ""
  (let ((full-name (intern (format "%s%s-previous-%s" prefix adjustment name)))
        (docstring (format "Select previous %s %s." adjustment things))
        (adjusted-things (things-apply-adjustment things adjustment)))
    `(things-evil--define-text-object ,full-name
       ,docstring
       (things-evil--reset-after)
       (things-evil-range
        (things-previous-bounds ',adjusted-things count)
        type))))

;; TODO may need to bring back some previous code that prevented hanging
;; (doesn't seem necessary currently)
(cl-defmacro things-evil--define-remote-text-object (name adjustment things
                                                          &key (prefix "things-evil-"))
  ""
  (let ((full-name (intern (format "%s%s-remote-%s" prefix adjustment name)))
        (docstring (format "Select %s %s remotely with avy." adjustment things))
        (adjusted-things (things-apply-adjustment things adjustment)))
    `(things-evil--define-text-object ,full-name
       ,docstring
       (things-evil--reset-after)
       ;; set repeat keys
       (when (evil-repeat-recording-p)
         (setq
          evil-repeat-info
          `(((lambda ()
               (setq prefix-arg ,current-prefix-arg)
               (setq unread-command-events
                     ',(listify-key-sequence (this-command-keys)))
               (call-interactively #',evil-this-operator)))))
         (evil-repeat-stop))
       (things-evil-range
        (things-remote-bounds ',adjusted-things)
        type))))

(cl-defmacro things-evil-define (name things &key
                                      (name-prefix "things-evil-")
                                      (inner-key "i")
                                      (a-key "a")
                                      (inside-key "I")
                                      (around-key "A")
                                      (next-key "n")
                                      (last-key "l")
                                      (remote-key "r")
                                      keys
                                      hooks)
  `(progn
     ,@(cl-loop for (adjustment prefix-key) in `((inner ,inner-key)
                                                 (a ,a-key)
                                                 (inside ,inside-key)
                                                 (around ,around-key))
                collect
                `(progn
                   (things-evil--maybe-define-keys
                    ,hooks ,prefix-key t ,keys
                    (things-evil--define-regular-text-object ,name ,adjustment ,things))
                   (things-evil--maybe-define-keys
                    ,hooks ,prefix-key ,next-key ,keys
                    (things-evil--define-next-text-object ,name ,adjustment ,things))
                   (things-evil--maybe-define-keys
                    ,hooks ,prefix-key ,last-key ,keys
                    (things-evil--define-last-text-object ,name ,adjustment ,things))
                   (things-evil--maybe-define-keys
                    ,hooks ,prefix-key ,remote-key ,keys
                    (things-evil--define-remote-text-object ,name ,adjustment ,things))))))

;; TODO way to simultaneously create thing (would require `things-evil-define'
;; to be a function):
;; (things-evil-define 'paren (things-define-pair 'things-paren "(" ")"))
;; TODO consistent between things.el and things-evil.el on prefix handling
;; (currently `things-define-pair 'and similar functions do not add any prefix
;; implicitly)

;; * TODO Setup

(provide 'things-evil)
;;; things-evil.el ends here
