;;; htab.el --- Use helm to switch tabs. -*- lexical-binding: t -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;; Author: Herbert Jones <jones.herbert@gmail.com>
;; Version: 0.0.0
;; Package-Requires: ((emacs "27.0") (cl-lib "1.0"))
;; Keywords: tabs

;;; Commentary:
;; Use HTAB-PICK-BY-NAME to switch between tabs.  Add a new tab by entering a
;; new name for the tab.  If HTAB-CREATE-PREDEFINED-LAYOUT has been used to
;; create a predefined tab layout, it can be selected.
;;
;; Recommended tab setup:
;;
;; ;; Give default tab a name.
;; (tab-bar-rename-tab "default")
;;
;; (customize-set-variable 'tab-bar-show nil)
;;
;; ;; New tab setup
;; (customize-set-variable 'tab-bar-new-tab-choice
;;                         (lambda () (get-buffer "*scratch*")))
;;
;; (customize-set-variable 'tab-bar-close-button-show nil)
;;
;; TODO: Should be able to use C-SPC to select multiple tabs and delete them all
;; at once.
;;
;; TODO: Helm may need to tag popup to prevent it being activated in other
;; contexts.

;;; Code:
(require 'dash)
(require 'cl-lib)
(require 'helm)
(require 'ring)

(defvar htab-predefined-layouts '()
  "List of predefined tabs.  Set via HTAB-CREATE-PREDEFINED-LAYOUT.")

(defvar htab-last-picked-tabs-max-length 10
  "How long HTAB-LAST-PICKED-TABS can get.")

(defvar htab-last-picked-tabs (make-ring htab-last-picked-tabs-max-length)
  "List of tab names last used.")

(defun htab--define-layout-inner (name f)
  "Add layout config to htab-predefined-layouts"
  (setq htab-predefined-layouts
        (cl-delete-if (lambda (a) (string-equal (car a) name))
                      htab-predefined-layouts))
  (push (cl-list* name (cons name f)) htab-predefined-layouts)
  (setq htab-predefined-layouts
        (sort htab-predefined-layouts
              (lambda (a b) (string< (car a) (car b))))))

(defmacro htab-create-predefined-layout (name &rest body)
  "Add layout config to htab-predefined-layouts.

Example:
    (htab-create-predefined-layout \"tasks\"
        (let ((first-window (get-buffer-window)))
          (find-file \"~/Documents/org/time-sheet.org\")
          (split-window-below)
          (windmove-down)

          (find-file \"~/Documents/org/tasks.org\")
          (split-window-right)
          (windmove-right)

          (find-file \"~/Documents/journal\")
          (select-window first-window)))
"
  (declare (indent defun))
  `(htab--define-layout-inner ,name (lambda () ,@body)))

(defun htab--make-tab-f (name setup-actions)
  "Select an existing tab, or create one and configure it."
  (cond
   ((htab--tab-name-exists-p name)
    (tab-bar-switch-to-tab name))
   (t
    (tab-bar-new-tab)
    (tab-bar-rename-tab name)
    (funcall setup-actions))))

(defun htab--tab-name-exists-p (name)
  (let ((tabs (funcall tab-bar-tabs-function)))
    (and (--find (equal name (cdr (assq 'name it))) tabs) t)))

(defun htab--tab-name ()
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-tab (assq 'current-tab tabs)))
    (or (cdr (assq 'name current-tab)) "default")))

(defun htab--tab-names ()
  (let ((tabs (funcall tab-bar-tabs-function)))
    (--map (cdr (assq 'name it)) tabs)))

(defun htab--tab-update-last-picked (name)
  (ring-remove+insert+extend htab-last-picked-tabs name))

(defface htab-helm-current-tab-face
  '((((background light)) :foreground "gray30")
    (((background dark))  :foreground "gray50"))
  "Current tab")

(defun htab--tab-pick-close-tab (candidate)
  (tab-bar-close-tab (1+ (cdr (assq 'pos candidate)))))

(defun htab--tab-pick-run-close-tab ()
  (interactive)
  (with-helm-alive-p (helm-exit-and-execute-action 'htab--tab-pick-close-tab)))

(defun htab--tab-pick-rename-tab (candidate)
  (when-let ((new-name (read-string "New name: "
                                    (cdr (assq 'name candidate)))))
    (htab--tab-update-last-picked new-name)
    (tab-bar-rename-tab new-name (1+ (cdr (assq 'pos candidate))))))

(defun htab--tab-pick-run-rename-tab ()
  (interactive)
  (with-helm-alive-p (helm-exit-and-execute-action 'htab--tab-pick-rename-tab)))

(defun htab--order-assoc-by-last-picked (items)
  "Sort ITEMS by matching them up with HTAB-LAST-PICKED-TABS.

Any items that weren't sorted will be appended to returned list in current order."
  (let ((culled-items (append items))
        (ordered-items '()))

    (loop for match-item-idx below (ring-length htab-last-picked-tabs)
          for match-item = (ring-ref htab-last-picked-tabs match-item-idx)
          for idx = (--find-index (string= (car it) match-item) culled-items)
          when idx do (progn (push (elt culled-items idx) ordered-items)
                             (setf culled-items (-remove-at idx culled-items))))
    (append (nreverse ordered-items) culled-items)))

;;;###autoload
(defun htab-pick-by-name ()
  "Switch to a new or existing tab."
  (interactive)
  (let* ((tabs (funcall tab-bar-tabs-function))
         (tab-info-by-name (loop for pos from 0
                                 for tab in tabs
                                 collect (list (cdr (assq 'name tab))
                                               (cons 'name (cdr (assq 'name tab)))
                                               (cons 'pos pos)
                                               (cons 'current (eq 'current-tab (car tab))))))
         (current-tab (--first (cdr (assq 'current it)) tab-info-by-name))
         (current-tab-name (cdr (assq 'name current-tab)))
         (ordered-tab-info-by-name (htab--order-assoc-by-last-picked tab-info-by-name))
         ;; Reorder with current at top
         (ordered-tab-info-by-name
          (cons (cons (propertize current-tab-name 'face 'htab-helm-current-tab-face)
                      (cdr current-tab))
                (--remove (cdr (assq 'current it)) ordered-tab-info-by-name)))
         (tab-source
          (helm-build-sync-source "Tabs"
            :candidates ordered-tab-info-by-name
            :keymap (let ((map (make-sparse-keymap)))
                      (set-keymap-parent map helm-map)
                      (define-key map (kbd "C-d") 'htab--tab-pick-run-close-tab)
                      (define-key map (kbd "C-r") 'htab--tab-pick-run-rename-tab)
                      map)
            :action
            `(("Switch to tab" .
               (lambda (candidate)
                 (let ((name (cdr (assq 'name candidate))))
                   (htab--tab-update-last-picked (htab--tab-name))
                   (htab--tab-update-last-picked name)
                   (tab-bar-select-tab (1+ (cdr (assq 'pos candidate)))))))
              ("Close tab (C-d)" . htab--tab-pick-close-tab)
              ("Rename tab (C-r)" . htab--tab-pick-rename-tab))))
         (layouts-source (helm-build-sync-source "Layouts"
                           :candidates (let ((names (htab--tab-names)))
                                         (--filter (not (member (car it) names))
                                                   htab-predefined-layouts))
                           :action
                           `(("Load layout" .
                              (lambda (candidate)
                                (htab--make-tab-f (car candidate) (cdr candidate)))))))
         (new-tab-source
          (helm-build-dummy-source "New tab"
            :filtered-candidate-transformer
            (lambda (_candidates _source)
              (list (or (and (not (string= helm-pattern ""))
                             helm-pattern)
                        (propertize "Enter a new tab name"
                                    'face 'helm-action))))
            :action '(("New tab" . (lambda (name)
                                     (message "Making a new tab with name %S" name)
                                     (tab-bar-new-tab)
                                     (tab-bar-rename-tab name)
                                     (htab--tab-update-last-picked name)))))))
    (helm :sources (list tab-source layouts-source new-tab-source)
          :buffer "*helm htab*"
          :preselect (cdr (assq 'name (cadr ordered-tab-info-by-name)))
          :prompt "Switch to: ")))

(provide 'htab)

;;; htab.el ends here
