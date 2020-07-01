;;; htab.el --- Use helm to switch tabs. -*- lexical-binding: t -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

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
(require 'desktop)

(defvar htab-predefined-layouts '()
  "List of predefined tabs.  Set via HTAB-CREATE-PREDEFINED-LAYOUT.")

(defvar htab-last-picked-tabs-max-length 10
  "How long HTAB-LAST-PICKED-TABS can get.")

(defvar htab-last-picked-tabs (make-ring htab-last-picked-tabs-max-length)
  "List of tab names last used.")

(defvar htab--id-length 8
  "Size of random ids used to uniquely identify each tab.")

(defvar htab--tab-local-storage '()
  "Each tab can have its own local storage.")
(add-to-list 'desktop-globals-to-save 'htab--tab-local-storage)

(defun htab--define-layout-inner (name f)
  "Add layout config to htab-predefined-layouts.

NAME to uniquely describe the layout.  Function F called to setup layout."
  (setq htab-predefined-layouts
        (cl-delete-if (lambda (a) (string-equal (car a) name))
                      htab-predefined-layouts))
  (push (cl-list* name (cons name f)) htab-predefined-layouts)
  (setq htab-predefined-layouts
        (sort htab-predefined-layouts
              (lambda (a b) (string< (car a) (car b))))))

(defmacro htab-create-predefined-layout (name &rest body)
  "Add layout config to htab-predefined-layouts.

NAME to uniquely describe the layout.  The BODY is executed to setup the layout.

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
          (select-window first-window)))"
  (declare (indent defun))
  `(htab--define-layout-inner ,name (lambda () ,@body)))

(defun htab--make-tab-f (name setup-actions)
  "Select an existing tab, or create one and configure it.

Provide the NAME of the new tab and SETUP-ACTIONS to setup the
layout once in the new tab."
  (let ((existing-tab-id (htab--tab-id-from-name name)))
    (cond (existing-tab-id
           (tab-bar-switch-to-tab name))
          (t
           (tab-bar-new-tab)
           (htab-local-set 'name name)
           (funcall setup-actions)))))

(defun htab--tab-id-from-name (name)
  "Get tab id from tab name.

Provides the unique ID from the NAME of the tab."
  (when-let* ((tabs (funcall tab-bar-tabs-function))
              (tab-with-name (--find (when-let ((id (cdr (assq 'name it))))
                                       (equal name (htab--local-get-by-id id 'name)))
                                     (funcall tab-bar-tabs-function))))
    (cdr (assq 'name tab-with-name))))

(defun htab-tab-name ()
  "Get the name of the current tab."
  (or (htab-local-get 'name) "default"))

(defun htab--tab-names ()
  "Get a list of the names of the all tabs."
  (let ((tabs (funcall tab-bar-tabs-function)))
    (--map
     (let* ((id (cdr (assq 'name it))))
       (or (htab--local-get-by-id id 'name) (format "unknown-%s" id)))
     tabs)))

(defun htab--tab-update-last-picked (name)
  "Track the last tab by NAME."
  (ring-remove+insert+extend htab-last-picked-tabs name))

(defface htab-helm-current-tab-face
  '((((background light)) :foreground "gray30")
    (((background dark))  :foreground "gray50"))
  "Current tab")

(defun htab--tab-pick-close-tab (candidate)
  "Close tab handler for HTAB-PICK-BY-NAME.

CANDIDATE is selected alist from HTAB-PICK-BY-NAME."
  (tab-bar-close-tab (1+ (cdr (assq 'pos candidate))))
  (setf (alist-get (cdr (assq 'id candidate))
                   htab--tab-local-storage
                   nil 'remove #'equal)
        nil))

(defun htab--tab-pick-run-close-tab ()
  "Interactive close tab handler for HTAB-PICK-BY-NAME."
  (interactive)
  (with-helm-alive-p (helm-exit-and-execute-action 'htab--tab-pick-close-tab)))

(defun htab--tab-pick-rename-tab (candidate)
  "Rename tab handler for HTAB-PICK-BY-NAME.

CANDIDATE is selected alist from HTAB-PICK-BY-NAME."
  (when-let ((new-name (read-string "New name: "
                                    (cdr (assq 'name candidate)))))
    (htab--tab-update-last-picked new-name)
    (htab--local-set-by-id (cdr (assq 'id candidate)) 'name new-name)))

(defun htab--tab-pick-run-rename-tab ()
  "Interactive rename tab handler for HTAB-PICK-BY-NAME."
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
                                 for id = (cdr (assq 'name tab))
                                 for name = (or (htab--local-get-by-id id 'name)
                                                (format "unknown-%s" id))
                                 collect (list name
                                               (cons 'name name)
                                               (cons 'id id)
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
                   (htab--tab-update-last-picked (htab-tab-name))
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
            :action '(("New tab - blank"
                       . (lambda (name)
                           (let ((tab-bar-new-tab-choice "*scratch*"))
                             (htab--create-new-tab name))))
                      ("New tab from current buffer (C-b)"
                       . (lambda (name)
                           (let ((tab-bar-new-tab-choice t))
                             (htab--create-new-tab name))))
                      ("New tab - clone (C-l)"
                       . (lambda (name)
                           (let ((tab-bar-new-tab-choice nil))
                             (htab--create-new-tab name)))))
            :keymap (let ((map (make-sparse-keymap)))
                      (set-keymap-parent map helm-map)
                      (define-key map (kbd "C-b")
                        (lambda ()
                          (interactive)
                          (with-helm-alive-p
                            (helm-exit-and-execute-action
                             (lambda (name)
                               (let ((tab-bar-new-tab-choice t))
                                 (htab--create-new-tab name)))))))
                      (define-key map (kbd "C-l")
                        (lambda ()
                          (interactive)
                          (with-helm-alive-p
                            (helm-exit-and-execute-action
                             (lambda (name)
                               (let ((tab-bar-new-tab-choice nil))
                                 (htab--create-new-tab name)))))))
                      map))))
    (helm :sources (list tab-source layouts-source new-tab-source)
          :buffer "*helm htab*"
          :preselect (or (and (cadr ordered-tab-info-by-name)
                              (rx-to-string `(: bol ,(cdr (assq 'name (cadr ordered-tab-info-by-name))) eol)))
                         "default")
          :prompt "Switch to: ")))

(defun htab--create-new-tab (name)
  "Create tab handler for HTAB-PICK-BY-NAME.

NAME is name of new tab."
  (message "Making a new tab with name %S" name)
  (tab-bar-new-tab)
  (htab-local-set 'name name)
  (htab--tab-update-last-picked name))

(defun htab--random-from (chars size)
  "Create a random string selected from CHARS of length SIZE."
  (let ((chars-len (length chars))
        (result '()))
    (loop for i from 1 to size
          do (push (elt chars (random chars-len)) result))
    (apply #'string result)))

(defun htab--random-id ()
  "Create a new random id for new tabs."
  (htab--random-from
   "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
   htab--id-length))

(defun htab--get-tab-local-storage (id)
  "Get the tab local variable object for the tab identified by ID."
  (alist-get id htab--tab-local-storage nil nil #'equal))

(defun htab--current-tab-local-storage ()
  "Get the tab local variable object for the current tab."
  (let* ((id (htab-current-tab-id)))
    (htab--get-tab-local-storage id)))

(defun htab-local-get (var-id &optional default-value)
  "Get a tab local variable named VAR-ID for current tab.

If variable is not set, return DEFAULT-VALUE."
  (alist-get var-id (htab--current-tab-local-storage) default-value))

(defun htab--local-get-by-id (tab-id var-id &optional default-value)
  "Get a tab local variable named VAR-ID for tab identified by TAB-ID.

If variable is not set, return DEFAULT-VALUE."
  (let ((store (htab--get-tab-local-storage tab-id)))
    (if store
        (alist-get var-id store default-value)
      default-value)))

(defun htab-local-set (var-id new-value)
  "Set NEW-VALUE on the current tab local variable named VAR-ID."
  (htab--local-set-by-id (htab-current-tab-id) var-id new-value))

(defun htab--local-set-by-id (tab-id var-id new-value)
  "Set NEW-VALUE on the tab local variable named VAR-ID for tab identified by TAB-ID."
  (let ((tab-alist (htab--get-tab-local-storage tab-id)))
    ;; Update tab local
    (setf (alist-get var-id tab-alist nil 'remove) new-value)
    ;; Update global
    (setf (alist-get tab-id htab--tab-local-storage nil nil #'equal) tab-alist)))

(defun htab-current-tab-id ()
  "Get the current tab's ID."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-tab (assq 'current-tab tabs))
         (explicit-name (cdr (assq 'explicit-name current-tab)))
         (id (cdr (assq 'name current-tab))))
    (unless (and explicit-name id)
      (setq id (htab--random-id))
      (tab-bar-rename-tab id))
    id))

(provide 'htab)

;;; htab.el ends here
