;;; ee-info.el --- browse Info documentation

;; Copyright (C) 2002, 2003  Juri Linkov <juri@jurta.org>

;; Author: Juri Linkov <juri@jurta.org>
;; Keywords: ee, help

;; This file is [not yet] part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; See the file README and documentation for more information.

;; This file contains two main parts: one for ee-info-dir and one for ee-info:
;; ee-info-dir has unique index buffer created from dir,
;; ee-info has own buffer for index of every visited info file.

;; TODO: look in the texinfo.info for all possible node types:
;;   1. The menu entry name (optional).
;;   2. The name of the node (required).
;;   3. A description of the item (optional).
;;   The template for a menu entry looks like this:
;;      * MENU-ENTRY-NAME: NODE-NAME.   DESCRIPTION

;; TODO: Make clear why there are three types of menu links:
;; * Introduction::            Introduction and conventions used.
;; * Standards: Coding Conventions.    Coding conventions for Emacs Lisp.
;; * add-to-list:                           Setting Variables.

;; TODO: Make clear why there are four names of the same node,
;; e.g. in the next three lines from different parts of Info:
;; * Standards: Coding Conventions.    Coding conventions for Emacs Lisp.
;; Emacs Lisp Coding Conventions
;; =============================
;; there are next topis names:
;; 1. "Standards"
;; 2. "Coding Conventions"
;; 3. "Coding conventions for Emacs Lisp"
;; 4. "Emacs Lisp Coding Conventions"

;; Rough correspondence to HTML:
;; <DT><A HREF="Coding Conventions">Standards</A><DD>Coding conventions for Emacs Lisp.
;; <TITLE>Emacs Lisp Coding Conventions</TITLE>

;;; Code:

(require 'ee)

(eval-when-compile
  (require 'info))

;;; Constants

(defconst ee-info-dir-mode-name "ee-info"
  ;; Name "ee-info" (the same as next variable) is better than "ee-info-dir"
  "*info-dir mode name.")

(defconst ee-info-mode-name "ee-info"
  "*info mode name.")

;;; Customizable Variables

(defgroup ee-info nil
  "Browse Info documentation."
  :prefix "ee-info-"
  :group 'ee)

(defcustom ee-info-dir-read-section-names nil
  "*Non-nil means to extract section names (defined by `INFO-DIR-SECTION')
from all info files accessible from dir."
  :type 'file
  :group 'ee-info)

(defcustom ee-info-data-file-name-format "info-%s.ee"
  "*Format used to create data file name to save data collected from info files.
Format may contain %s which will be replaced by info file name."
  :type 'file
  :group 'ee-info)

;;; Local Variables

(defvar ee-info-file nil
  "Current info file name.")

(defvar ee-info-section-names nil
  "Global variable to hold association list during data collection.
The elements of list are (\"section name\" . \"node name\").")

;;; Info dir

;;; Data Description

(defvar ee-info-dir-data
  '[(meta
     (format-version . "0.0.1")
     (view-data-file . "view/info-dir.ee")
     (data-file . "info-dir.ee")
     (collector . ee-info-dir-data-collect)
     (fields title filename nodename dir-section info-dir-section ())
     (key-fields filename))])

;;; Data Extraction

;; TODO: refresh data when DIR is updated, i.e. Info-dir-contents
;; (or it's length or checksum) is not the same as prev value
(defun ee-info-dir-data-collect (data)
  (let ((new-data
         (ee-data-convert-lists-to-vectors
          (ee-info-dir-read-directory-node
           (ee-data-field-names data)))))
    (aset new-data 0 (aref data 0))
    new-data))

(defun ee-info-dir-read-directory-node (field-names)
  "
Output: [[title filename nodename dir-section info-dir-section] ...]
"
  ;; TODO: save in file with time-stamp, and update if date of one of
  ;; dir files is newer (this functionality is in (Info-insert-dir),
  ;; so compare string returned by (Info-insert-dir)?)
  (with-temp-buffer
    (Info-insert-dir)
    (goto-char (point-min))
    (if (search-forward "\n* Menu:" nil t)
        (let (res section)
          (forward-line 1)
          (while (not (eobp))
            (beginning-of-line)
            (cond
             ;; Menu line
             ((looking-at "^\\* +\\([^:\t\n]*\\):")
              (let* ((title (match-string-no-properties 1))
                     (nodename (Info-extract-menu-node-name))
                     filename)
                (string-match "\\s *\\((\\s *\\([^\t)]*\\)\\s *)\\s *\\|\\)\\(.*\\)"
                              nodename)
                (setq filename (if (= (match-beginning 1) (match-end 1))
                                   ""
                                 (substring nodename (match-beginning 2) (match-end 2)))
                      nodename (substring nodename (match-beginning 3) (match-end 3)))
                (let ((trim (string-match "\\s *\\'" filename)))
                  (if trim (setq filename (substring filename 0 trim))))
                (let ((trim (string-match "\\s *\\'" nodename)))
                  (if trim (setq nodename (substring nodename 0 trim))))
                (setq res
                      (cons
                       (mapcar
                        (lambda (field-name)
                          (cond
                           ((eq field-name 'title) title)
                           ((eq field-name 'filename) (if (equal filename "") nil filename))
                           ((eq field-name 'nodename) (if (equal nodename "") "Top" nodename))
                           ((eq field-name 'dir-section) section) ; section name from dir
                           ((eq field-name 'info-dir-section) nil) ; place for INFO-DIR-SECTION names
                           ;; TODO: extract and add the titles
                           ))
                        field-names)
                       res))))
             ;; Other non-empty strings in the dir buffer are section names
             ((looking-at "^\\([^ \t\n][^:\n]*\\)")
              (setq section (match-string-no-properties 1))))
            (forward-line 1))
          ;; Read section names from INFO-DIR-SECTION from info files
          (if ee-info-dir-read-section-names ;;TODO: move to data-fields of view-descr
              (save-excursion
                (setq res (mapcar
                           (lambda (r)
                             (when (equal (aref r 2) "Top")
                               (Info-find-node (aref r 1) "Top")
                               (widen)
                               (goto-char (point-min))
                               (let ((sections))
                                 (while (re-search-forward "\nINFO-DIR-SECTION +\\([^\n]+\\)" nil t)
                                   (setq sections (cons (match-string-no-properties 1) sections)))
                                 (and sections (aset r 4 (nreverse sections)))))
                             r)
                           res))))
          (nreverse res)))))

;;; Actions

(defun ee-info-dir-ee-info (&optional arg)
  (interactive)
  (let ((filename (ee-field 'filename)))
    (and filename (ee-info filename))))

(defun ee-info-dir-find-node (&optional arg)
  (let* ((r (ee-view-record-get))
         (filename (ee-field 'filename))
         (nodename (ee-field 'nodename)))
    (when r
      ;; Pop to *info* to save previous node into Info-history
      (pop-to-buffer "*info*")
      (Info-find-node filename nodename))))

;;; Key Bindings

(defvar ee-info-dir-keymap nil
  "Local keymap for ee-info-dir-mode info-dir.")

;; TODO: move keybindings to defvar view/info-dir.ee?
(defun ee-info-dir-keymap-make-default ()
  "Defines default key bindings for `ee-info-dir-keymap'.
It inherits key bindings from `ee-mode-map'."
  (or ee-mode-map
      (ee-mode-map-make-default))
  (let ((map (copy-keymap ee-mode-map)))
    (define-key map "\C-o" 'ee-info-dir-find-node)
    (setq ee-info-dir-keymap map)))

(or ee-info-dir-keymap
    (ee-info-dir-keymap-make-default))

;;; Info files

;;; Data Description

(defvar ee-info-data
  '[(meta
     ;; (data-file . "info-%s.ee")
     (format-version . "0.0.1")
     (view-data-file . "view/info.ee")
     (collector . ee-info-data-collect)
     (fields nodename category menulist indexlist ())
     (key-fields nodename))])

;;; Data Extraction

(defun ee-info-data-collect (data)
  (let ((new-data (ee-data-convert-lists-to-vectors
                   (ee-info-data-collect-menus
                    (ee-data-field-names data)
                    ee-info-file))))
    (aset new-data 0 (aref data 0))
    new-data))

(defun ee-info-data-collect-menus (field-names filename)
  (save-excursion
    (let ((res))
      (Info-find-node filename "Top")
      (setq ee-info-section-names '(("Top" "Top")))
      ;; Read menus from info file of Top node
      (setq res (ee-info-data-collect-menus-current field-names))
      ;; Read menus from info subfiles
      (let ((list ())
            (osubfile ))
        ;; Get list of subfiles (code borrowed from `Info-search')
        (save-excursion
          (set-buffer (marker-buffer Info-tag-table-marker))
          (goto-char (point-min))
          (if (search-forward "\n\^_\nIndirect:" nil t)
              (save-restriction
                (narrow-to-region (point)
                                  (progn (search-forward "\n\^_")
                                         (1- (point))))
                (goto-char (point-min))
                (beginning-of-line)
                (while (not (eobp))
                  (re-search-forward "\\(^.*\\): [0-9]+$")
                  (goto-char (+ (match-end 1) 2))
                  (setq list (cons (cons (read (current-buffer))
                                         (buffer-substring-no-properties
                                          (match-beginning 1) (match-end 1)))
                                   list))
                  (goto-char (1+ (match-end 0))))
                (setq list (nreverse list)
                      list (cdr list)))))
        (while list
          (message "Searching subfile %s..." (cdr (car list)))
          (Info-read-subfile (car (car list)))
          (setq res (append (ee-info-data-collect-menus-current field-names) res))
          (setq list (cdr list)))
        (nreverse res)))))

(defun ee-info-data-collect-menus-current (field-names)
  "Returns list of menus extracted from current info file.
It returns all nodes, even those that are not accessible from menus.
Output: [[\"nodename1\",(\"subnode2\",\"subnode3\")]]."
  ;; TODO: restore *info* buffer prev content after search
  (let ((res))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (and (search-forward "\n\^_\nFile:" nil 'move)
                    (search-forward "Node: " nil 'move))
          (let (nodename section-name menu-items index-items ref-items beg bound)
            (setq nodename (Info-following-node-name))
            (forward-line 1)
            (setq beg (point))
            (search-forward "\n\^_" nil 'move)
            (beginning-of-line)
            (forward-line -1)
            (setq bound (point))
            (goto-char beg)
            (when (re-search-forward "^\\* Menu:" bound t)
              (forward-line 1)
              (beginning-of-line)
              ;; TODO: read section names from Top node
              (cond
               ((equal nodename "Top")
                (while (and (< (point) bound)
                            (not (looking-at "^[ \t]*-+ The Detailed Node Listing")))
                  (cond
                   ;; Menu line
                   ((looking-at "^\\* +\\([^:\t\n]*\\):")
                    (beginning-of-line)
                    (forward-char 2)
                    (let ((menu-node-name (Info-extract-menu-node-name)))
                      (setq menu-items
                            (cons menu-node-name ;; (list menu-node-name section-name)
                                  ;; (match-string-no-properties 1)
                                  menu-items))
                      (setq ee-info-section-names
                            (cons (list menu-node-name (or section-name "Top"))
                                   ee-info-section-names))))
                   ;; Other non-empty strings in the dir buffer are section names
                   ((looking-at "^\\([^ \t\n][^:\n]*\\)")
                    (setq section-name (match-string-no-properties 1))))
                  (forward-line 1)
                  (beginning-of-line)))
               ((string-match "Index" nodename)
                ;; Accept index menu items, e.g.:
                ;; * forward-list:                          List Motion.
                (while (re-search-forward "\n\\* +\\([^:\t\n]*\\):" bound t)
                  (beginning-of-line)
                  (forward-char 2)
                  (setq index-items (cons (Info-extract-menu-node-name)
                                          ;; (match-string-no-properties 1)
                                          index-items))))
               (t
                (while (re-search-forward "\n\\* +\\([^:\t\n]*\\):" bound t)
                  (beginning-of-line)
                  (forward-char 2)
                  (setq menu-items (cons (Info-extract-menu-node-name)
                                         ;; (match-string-no-properties 1)
                                         menu-items))))))
            (setq res (cons
                       (mapcar
                        (lambda (field-name)
                          (cond
                           ((eq field-name 'nodename)
                            nodename)
                           ((eq field-name 'category)
                            (cadr (assoc nodename ee-info-section-names)))
                           ((eq field-name 'menulist)
                            (if (not (equal nodename "Top")) ; hack
                                (nreverse menu-items)))
                           ((eq field-name 'indexlist)
                            (nreverse index-items))))
                        field-names)
                       res))
            (goto-char bound)))))
    res))

;; TODO: make patch and send to <bug-gnu-emacs@gnu.org>
(defun Info-extract-menu-node-name (&optional errmessage multi-line)
  (skip-chars-forward " \t\n")
  (let ((beg (point))
        str i)
    (skip-chars-forward "^:")
    (forward-char 1)
    (setq str
          (if (looking-at ":")
              (buffer-substring-no-properties beg (1- (point)))
            (skip-chars-forward " \t\n")
            (Info-following-node-name (if multi-line "^.,\t" "^.,\t\n"))))
    (while (setq i (string-match "\n" str i))
      (aset str i ?\ ))
    ;; Collapse multiple spaces.
    (while (string-match "  +" str)
      (setq str (replace-match " " t t str)))
    (let ((trim (string-match "\\s *\\'" str)))
      (if trim (setq str (substring str 0 trim))))
    str))

;;; Actions

(defun ee-info-find-node (&optional arg other-window)
  (interactive)
  (let ((nodename (ee-field 'nodename))
        ;; Set ee-info-file to info-file, because buffer-local
        ;; ee-info-file is not available after switching to *info*
        (info-file ee-info-file))
    ;; Mark as read and update view
    (ee-field-set 'read t)
    (ee-view-update '(read)) ;; (ee-view-record-update)
    (when other-window
      (if (one-window-p)
          ;; (split-window-vertically 11)
          (split-window-horizontally 33))
      (select-window (next-window)))
    ;;     (some-window (lambda (w)
    ;;                    (eq (buffer-name (window-buffer w)) "*info*")
    ;;                    ;(eq (window-buffer w) ee-parent-buffer)
    ;;                    ))
    (when nodename
      ;; Pop to *info* to save previous node into Info-history
      (pop-to-buffer "*info*")
      (Info-find-node info-file nodename))
      (if (eq other-window 'display)
          (select-window (next-window)))))

(defun ee-info-find-node-other-window (&optional arg)
  (interactive)
  (ee-info-find-node arg t))

(defun ee-info-find-node-other-window-display (&optional arg)
  (interactive)
  (ee-info-find-node arg 'display))

(defun ee-info-mark-bookmark ()
  (interactive)
  (if (ee-field 'bookmark)
      (ee-field-set 'bookmark nil)
    (ee-field-set 'bookmark t))
  (ee-view-update '(bookmark)) ;; (ee-view-record-update)
  )

(defun ee-info-mark-unread ()
  (interactive)
  (if (ee-field 'read)
      (ee-field-set 'read nil)
    (ee-field-set 'read t))
  (ee-view-update '(read)) ;; (ee-view-record-update)
  )

(defun ee-info-next-unread ()
  (interactive)
  (ee-view-record-next-with
   (lambda () (eq (ee-field 'read) nil)))
  (ee-info-find-node-other-window-display))

;;; Key Bindings

(defvar ee-info-keymap nil
  "Local keymap for ee-info-mode info.")

;; TODO: move keybindings to defvar view/info.ee?
(defun ee-info-keymap-make-default ()
  "Defines default key bindings for `ee-info-keymap'.
It inherits key bindings from `ee-mode-map'."
  (or ee-mode-map
      (ee-mode-map-make-default))
  (let ((map (copy-keymap ee-mode-map)))
    (define-key map "e" 'ee-info-find-node)
    (define-key map "o" 'ee-info-find-node-other-window)
    (define-key map "\C-o" 'ee-info-find-node-other-window-display)
    (define-key map "b" 'ee-info-mark-bookmark)
    ;; TODO: same keys as in Gnus
    (define-key map "\M-r" 'ee-info-mark-unread)
    (define-key map "\M-u" 'ee-info-mark-unread)
    (define-key map [tab] 'ee-info-next-unread)
    (setq ee-info-keymap map)))

(or ee-info-keymap
    (ee-info-keymap-make-default))

;;; Top-Level Functions

;; TODO: place next into e.g. (if ee-info-selection-hook-p ...) or (if ee-info-follow-nodes ...)
(add-hook 'Info-selection-hook
          (lambda ()
            (let ((node Info-current-node)
                  (file Info-current-file)
                  (curr-buffer (current-buffer))
                  (curr-window (selected-window))
                  buffer)
              (mapc (lambda (dir)
                      (if (string-match (concat "^" dir) file)
                          (setq file (substring file (length dir)))))
                    Info-directory-list)
              (setq buffer (format "*%s*/%s" ee-info-mode-name file))
              (cond ((get-buffer-window buffer)
                     (select-window (get-buffer-window buffer))
                     (when (not (eq (point-min) (point-max)))
                       (goto-char (point-min))
                       (search-forward node nil t)
                       (beginning-of-line)
                       ;; TODO: expand hidden branch
                       (ee-field-set 'read t)
                       (ee-view-update '(read)))
                     (select-window curr-window))
                    ((get-buffer buffer)
                     (set-buffer buffer)
                     (when (not (eq (point-min) (point-max)))
                       (goto-char (point-min))
                       (search-forward node nil t)
                       (beginning-of-line)
                       ;; TODO: expand hidden branch
                       (ee-field-set 'read t)
                       (ee-view-update '(read)))
                     (set-buffer curr-buffer))))))

;;;###autoload
(defun ee-info (&optional file)
  "Enter ee-info, the documentation browser.
Optional argument FILE specifies the file to examine;
the default is the top-level directory of Info.

In interactive use, a prefix argument directs this command
to read a file name from the minibuffer.

The search path for Info files is in the variable `Info-directory-list'.
The top-level Info directory is made by combining all the files named `dir'
in all the directories in that path."
  (interactive (if current-prefix-arg
                   ;; TODO: make list of available info node names
                   (list (read-string "Info file name: " nil nil t))))
  (or (featurep 'info)
      (require  'info))
  (info-initialize)
  (if (not (stringp file))
      (ee-view-buffer-create
       (format "*%s*/dir" ee-info-dir-mode-name)
       ee-info-dir-mode-name
       ee-info-dir-keymap
       ee-info-dir-data)
    (setq ee-info-file file)
    (ee-view-buffer-create
     (format "*%s*/%s" ee-info-mode-name file)
     ee-info-mode-name
     ee-info-keymap
     ee-info-data
     nil
     nil ;; TODO: setq ee-parent-buffer to *info*?
     nil
     (format ee-info-data-file-name-format file)
     t ;; auto-save
     )))

(provide 'ee-info)

;;; ee-info.el ends here
