;;; ee-autoloads.el --- automatically extracted autoloads (do not edit)  -*- lexical-binding: nil -*-
(provide 'ee-autoloads)

;;;### (autoloads nil "ee" "ee.el" (0 0 0 0))
;;; Generated autoloads from ee.el

(autoload 'ee "ee" "\
Enter top-level index of all available ee extensions.
Optional argument FILE specifies the file to examine;
the default is the top-level mode list.
In interactive use, a prefix argument directs this command
to read a root file name from the minibuffer.

\(fn &optional FILE)" t)

(register-definition-prefixes "ee" '("ee-" "posn-col-row-sans-header"))

;;;***

;;;### (autoloads nil "ee-bbdb" "ee-bbdb.el" (0 0 0 0))
;;; Generated autoloads from ee-bbdb.el

(autoload 'ee-bbdb "ee-bbdb" "\
Summary mode for BBDB.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-bbdb" '("ee-bbdb-"))

;;;***

;;;### (autoloads nil "ee-buffers" "ee-buffers.el" (0 0 0 0))
;;; Generated autoloads from ee-buffers.el

(autoload 'ee-buffers "ee-buffers" "\
Display and manipulate Emacs buffers.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-buffers" '("ee-buffers-"))

;;;***

;;;### (autoloads nil "ee-commands" "ee-commands.el" (0 0 0 0))
;;; Generated autoloads from ee-commands.el

(autoload 'ee-commands "ee-commands" "\
Categorized menu of Emacs commands.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-commands" '("ee-commands-"))

;;;***

;;;### (autoloads nil "ee-customize" "ee-customize.el" (0 0 0 0))
;;; Generated autoloads from ee-customize.el

(autoload 'ee-customize "ee-customize" "\
Browse Emacs customization groups.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-customize" '("ee-customize-"))

;;;***

;;;### (autoloads nil "ee-datafile" "ee-datafile.el" (0 0 0 0))
;;; Generated autoloads from ee-datafile.el
 (add-to-list 'auto-mode-alist '("\\.ee\\'" . lisp-data-mode))

(autoload 'ee-datafile-mode "ee-datafile" "\
Datafile view mode.
The purpose of this function is to create the view buffer,
when user visits a file with -*- mode: ee-datafile -*-.

\(fn &optional ARG)" t)

(autoload 'ee-datafile "ee-datafile" "\
Display and edit data files.

\(fn &optional ARG FILE)" t)

(register-definition-prefixes "ee-datafile" '("ee-datafile-mode-name"))

;;;***

;;;### (autoloads nil "ee-dired" "ee-dired.el" (0 0 0 0))
;;; Generated autoloads from ee-dired.el

(autoload 'ee-dired "ee-dired" "\
Categorized directory listings.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-dired" '("ee-dired-"))

;;;***

;;;### (autoloads nil "ee-dselect" "ee-dselect.el" (0 0 0 0))
;;; Generated autoloads from ee-dselect.el

(autoload 'ee-dselect "ee-dselect" "\
Debian package handling frontend.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-dselect" '("ee-dselect-"))

;;;***

;;;### (autoloads nil "ee-edb" "ee-edb.el" (0 0 0 0))
;;; Generated autoloads from ee-edb.el

(autoload 'ee-edb "ee-edb" "\
Summary mode for EDB.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-edb" '("ee-edb-"))

;;;***

;;;### (autoloads nil "ee-ell" "ee-ell.el" (0 0 0 0))
;;; Generated autoloads from ee-ell.el

(autoload 'ee-ell "ee-ell" "\
Browse the categorized Emacs Lisp List.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-ell" '("ee-ell-"))

;;;***

;;;### (autoloads nil "ee-example" "ee-example.el" (0 0 0 0))
;;; Generated autoloads from ee-example.el

(autoload 'ee-example "ee-example" "\
Accompanying example for demonstration of ee capabilities.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-example" '("ee-example-"))

;;;***

;;;### (autoloads nil "ee-fields" "ee-fields.el" (0 0 0 0))
;;; Generated autoloads from ee-fields.el

(autoload 'ee-fields "ee-fields" "\
Display and edit fields of the current record.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-fields" '("ee-fields-"))

;;;***

;;;### (autoloads nil "ee-finder" "ee-finder.el" (0 0 0 0))
;;; Generated autoloads from ee-finder.el

(autoload 'ee-finder "ee-finder" "\
Keyword-based Emacs code finder.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-finder" '("ee-finder-"))

;;;***

;;;### (autoloads nil "ee-gnus" "ee-gnus.el" (0 0 0 0))
;;; Generated autoloads from ee-gnus.el

(autoload 'ee-gnus "ee-gnus" "\
Summary and topic mode for Gnus.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-gnus" '("ee-gnus-"))

;;;***

;;;### (autoloads nil "ee-history" "ee-history.el" (0 0 0 0))
;;; Generated autoloads from ee-history.el

(autoload 'ee-history-command "ee-history" "\
Display list from Emacs variable `command-history'.

\(fn &optional ARG)" t)

(autoload 'ee-history-extended-command "ee-history" "\
Display list from Emacs variable `extended-command-history'.

\(fn &optional ARG)" t)

(autoload 'ee-history-shell-command "ee-history" "\
Display list from Emacs variable `shell-command-history'.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-history" '("ee-history-"))

;;;***

;;;### (autoloads nil "ee-imenu" "ee-imenu.el" (0 0 0 0))
;;; Generated autoloads from ee-imenu.el

(autoload 'ee-imenu "ee-imenu" "\
Categorized mode-specific buffer indexes.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-imenu" '("ee-imenu-"))

;;;***

;;;### (autoloads nil "ee-info" "ee-info.el" (0 0 0 0))
;;; Generated autoloads from ee-info.el

(autoload 'ee-info "ee-info" "\
Enter ee-info, the documentation browser.
Optional argument FILE specifies the file to examine;
the default is the top-level directory of Info.

In interactive use, a prefix argument directs this command
to read a file name from the minibuffer.

The search path for Info files is in the variable `Info-directory-list'.
The top-level Info directory is made by combining all the files named `dir'
in all the directories in that path.

\(fn &optional FILE)" t)

(register-definition-prefixes "ee-info" '("ee-info-"))

;;;***

;;;### (autoloads nil "ee-marks" "ee-marks.el" (0 0 0 0))
;;; Generated autoloads from ee-marks.el

(autoload 'ee-marks "ee-marks" "\
Display and go to marked lines in the current Emacs buffer.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-marks" '("ee-marks-"))

;;;***

;;;### (autoloads nil "ee-menubar" "ee-menubar.el" (0 0 0 0))
;;; Generated autoloads from ee-menubar.el

(autoload 'ee-menubar "ee-menubar" "\
Categorized access to Emacs menu-bar.

\(fn &optional ARG)" t)
 (defalias 'ee-tmm 'ee-menubar)

(register-definition-prefixes "ee-menubar" '("ee-menubar-"))

;;;***

;;;### (autoloads nil "ee-outline" "ee-outline.el" (0 0 0 0))
;;; Generated autoloads from ee-outline.el

(autoload 'ee-outline "ee-outline" "\
Manipulate outlines collected from outline-mode.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-outline" '("ee-outline-"))

;;;***

;;;### (autoloads nil "ee-processes" "ee-processes.el" (0 0 0 0))
;;; Generated autoloads from ee-processes.el

(autoload 'ee-processes "ee-processes" "\
Display and manipulate Emacs processes.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-processes" '("ee-processes-"))

;;;***

;;;### (autoloads nil "ee-programs" "ee-programs.el" (0 0 0 0))
;;; Generated autoloads from ee-programs.el

(autoload 'ee-programs "ee-programs" "\
Categorized program menu.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-programs" '("ee-programs-"))

;;;***

;;;### (autoloads nil "ee-ps" "ee-ps.el" (0 0 0 0))
;;; Generated autoloads from ee-ps.el

(autoload 'ee-ps "ee-ps" "\
Display CPU processes.

\(fn &optional ARG)" t)
 (fset 'ee-top 'ee-ps)

(register-definition-prefixes "ee-ps" '("ee-ps-"))

;;;***

;;;### (autoloads nil "ee-tags" "ee-tags.el" (0 0 0 0))
;;; Generated autoloads from ee-tags.el

(autoload 'ee-tags "ee-tags" "\
Etags facility.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-tags" '("ee-tags-"))

;;;***

;;;### (autoloads nil "ee-textfile" "ee-textfile.el" (0 0 0 0))
;;; Generated autoloads from ee-textfile.el

(autoload 'ee-textfile-changelog "ee-textfile" "\
Organize information from ChangeLog files.

\(fn &optional ARG)" t)

(autoload 'ee-textfile-apachelog "ee-textfile" "\
Organize information from Apache log files.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-textfile" '("ee-textfile-"))

;;;***

;;;### (autoloads nil "ee-variables" "ee-variables.el" (0 0 0 0))
;;; Generated autoloads from ee-variables.el

(autoload 'ee-variables "ee-variables" "\
Categorized menu of Emacs variables.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-variables" '("ee-variables-"))

;;;***

;;;### (autoloads nil "ee-views" "ee-views.el" (0 0 0 0))
;;; Generated autoloads from ee-views.el

(autoload 'ee-views "ee-views" "\
Display, edit and switch views.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-views" '("ee-views-"))

;;;***

;;;### (autoloads nil "ee-windows" "ee-windows.el" (0 0 0 0))
;;; Generated autoloads from ee-windows.el

(autoload 'ee-windows-and-add-current "ee-windows" "\


\(fn &optional ARG)" t)

(autoload 'ee-windows-add "ee-windows" "\
Add current Emacs window configuration." t)

(autoload 'ee-windows "ee-windows" "\
Display and switch Emacs window configurations.

\(fn &optional ARG)" t)

(register-definition-prefixes "ee-windows" '("ee-windows-"))

;;;***

