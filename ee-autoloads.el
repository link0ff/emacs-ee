(provide 'ee-autoloads)

;;;### (autoloads (ee) "ee" "ee.el" (16181 17540))
;;; Generated autoloads from ee.el

(autoload (quote ee) "ee" "\
Enter top-level index of all available ee extensions.
Optional argument FILE specifies the file to examine;
the default is the top-level mode list.
In interactive use, a prefix argument directs this command
to read a root file name from the minibuffer." t nil)

;;;***

;;;### (autoloads (ee-bbdb) "ee-bbdb" "ee-bbdb.el" (16146 26252))
;;; Generated autoloads from ee-bbdb.el

(autoload (quote ee-bbdb) "ee-bbdb" "\
Summary mode for BBDB." t nil)

;;;***

;;;### (autoloads (ee-buffers) "ee-buffers" "ee-buffers.el" (16158
;;;;;;  29978))
;;; Generated autoloads from ee-buffers.el

(autoload (quote ee-buffers) "ee-buffers" "\
Display and manipulate Emacs buffers." t nil)

;;;***

;;;### (autoloads (ee-commands) "ee-commands" "ee-commands.el" (16146
;;;;;;  26272))
;;; Generated autoloads from ee-commands.el

(autoload (quote ee-commands) "ee-commands" "\
Categorized menu of Emacs commands." t nil)

;;;***

;;;### (autoloads (ee-customize) "ee-customize" "ee-customize.el"
;;;;;;  (16146 28252))
;;; Generated autoloads from ee-customize.el

(autoload (quote ee-customize) "ee-customize" "\
Browse Emacs customization groups." t nil)

;;;***

;;;### (autoloads (ee-datafile ee-datafile-mode) "ee-datafile" "ee-datafile.el"
;;;;;;  (16133 38906))
;;; Generated autoloads from ee-datafile.el
 (add-to-list 'auto-mode-alist '("\\.ee\\'" . emacs-lisp-mode))

(autoload (quote ee-datafile-mode) "ee-datafile" "\
Datafile view mode.
The purpose of this function is to create the view buffer,
when user visits a file with -*- mode: ee-datafile -*-." t nil)

(autoload (quote ee-datafile) "ee-datafile" "\
Display and edit data files." t nil)

;;;***

;;;### (autoloads (ee-dired) "ee-dired" "ee-dired.el" (16146 26298))
;;; Generated autoloads from ee-dired.el

(autoload (quote ee-dired) "ee-dired" "\
Categorized directory listings." t nil)

;;;***

;;;### (autoloads (ee-dselect) "ee-dselect" "ee-dselect.el" (16148
;;;;;;  18956))
;;; Generated autoloads from ee-dselect.el

(autoload (quote ee-dselect) "ee-dselect" "\
Debian package handling frontend." t nil)

;;;***

;;;### (autoloads (ee-edb) "ee-edb" "ee-edb.el" (16146 26322))
;;; Generated autoloads from ee-edb.el

(autoload (quote ee-edb) "ee-edb" "\
Summary mode for EDB." t nil)

;;;***

;;;### (autoloads (ee-example) "ee-example" "ee-example.el" (16146
;;;;;;  26340))
;;; Generated autoloads from ee-example.el

(autoload (quote ee-example) "ee-example" "\
Accompanying example for demonstration of ee capabilities." t nil)

;;;***

;;;### (autoloads (ee-fields) "ee-fields" "ee-fields.el" (16146 32526))
;;; Generated autoloads from ee-fields.el

(autoload (quote ee-fields) "ee-fields" "\
Display and edit fields of the current record." t nil)

;;;***

;;;### (autoloads (ee-finder) "ee-finder" "ee-finder.el" (16146 36440))
;;; Generated autoloads from ee-finder.el

(autoload (quote ee-finder) "ee-finder" "\
Keyword-based Emacs code finder." t nil)

;;;***

;;;### (autoloads (ee-gnus) "ee-gnus" "ee-gnus.el" (16146 26420))
;;; Generated autoloads from ee-gnus.el

(autoload (quote ee-gnus) "ee-gnus" "\
Summary and topic mode for Gnus." t nil)

;;;***

;;;### (autoloads (ee-history-shell-command ee-history-extended-command
;;;;;;  ee-history-command) "ee-history" "ee-history.el" (16146 26440))
;;; Generated autoloads from ee-history.el

(autoload (quote ee-history-command) "ee-history" "\
Display list from Emacs variable `command-history'." t nil)

(autoload (quote ee-history-extended-command) "ee-history" "\
Display list from Emacs variable `extended-command-history'." t nil)

(autoload (quote ee-history-shell-command) "ee-history" "\
Display list from Emacs variable `shell-command-history'." t nil)

;;;***

;;;### (autoloads (ee-imenu) "ee-imenu" "ee-imenu.el" (16146 26454))
;;; Generated autoloads from ee-imenu.el

(autoload (quote ee-imenu) "ee-imenu" "\
Categorized mode-specific buffer indexes." t nil)

;;;***

;;;### (autoloads (ee-info) "ee-info" "ee-info.el" (16175 50042))
;;; Generated autoloads from ee-info.el

(autoload (quote ee-info) "ee-info" "\
Enter ee-info, the documentation browser.
Optional argument FILE specifies the file to examine;
the default is the top-level directory of Info.

In interactive use, a prefix argument directs this command
to read a file name from the minibuffer.

The search path for Info files is in the variable `Info-directory-list'.
The top-level Info directory is made by combining all the files named `dir'
in all the directories in that path." t nil)

;;;***

;;;### (autoloads (ee-marks) "ee-marks" "ee-marks.el" (16150 33348))
;;; Generated autoloads from ee-marks.el

(autoload (quote ee-marks) "ee-marks" "\
Display and go to marked lines in the current Emacs buffer." t nil)

;;;***

;;;### (autoloads (ee-menubar) "ee-menubar" "ee-menubar.el" (16153
;;;;;;  19104))
;;; Generated autoloads from ee-menubar.el
 (define-key global-map "\M-`" 'ee-menubar)
 (define-key global-map [f10] 'ee-menubar)
 (fset 'ee-textmenu 'ee-menubar)
 (fset 'ee-tmm 'ee-menubar)

(autoload (quote ee-menubar) "ee-menubar" "\
Categorized access to Emacs menu-bar." t nil)

;;;***

;;;### (autoloads (ee-outline) "ee-outline" "ee-outline.el" (16146
;;;;;;  26526))
;;; Generated autoloads from ee-outline.el

(autoload (quote ee-outline) "ee-outline" "\
Manipulate outlines collected from outline-mode." t nil)

;;;***

;;;### (autoloads (ee-processes) "ee-processes" "ee-processes.el"
;;;;;;  (16146 26536))
;;; Generated autoloads from ee-processes.el

(autoload (quote ee-processes) "ee-processes" "\
Display and manipulate Emacs processes." t nil)

;;;***

;;;### (autoloads (ee-programs) "ee-programs" "ee-programs.el" (16150
;;;;;;  52362))
;;; Generated autoloads from ee-programs.el

(autoload (quote ee-programs) "ee-programs" "\
Categorized program menu." t nil)

;;;***

;;;### (autoloads (ee-ps) "ee-ps" "ee-ps.el" (16146 26576))
;;; Generated autoloads from ee-ps.el

(autoload (quote ee-ps) "ee-ps" "\
Display CPU processes." t nil)
 (fset 'ee-top 'ee-ps)

;;;***

;;;### (autoloads (ee-tags) "ee-tags" "ee-tags.el" (16149 4780))
;;; Generated autoloads from ee-tags.el

(autoload (quote ee-tags) "ee-tags" "\
Etags facility." t nil)

;;;***

;;;### (autoloads (ee-textfile-apachelog ee-textfile-changelog) "ee-textfile"
;;;;;;  "ee-textfile.el" (16180 54332))
;;; Generated autoloads from ee-textfile.el

(autoload (quote ee-textfile-changelog) "ee-textfile" "\
Organize information from ChangeLog files." t nil)

(autoload (quote ee-textfile-apachelog) "ee-textfile" "\
Organize information from Apache log files." t nil)

;;;***

;;;### (autoloads (ee-views) "ee-views" "ee-views.el" (16146 36680))
;;; Generated autoloads from ee-views.el

(autoload (quote ee-views) "ee-views" "\
Display, edit and switch views." t nil)

;;;***

;;;### (autoloads (ee-windows ee-windows-add) "ee-windows" "ee-windows.el"
;;;;;;  (16146 29634))
;;; Generated autoloads from ee-windows.el

(autoload (quote ee-windows-add) "ee-windows" "\
Add current Emacs window configuration." t nil)

(autoload (quote ee-windows) "ee-windows" "\
Display and switch Emacs window configurations." t nil)

;;;***

