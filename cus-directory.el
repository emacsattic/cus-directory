(defun custom-add-to-group (group option widget)
  "To existing GROUP add a new OPTION of type WIDGET.
If there already is an entry for OPTION and WIDGET, nothing is done."
  (let ((options (get group 'custom-group))
	(entry (list option widget))
	(type (cond ((eq widget 'custom-variable) 'variable-groups)
		    ((eq widget 'custom-face) 'face-groups))))
    (unless (member entry options)
      (put group 'custom-group (nconc options (list entry)))
      (let ((groups (get option type)))
	;; This allows us to determine the main group of an option
	;; and also more efficient lookup.
	(when type
	  (put option type (nconc groups (list group))))))))

;; This will only be non-nil if the user init
;; file contains a call to `custom-set-all'.
(defvar custom-directory nil)

;; FIXME this shouldn't need to be non-nil at compile time
(setq custom-directory t)

(defun custom-set-all (&optional directory)
  "Install user customizations of all option values.
By default customization is loaded from files ending in \".el\" inside
the custom directory \"~/.emacs.d/custom.d/\".

If your customization is stored elsewhere you have to set the optional
DIRECTORY argument to that location, causing this function to load files
from that directory.  DIRECTORY is stored in variable `custom-directory'
so that functions that save customization use the same location.

Note that even when you use the default location your init file still has
to contain a call to this function.  Otherwise you can't customize any
options and your existing customization aren't loaded.

You should call this function only once per session and you shouldn't set
`custom-directory' by any other means.  Also make sure you place the call
to this function before library `cus-edit.el' is loaded or you won't be
able to save customizations.

In order to change the location of your customization files you have to
manually move them.  Then you should change the DIRECTORY argument to this
function accordingly and restart Emacs.  (Alternatively you can also set
variable `custom-directory' this one time only, so you don't have to
restart; if you know what you are doing.)

The files inside the custom directory are named after the main group of
the options they contain.  The main group is the first group used in the
definition of the option.  For options that do not define a group the file
is \"nil.el\".

The custom directory may contain Emacs lisp files that do not contain
calls to the functions `custom-set-variables' and `custom-set-faces'.
Regardless of the presence of these forms any file ending in \".el\"
inside `custom-directory' is loaded by this function. (That why by default
the custom directory is a subdirectory of \"~/.emacs.d\" not that
directory itself.)  Any file ending in \".el\" inside the custom directory
is deleted if it only contains whitespace."
  (setq custom-directory
	(if directory
	    (file-name-as-directory directory)
	  (convert-standard-filename "~/.emacs.d/custom.d/")))
  (unless (file-exists-p custom-directory)
    (make-directory custom-directory t))
  (mapc (lambda (file)
	  (load file nil t nil t))
	(directory-files custom-directory t "^[^.]+\.el$")))

(eval-when-compile
  (require 'cl))

(setq custom-commands
  '(("Set for current session" Custom-set t
     "Apply all settings in this buffer to the current session"
     "index")
    ("Save for future sessions" Custom-save custom-directory
     "Apply all settings in this buffer and save them for future Emacs sessions."
     "save")
    ("Undo edits" Custom-reset-current t
     "Restore all settings in this buffer to reflect their current values."
     "refresh")
    ("Reset to saved" Custom-reset-saved t
     "Restore all settings in this buffer to their saved values (if any)."
     "undo")
    ("Erase customizations" Custom-reset-standard custom-directory
     "Un-customize all settings in this buffer and save them with standard values."
     "delete")
    ("Help for Customize" Custom-help t
     "Get help for using Customize."
     "help")
    ("Exit" Custom-buffer-done t "Exit Customize." "exit")))

(defun custom-buffer-create-internal (options &optional description)
  (Custom-mode)
  ;; Insert verbose help at the top of the custom buffer.
  (when custom-buffer-verbose-help
    (widget-insert "Editing a setting changes only the text in this buffer."
		   (if custom-directory
		       "
To apply your changes, use the Save or Set buttons."
		     "
Currently, these settings cannot be saved for future Emacs sessions,
possibly because you started Emacs with `-q' or your init file does
not contain a call to `custom-set-all'.")
		   "\nFor details, see ")
    (widget-create 'custom-manual
		   :tag "Saving Customizations"
		   "(emacs)Saving Customizations")
    (widget-insert " in the ")
    (widget-create 'custom-manual
		   :tag "Emacs manual"
		   :help-echo "Read the Emacs manual."
		   "(emacs)Top")
    (widget-insert ".")
    (widget-insert "\n"))
    ;; The custom command buttons are also in the toolbar, so for a
    ;; time they were not inserted in the buffer if the toolbar was in use.
    ;; But it can be a little confusing for the buffer layout to
    ;; change according to whether or nor the toolbar is on, not to
    ;; mention that a custom buffer can in theory be created in a
    ;; frame with a toolbar, then later viewed in one without.
    ;; So now the buttons are always inserted in the buffer.  (Bug#1326)
;;;    (when (not (and (bound-and-true-p tool-bar-mode) (display-graphic-p)))
  (when custom-buffer-verbose-help
    (widget-insert "\n
 Operate on all settings in this buffer that are not marked HIDDEN:\n"))
  (let ((button (lambda (tag action active help icon)
		  (widget-insert " ")
		  (if (eval active)
		      (widget-create 'push-button :tag tag
				     :help-echo help :action action))))
	(commands custom-commands))
    (apply button (pop commands)) ; Set for current session
    (apply button (pop commands)) ; Save for future sessions
    (if custom-reset-button-menu
	(progn
	  (widget-insert " ")
	  (widget-create 'push-button
			 :tag "Reset buffer"
			 :help-echo "Show a menu with reset operations."
			 :mouse-down-action 'ignore
			 :action 'custom-reset))
      (widget-insert "\n")
      (apply button (pop commands)) ; Undo edits
      (apply button (pop commands)) ; Reset to saved
      (apply button (pop commands)) ; Erase customization
      (widget-insert "  ")
      (pop commands) ; Help (omitted)
      (apply button (pop commands)))) ; Exit
  (widget-insert "\n\n")

  ;; Now populate the custom buffer.
  (message "Creating customization items...")
  (buffer-disable-undo)
  (setq custom-options
	(if (= (length options) 1)
	    (mapcar (lambda (entry)
		      (widget-create (nth 1 entry)
				     :documentation-shown t
				     :custom-state 'unknown
				     :tag (custom-unlispify-tag-name
					   (nth 0 entry))
				     :value (nth 0 entry)))
		    options)
	  (let ((count 0)
		(length (length options)))
	    (mapcar (lambda (entry)
		      (prog2
			  (message "Creating customization items ...%2d%%"
				   (/ (* 100.0 count) length))
			  (widget-create (nth 1 entry)
					 :tag (custom-unlispify-tag-name
					       (nth 0 entry))
					 :value (nth 0 entry))
			(setq count (1+ count))
			(unless (eq (preceding-char) ?\n)
			  (widget-insert "\n"))
			(widget-insert "\n")))
		    options))))
  (unless (eq (preceding-char) ?\n)
    (widget-insert "\n"))
  (message "Creating customization items ...done")
  (message "Resetting customization items...")
  (unless (eq custom-buffer-style 'tree)
    (mapc 'custom-magic-reset custom-options))
  (message "Resetting customization items...done")
  (message "Creating customization setup...")
  (widget-setup)
  (buffer-enable-undo)
  (goto-char (point-min))
  (message "Creating customization setup...done"))

(setq custom-variable-menu
  `(("Set for Current Session" custom-variable-set
     (lambda (widget)
       (eq (widget-get widget :custom-state) 'modified)))
    ;; Note that in all the backquoted code in this file, we test
    ;; init-file-user rather than user-init-file.  This is in case
    ;; cus-edit is loaded by something in site-start.el, because
    ;; user-init-file is not set at that stage.
    ;; http://lists.gnu.org/archive/html/emacs-devel/2007-10/msg00310.html
    ,@(when custom-directory
	'(("Save for Future Sessions" custom-variable-save
	   (lambda (widget)
	     (memq (widget-get widget :custom-state)
		   '(modified set changed rogue))))))
    ("Undo Edits" custom-redraw
     (lambda (widget)
       (and (default-boundp (widget-value widget))
	    (memq (widget-get widget :custom-state) '(modified changed)))))
    ("Reset to Saved" custom-variable-reset-saved
     (lambda (widget)
       (and (or (get (widget-value widget) 'saved-value)
		(get (widget-value widget) 'saved-variable-comment))
	    (memq (widget-get widget :custom-state)
		  '(modified set changed rogue)))))
    ,@(when custom-directory
	'(("Erase Customization" custom-variable-reset-standard
	   (lambda (widget)
	     (and (get (widget-value widget) 'standard-value)
		  (memq (widget-get widget :custom-state)
			'(modified set changed saved rogue)))))))
    ("Set to Backup Value" custom-variable-reset-backup
     (lambda (widget)
       (get (widget-value widget) 'backup-value)))
    ("---" ignore ignore)
    ("Add Comment" custom-comment-show custom-comment-invisible-p)
    ("---" ignore ignore)
    ("Show Current Value" custom-variable-edit
     (lambda (widget)
       (eq (widget-get widget :custom-form) 'lisp)))
    ("Show Saved Lisp Expression" custom-variable-edit-lisp
     (lambda (widget)
       (eq (widget-get widget :custom-form) 'edit)))))

(setq custom-face-menu
  `(("Set for Current Session" custom-face-set)
    ,@(when custom-directory
	'(("Save for Future Sessions" custom-face-save)))
    ("Undo Edits" custom-redraw
     (lambda (widget)
       (memq (widget-get widget :custom-state) '(modified changed))))
    ("Reset to Saved" custom-face-reset-saved
     (lambda (widget)
       (or (get (widget-value widget) 'saved-face)
	   (get (widget-value widget) 'saved-face-comment))))
    ,@(when custom-directory
	'(("Erase Customization" custom-face-reset-standard
	   (lambda (widget)
	     (get (widget-value widget) 'face-defface-spec)))))
    ("---" ignore ignore)
    ("Add Comment" custom-comment-show custom-comment-invisible-p)
    ("---" ignore ignore)
    ("For Current Display" custom-face-edit-selected
     (lambda (widget)
       (not (eq (widget-get widget :custom-form) 'selected))))
    ("For All Kinds of Displays" custom-face-edit-all
     (lambda (widget)
       (not (eq (widget-get widget :custom-form) 'all))))
    ("Show Lisp Expression" custom-face-edit-lisp
     (lambda (widget)
       (not (eq (widget-get widget :custom-form) 'lisp))))))

;;; Reading and writing the custom files.

;; remove `custom-file'

;;;###autoload
(defun custom-save-all ()
  "Save all customizations."
  (dolist (group (custom-groups))
    (custom-save-group group)))

(defun custom-groups ()
  "Return all customization groups."
  (let ((groups (make-list 1 0)))
    (mapatoms (lambda (symbol)
		(when (get symbol 'custom-group)
		  (nconc groups (list symbol)))))
    (cdr groups)))

(defun custom-group-options (group)
  "Return all options whose main group is GROUP."
  ;; ??? Why is spec relevant here?  It isn't used in
  ;; `custom-save-variables' and `custom-save-faces'?
  (let ((variables (make-list 1 0))
	(faces (make-list 1 0)))
    (dolist (elt (get group 'custom-group))
      (let ((symbol (car elt)))
	(case (cadr elt)
	  (custom-variable
	   (when (and (eq (car (get symbol 'variable-groups)) group)
		      (let ((spec (car-safe (get symbol 'theme-value))))
			(or (get symbol 'saved-variable-comment)
			    (if spec
				(eq (car spec) 'user)
			      (get symbol 'saved-value)))))
	     (nconc variables (list symbol))))
	  (custom-face
	   (when (and (eq (car (get symbol 'face-groups)) group)
		      (let ((spec (car-safe (get symbol 'theme-face))))
			(or (get symbol 'saved-face-comment)
			    (if spec
				(eq (car spec) 'user)
			      (get symbol 'saved-face)))))
	     (nconc faces (list (car elt))))))))
    (list (sort (cdr variables) 'string<)
	  (sort (cdr faces) 'string<))))

(defun custom-save-group (group)
  "Save GROUP's customization in the group-specific custom file."
  (let* ((options (custom-group-options group))
	 (variables (car options))
	 (faces (cadr options))
	 (filename (concat custom-directory (symbol-name group) ".el"))
	 (recentf-exclude
	  (if recentf-mode
	      (cons (concat "\\`"
			    (regexp-quote
			     (recentf-expand-file-name filename))
			    "\\'")
		    recentf-exclude)))
	 (old-buffer (find-buffer-visiting filename))
	 old-buffer-name)
    (when (or variables faces
	      (file-exists-p filename))
      (with-current-buffer (let ((find-file-visit-truename t))
			     (or old-buffer (find-file-noselect filename)))
	(when old-buffer
	  (setq old-buffer-name (buffer-file-name))
	  (set-visited-file-name (file-chase-links filename)))
	(unless (eq major-mode 'emacs-lisp-mode)
	  (emacs-lisp-mode))
	(let ((inhibit-read-only t))
	  (custom-save-variables variables)
	  (custom-save-faces faces))
	(let ((file-precious-flag t))
	  (save-buffer))
	;; Delete file if empty except for whitespace.
	(save-excursion
	  (widen)
	  (goto-char (point-min))
	  (while (memq (char-after) '(9 10 32))
	    (forward-char))
	  (when (eq (point) (point-max))
	    (delete-file filename)))
	(if old-buffer 9 10 32
	    (progn
	      (set-visited-file-name old-buffer-name)
	      (set-buffer-modified-p nil))
	    (kill-buffer (current-buffer)))))))

(defun custom-save-variables (variables)
  "Save VARIABLES in currently visited file."
  ;; Callers of this function have to make sure VARIABLES
  ;; only contain customized variables.
  (save-excursion
    (custom-save-delete 'custom-set-variables)
    (when variables
      (let ((standard-output (current-buffer))
	    sort-fold-case)
	(unless (bolp)
	  (princ "\n"))
	(princ "(custom-set-variables")
	(dolist (symbol variables)
	  (let ((value (get symbol 'saved-value))
		(requests (get symbol 'custom-requests))
		(now (and (not (custom-variable-p symbol))
			  (or (boundp symbol)
			      (eq (get symbol 'force-value)
				  'rogue))))
		(comment (get symbol 'saved-variable-comment)))
	    ;; Check REQUESTS for validity.
	    (dolist (request requests)
	      (when (and (symbolp request) (not (featurep request)))
		(message "Unknown requested feature: %s" request)
		(setq requests (delq request requests))))
	    (unless (bolp)
	      (princ "\n"))
	    (princ " '(")
	    (prin1 symbol)
	    (princ " ")
	    (prin1 (car value))
	    (when (or now requests comment)
	      (princ " ")
	      (prin1 now)
	      (when (or requests comment)
		(princ " ")
		(prin1 requests)
		(when comment
		  (princ " ")
		  (prin1 comment))))
	    (princ ")")))
	(when (bolp)
	  (princ " "))
	(princ ")")
	(unless (looking-at "\n")
	  (princ "\n"))))))

(defun custom-save-faces (faces)
  "Save FACES in currently visited file."
  ;; Callers of this function have to make sure FACES
  ;; only contain customized faces.
  (save-excursion
    (custom-save-delete 'custom-reset-faces)
    (custom-save-delete 'custom-set-faces)
    (when faces
      (let ((standard-output (current-buffer))
	    sort-fold-case)
	;; The default face must be first, since it affects the others.
	(when (memq 'default faces)
	  (setq faces (cons 'default (delq 'default faces))))
	(unless (bolp)
	  (princ "\n"))
	(princ "(custom-set-faces")
	(dolist (symbol faces)
	  (let ((value (get symbol 'saved-face))
		(now (not (or (get symbol 'face-defface-spec)
			      (and (not (custom-facep symbol))
				   (not (get symbol 'force-face))))))
		(comment (get symbol 'saved-face-comment)))
	    (unless (bolp)
	      (princ "\n"))
	    (princ " '(")
	    (prin1 symbol)
	    (princ " ")
	    (prin1 value)
	    (when (or now comment)
	      (princ " ")
	      (prin1 now)
	      (when comment
		(princ " ")
		(prin1 comment)))
	    (princ ")")))
	(when (bolp)
	  (princ " "))
	(princ ")")
	(unless (looking-at "\n")
	  (princ "\n"))))))

(provide 'cus-directory)
