(defun ideas ()


  retain point and mark positions
  perhaps insert some magic markers <<COLLAB-POINT 3945345>>
  go through merge
  and find them again

  after merging changes
  saving re-recreates conflicts


  maybe settle on this for now:
  when changes can be merged in silently, do so
  otherwise, use existing emacs mechanism


  in such a case
  the new ancestor should be
  - internal buffer
  - with all clean merges [optional -- consider later undo]
  - and all external conflicts resolved to "original"
  So the easiest solution would be to leave it as is.

  When conflicts, do not update file-visited time so that emacs warns of conflict.


  if merge conflicts are inserted
  what is the ancestor then?

  maybe remove all merge conflicts before merging in anything?
  we need warning when saving, but not when editing  


  right now we're replacing the complete buffer with the merge result
  wouldn't it be better to apply individual chunks?
  preserve point, mark, possible overlays, and so on?


  point/mark preserving doesn't work yet
  what about undo?  don't we need to undo the ancestor-setting, too?
  (maybe only ever set ancestor on save?  what about merge info?)


  if before-change-functions signal an error, they get taken out of the hook
  
  
    )



(defvar collaboration-ancestor nil)
(make-variable-buffer-local 'collaboration-ancestor)
(put 'collaboration-ancestor 'permanent-local t)

(defvar collab-diff3-program "diff3")

(defmacro with-temp-file-containing (source file-variable prefix &rest body)
  "Create temp file containing source (buffer or string).
file name is in variable given as `file-variable' while body executes.
File is removed afterwards."
  `(let ((wtfc-source ,source)
	 (wtfc-temp-file (make-temp-file ,prefix)))
     (let ((,file-variable wtfc-temp-file))
       (message "before write")
       (with-current-buffer (if (bufferp wtfc-source) wtfc-source (current-buffer))
	 (write-region (if (bufferp wtfc-source) nil wtfc-source)
		       nil wtfc-temp-file nil 'quiet))
       (unwind-protect
	   (progn
	     ,@body)
      (condition-case nil
	  (delete-file wtfc-temp-file)
	(error nil))))))
(put 'with-temp-file-containing 'lisp-indent-function 2)

(defun collaboration-merge-remote (beg end &optional oldlen)
  "Called before a change to a buffer.
Checks if remote (file) was changed, and merges in changes if necessary."

  (when (not (verify-visited-file-modtime))
    ;; must handle error because this is called from before-change-hook
    (condition-case err
	(collaboration-merge-before-save-function nil 'background)
      (error (message "%s" err))
    )))

(defun collaboration-merge (&optional merge-conflicts)
  "Merges buffer's file with buffer contents based on common ancestor.
With prefix argument, do not merge in conflicts."
  (interactive "P") ;; works find as a command as well
  (collaboration-merge-before-save-function (not merge-conflicts)))


(defun collab--get-location-context (&optional pos)
  (save-excursion
    (let ((pos (or pos (point)))
	  (left-context nil)
	  (right-context nil))
    (forward-line -1)
    (beginning-of-line)
    (setq left-context (buffer-substring-no-properties (point) pos))

    (goto-char pos)
    (forward-line 1)
    (end-of-line)
    (setq right-context (buffer-substring-no-properties pos (point)))
    (list left-context pos right-context))))

(defun collab--find-location-from-context (location-info)

  (let ((left-context (first location-info))
	(pos (second location-info))
	(right-context (third location-info)))

    (goto-char pos)
    ;; let's try to find the left context
    (or
     (if (or
	  (search-backward left-context (- pos 1000) 'noerr)
	  (search-forward left-context (+ pos 1000) 'noerr))
	 (goto-char (+ (point) (length left-context))))
     (progn
       (goto-char pos)
       (or
	(search-forward right-context (+ pos 1000) 'noerr)
	(search-backward right-context (- pos 1000) 'noerr)))
     ;; fall-back
     (goto-char pos)
     )))
;; to do - make this more robust

(defconst collab--preserve-undo-capability t) ;; we want to be able to undo merges

(defvar collaboration-last-number-of-conflicts 0)
(defun collaboration-merge-before-save-function (&optional merge-conflicts silently)
  "Merges buffer's file with buffer contents based on common ancestor.
Suitable for `before-save-hook'.
For an interactive function, call `collaboration-merge'."
  (collab-check-buffer-conflicted)
  (let ((own-buffer (current-buffer))
	(mark-loc (collab--get-location-context (mark)))
	(point-loc (collab--get-location-context)))
  (if (and collaboration-ancestor (buffer-file-name))
      (with-temp-file-containing collaboration-ancestor ancestor-file "Common-Ancestor-"
	(with-temp-file-containing own-buffer own-file "My-Version-"
	  (let ((their-file (buffer-file-name)) (merged-text nil))

	    ;; now call diff3 on this

	    ;; it's MYFILE OLDFILE YOURFILE
	    (with-temp-buffer
	       (let ((temp-buffer (current-buffer)))
		
	      ;; -3 can be used to only merge in unconflicted changes

					   (setq collaboration-last-number-of-conflicts 0)
	      (let ((d3args "--merge"))
		(collab-exec-process collab-diff3-program (current-buffer)
				     d3args
				     own-file ancestor-file their-file))

	      (setq collab-debug-merged (buffer-string))

	      (unless merge-conflicts
		;; now go through buffer and remove all conflicts
		(smerge-mode)
		(condition-case nil
		    (smerge-resolve-all)
		  (error nil))
		(goto-char (point-min))
		(condition-case nil
		    (while t
		      (smerge-next) ;; error at end
		      (smerge-keep-mine)
		      (setq collaboration-last-number-of-conflicts
			    (1+ collaboration-last-number-of-conflicts)))
		  (error nil)
		  ))
	      (message "%s conflicts ignored" collaboration-last-number-of-conflicts)
	      (set-buffer own-buffer)
	      
	      (if (> (point-max) 1)
		  (progn
		    (save-excursion ;; hopefully no change of point / mark etc - to do
		      (let ((modtime (visited-file-modtime)))
			(clear-visited-file-modtime)
			(erase-buffer)
			(insert-buffer-substring temp-buffer)
			;; saving is save now (provided there are no conflicts). 
			(set-visited-file-modtime
			 (if (eq collaboration-last-number-of-conflicts 0)
			     ;; if there were no conflicts, we can update the modtime
			     nil
			   ;; otherwise, set it back to what it was
			   modtime))))
		    (collab--find-location-from-context mark-loc)
		    (push-mark (point))
		    (collab--find-location-from-context point-loc)
		    (if merge-conflicts
			;; if we're merging in visble conflicts, we're going to want to
			;; set the ancestor to the external file, because
			;; we assume that if the user resolves conflicts,
			;; the current clean state of the external file can become the
			;; ancestor (so we track what the remote is doing).
			(collaboration-merge-set-ancestor-from-file))
		    ;; check for conflicts
		    ;; if we have conflicts, prevent saving and prompt to fix.
		    (if (collab-buffer-conflicted-p)
			(progn
			  (collab-init-smerge-mode silently)
			  (or silently
			      (message "Conflicting changes present in buffer."))
			  ;; prevent saving
			  (error "Conflicting changes in this file.  Resolve conflicts before saving."))
		      (if (and (eq collaboration-last-number-of-conflicts 0)
			       (not collab--preserve-undo-capability))
			  (collaboration-merge-set-ancestor))
		      (message "Changes merged. %s"
			       (if (> collaboration-last-number-of-conflicts 0)
				   (substitute-command-keys
				    (format "%s conflicting changes ignored.  Use \\[collaboration-merge] to see them." collaboration-last-number-of-conflicts))))
		      ))
		(error "Merge unsuccessful.")
		)))))))
	  ;; no ancestor found:
	  (or (collaboration-merge-set-ancestor-from-file)
	    (collab-init-smerge-mode silently)
	    (error "Collaboration could not be initialized.  Resolve conflicts first."))
	  ))


    ;; (message "Collaboration initialized.  Discarding other variants - your buffer will be saved.")


(defun collab-check-buffer-conflicted ()
  "Checks if conflicts are present.
Throws error and enters `smerge-mode' if so."
  (when (collab-buffer-conflicted-p)
    (collab-init-smerge-mode)
    (error "Collaboration could not be initialized.  Resolve conflicts first.")))

(defun collab-init-smerge-mode (&optional silently)
  (if silently
      (smerge-mode)
    (push-mark)
  (smerge-mode)
  (goto-char (point-min))
  (smerge-next)))


(defun collab-buffer-conflicted-p ()

  (save-match-data
    (save-excursion
      (condition-case nil
	  (progn
	    (goto-char (point-min))
	    (smerge-next) ;; signals error if none found
	 t)
	(error nil)))))
	    


(defun collaboration-merge-set-ancestor-from-file ()
  "Sets ancestor for `collaboration-mode' using visited file.
Applies to current buffer.

If no file is visited, call `collaboration-merge-set-ancestor'.

Called when first entering `collaboration-mode'."
  (interactive)

  (let ((my-buffer (current-buffer))
	(file (buffer-file-name)))
    (if file
	;; set local variable of current (my) buffer:
	(setq collaboration-ancestor
	      (with-temp-buffer 
		(insert-file-contents file nil)
		(buffer-string)))
      ;; no file visited
      (collaboration-merge-set-ancestor))))

(defun collaboration-merge-set-ancestor ()
  "Sets ancestor for `collaboration-mode'.
Applies to current buffer.

Will refuse to set ancestor if there are unresolved conflicts.
Returns nil in that case.

Suitable for `after-save-hook' and `after-load-functions'.
Also called when first entering `collaboration-mode'."
  (interactive)
  (if (collab-buffer-conflicted-p)
      (progn
	(if (called-interactively-p)
	    (message "Can't set ancestor - contains unresolved conflicts."))
	nil ;; do nothing
	)
    (setq collaboration-ancestor
	  (buffer-string))
    t))




    
  
  
;; Execute PROGRAM asynchronously, unless OS/2, Windows-*, or DOS, or unless
;; SYNCH is non-nil.  BUFFER must be a buffer object, and must be alive.  The
;; OPTIONS arg is a list of options to pass to PROGRAM. It may be a blank
;; string.  All elements in FILES must be strings.  We also delete nil from
;; args.
(defun collab-exec-process (program buffer options &rest files)
  ;; (let ((data (match-data))
	;; (unwind-protect
  (let ((directory default-directory)
	(args (append (split-string options) files))
		  proc)
	      (save-excursion
		(set-buffer buffer)
		(erase-buffer)
		
		(setq default-directory directory)
		(apply 'call-process program nil buffer nil args)
		)))
	  ;; (store-match-data data))))



(defadvice ask-user-about-lock  (around collaboration-lock-advice)
  "Do not ask if collaboration-mode is in effect."
  (if collaboration-mode
      t
    ad-do-it))

(defadvice ask-user-about-supersession-threat  (around collaboration-supersession-threat-advice)
  "Do not ask if collaboration-mode is in effect."
  (if collaboration-mode
      t
    ad-do-it))




(define-minor-mode collaboration-mode
  "Enables collaborative editing.
Similar to `auto-revert-mode', but merges in changes
present in the local file.  Changes are merged regularly,
and whenever the user attempts to modify the buffer.

This is intended to be used in conjunction with file
synchronization via `rsync', `Dropbox', `Google Drive', or a
common, networked file share.

The user is no longer asked before modifying the buffer,
even if the visited file has changed, or if another,
possibly remote Emacs process is visiting the file."
  :lighter " Collab"
  :version "0.1"

  (if collaboration-mode
      (progn
	(collab-check-buffer-conflicted)
	(add-hook 'before-save-hook #'collaboration-merge-before-save-function nil  'local)
	(add-hook 'after-save-hook #'collaboration-merge-set-ancestor-from-file nil 'local)
	(add-hook 'after-load-hook #'collaboration-merge-set-ancestor nil 'local)
	(add-hook 'before-change-functions #'collaboration-merge-remote nil 'local)
	(collaboration-merge-set-ancestor-from-file))
    (setq collaboration-ancestor nil)
    (remove-hook 'before-save-hook #'collaboration-merge-before-save-function 'local)
    (remove-hook 'after-save-hook #'collaboration-merge-set-ancestor-from-file 'local)
    (remove-hook 'after-load-hook #'collaboration-merge-set-ancestor 'local)
    (remove-hook 'before-change-functions #'collaboration-merge-remote 'local)))
