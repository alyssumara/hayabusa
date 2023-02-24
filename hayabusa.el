;;; hayabusa.el --- hayabusa -*- lexical-binding: t -*-



;;; Code:

(defgroup hayabusa nil
  ""
  :group 'convenience)

(defcustom hayabusa-keys-alist '(())
  ""
  :group 'hayabusa
  :type 'alist)

(defvar hayabusa-mode-map (make-sparse-keymap))

(define-minor-mode hayabusa-mode
  "hayabusa"
  :global t
  :keymap hayabusa-mode-map
  :lighter " 隼")

(defcustom hayabusa-insert-delay 0.3
  ""
  :group 'hayabusa
  :type 'float)

(defcustom hayabusa-insert-enable-when-isearch-mode t
  ""
  :group 'hayabusa
  :type 'boolean)

(defvar hayabusa--last-command nil)

(defvar hayabusa--last-command-event nil)

(defvar hayabusa--clear-last-command-with-idle-timer nil)

(defun hayabusa--set-clear-last-command-with-idle-timer ()
  (when hayabusa-insert-delay
    (setq hayabusa--clear-last-command-with-idle-timer
	  (run-with-idle-timer hayabusa-insert-delay nil #'hayabusa--clear-last-command))))

(defun hayabusa--clear-last-command ()
  (setq hayabusa--last-command nil))

(defun hayabusa--copy-last-command ()
  (setq hayabusa--last-command this-command))

(defun hayabusa-insert (&optional count char alt-char)
  (interactive)
  (let* ((count (or count current-prefix-arg))
	 (char (or char last-command-event))
	 (alt-char (or alt-char (hayabusa--alt-char char))))
    (cond ((hayabusa--should-insert-single-char-p count)
	   (hayabusa--insert-single-char char))
	  ((hayabusa--should-insert-alt-char-p count)
	   (hayabusa--insert-alt-char count (or alt-char char)))
	  ((hayabusa--should-replace-char-p count char alt-char)
	   (hayabusa--replace-char alt-char))
	  (t (hayabusa--insert-char count char)))
    (setq hayabusa--last-command-event char))
  (hayabusa--set-clear-last-command-with-idle-timer))

(defun hayabusa-mode-enable ()
  (interactive)
  (add-hook 'post-command-hook 'hayabusa--copy-last-command)
  (when hayabusa-insert-enable-when-isearch-mode
    (define-key isearch-mode-map [remap isearch-printing-char] #'hayabusa-insert))
  (hayabusa--set-key-bindings)
  (hayabusa-mode t))

(defun hayabusa--set-key-bindings ()
  (dolist (i (number-sequence ?\s ?\377))
    (define-key hayabusa-mode-map (vector i) #'hayabusa-insert)))

(defun hayabusa--alt-char (char)
  (let* ((alt-char (or (cdr (assoc char hayabusa-keys-alist))
		       (cdr (assoc (string char) hayabusa-keys-alist)))))
    (cond ((stringp alt-char) (string-to-char alt-char))
	  (t alt-char))))

(defun hayabusa--should-insert-single-char-p (count)
  (and (consp count)
       (eq 1 (% (round (log (abs (prefix-numeric-value count)) 4)) 2))))

(defun hayabusa--insert-single-char (char)
  (hayabusa--insert-command 1 char))

(defun hayabusa--should-insert-alt-char-p (count)
  (or (consp count)
      (> 0 (prefix-numeric-value count))))

(defun hayabusa--insert-alt-char (count alt-char)
  (when (consp count)
    (setq count 1))
  (hayabusa--insert-command (abs (prefix-numeric-value count)) alt-char))

(defun hayabusa--preceding-char-in-isearch ()
  (when (and isearch-mode (> (length isearch-string) 0))
    (string-to-char (substring isearch-string -1))))

(defun hayabusa--should-replace-char-p (count char alt-char)
  (let* ((preceding-char (or (hayabusa--preceding-char-in-isearch)
			     (preceding-char))))
    (and (not count)
	 alt-char
	 (eq preceding-char char)
	 (eq hayabusa--last-command-event char)
 	 (eq hayabusa--last-command #'hayabusa-insert))))

(defun hayabusa--replace-char (alt-char)
  (hayabusa--delete-command)
  (hayabusa--insert-command 1 alt-char))

(defun hayabusa--insert-char (count char)
  (hayabusa--insert-command (abs (prefix-numeric-value count)) char))

(defun hayabusa--delete-command ()
  (set--this-command-keys (kbd "DEL")) ; for term-char-mode
  (call-interactively (or (if isearch-mode #'isearch-del-char)
			  (hayabusa--lookup-key (kbd "DEL"))
			  #'delete-backward-char)))

(defun hayabusa--insert-command (count char)
  (set--this-command-keys (string char)) ; for term-char-mode
  (let* ((current-prefix-arg count)
	 (last-command-event char))
    (call-interactively (or (if isearch-mode #'isearch-printing-char)
			    (hayabusa--lookup-key (vector last-command-event))
			    #'self-insert-command))))

(defun hayabusa--lookup-key (key)
  (or (if (get-text-property (point) 'local-map)
	  (lookup-key (get-char-property (point) 'local-map) key)
	(or (command-remapping (lookup-key (current-local-map) key))
	    (lookup-key (current-local-map) key)))
      (command-remapping (lookup-key (current-global-map) key))
      (lookup-key (current-global-map) key)))


(provide 'hayabusa)



;;; hayabusa.el ends here
