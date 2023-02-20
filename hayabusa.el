;;; hayabusa.el --- hayabusa -*- lexical-binding: t -*-


;;; Code:

(defgroup hayabusa nil
  ""
  :group 'convenience)

(defcustom hayabusa-keys-alist
  '(())
  ""
  :group 'hayabusa
  :type 'alist)

(defcustom hayabusa-mode-hooks ()
  ""
  :group 'hayabusa
  :type 'hooklist)

(defvar hayabusa-mode-map (make-sparse-keymap))

(define-minor-mode hayabusa-mode
  "hayabusa"
  :keymap hayabusa-mode-map
  :lighter " 隼")

(defcustom hayabusa-insert-delay 0.5
  ""
  :group 'hayabusa
  :type 'float)

(defvar hayabusa--pre-command nil)

(defvar hayabusa--clear-pre-command-with-idle-timer nil)

(defun hayabusa--set-clear-pre-command-with-idle-timer ()
  (when hayabusa-insert-delay
    (setq hayabusa--clear-pre-command-with-idle-timer
	  (run-with-idle-timer hayabusa-insert-delay nil #'hayabusa--clear-pre-command))))

(defun hayabusa--clear-pre-command ()
  (setq hayabusa--pre-command nil))

(defun hayabusa--copy-pre-command ()
  (setq hayabusa--pre-command this-command))

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
	  (t (hayabusa--insert-char count char))))
  (hayabusa--set-clear-pre-command-with-idle-timer))

(defun hayabusa-global-mode ()
  (interactive)
  (dolist (hook hayabusa-mode-hooks)
    (pcase hook
      ('isearch-mode-hook
       (define-key isearch-mode-map [remap isearch-printing-char] #'hayabusa-insert))
      (hook
       (add-hook hook 'hayabusa-mode))))
  (add-hook 'post-command-hook 'hayabusa--copy-pre-command)
  (hayabusa--set-key-bindings))

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
 	 (eq hayabusa--pre-command #'hayabusa-insert))))

(defun hayabusa--replace-char (alt-char)
  (hayabusa--delete-command)
  (hayabusa--insert-command 1 alt-char))

(defun hayabusa--insert-char (count char)
  (hayabusa--insert-command (abs (prefix-numeric-value count)) char))

(defun hayabusa--delete-command ()
  (set--this-command-keys (kbd "DEL")) ; for term-char-mode
  (call-interactively (or (if isearch-mode #'isearch-del-char)
			  (command-remapping (lookup-key (current-local-map) (kbd "DEL")))
			  (lookup-key (current-local-map) (kbd "DEL"))
			  (command-remapping (lookup-key (current-global-map) (kbd "DEL")))
			  (lookup-key (current-global-map) (kbd "DEL"))
			  #'delete-backward-char)))

(defun hayabusa--insert-command (count char)
  (set--this-command-keys (string char)) ; for term-char-mode
  (let* ((current-prefix-arg count)
	 (last-command-event char))
    (call-interactively (or (if isearch-mode #'isearch-printing-char)
			    (command-remapping (lookup-key (current-local-map) (vector last-command-event)))
			    (lookup-key (current-local-map) (vector last-command-event))
			    (command-remapping (lookup-key (current-global-map) (vector last-command-event)))
			    (lookup-key (current-global-map) (vector last-command-event))
			    #'self-insert-command))))

(provide 'hayabusa)

;;; hayabusa.el ends here
