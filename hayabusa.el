;;; hayabusa.el --- hayabusa -*- lexical-binding: t; coding:utf-8 -*-


;;; Code:

(defgroup hayabusa nil
  "Hayabusa minor mode."
  :group 'convenience)

(defcustom hayabusa-keys-alist '(())
  ""
  :group 'hayabusa
  :type 'alist)

(defvar hayabusa-mode-map
  (let* ((map (make-sparse-keymap)))
    (dolist (i (number-sequence ?\s ?\377))
      (define-key map (vector i) #'hayabusa-insert))
    map))

(defcustom hayabusa-insert-enable-when-isearch-mode t
  ""
  :group 'hayabusa
  :type 'boolean)

;;; command

(define-minor-mode hayabusa-mode
  "hayabusa"
  :global t
  :keymap hayabusa-mode-map
  :lighter " ﾊﾔﾌﾞｻ")

(defun hayabusa-insert ()
  (interactive)
  (dolist (func (hayabusa--insert-function-query))
    (funcall func)))

(defun hayabusa--insert-self-char-command ()
  (hayabusa--insert-command))

(defun hayabusa--insert-single-self-char-command ()
  (let* ((current-prefix-arg 1))
    (hayabusa--insert-command)))

(defun hayabusa--insert-alt-char-command ()
  (let* ((last-command-event (hayabusa--alt-char last-command-event))
	 (current-prefix-arg (abs (prefix-numeric-value current-prefix-arg))))
    (hayabusa--insert-command)))

(defun hayabusa--insert-single-alt-char-command ()
  (let* ((current-prefix-arg 1)
	 (last-command-event (hayabusa--alt-char last-command-event)))
    (hayabusa--insert-command)))

(defun hayabusa--insert-command ()
  (if isearch-mode (call-interactively #'isearch-printing-char)
    (set--this-command-keys (string last-command-event)) ; for term-char-mode
    (call-interactively (or (hayabusa--lookup-key (vector last-command-event))
			    #'self-insert-command))))

(defun hayabusa--delete-backward-char-command ()
  (if isearch-mode (call-interactively #'isearch-del-char)
    (set--this-command-keys (kbd "DEL")) ; for term-char-mode
    (call-interactively (or (hayabusa--lookup-key (kbd "DEL"))
			    #'delete-backward-char))))

(defun hayabusa-mode-enable ()
  (interactive)
  (add-hook 'isearch-mode-hook #'hayabusa--isearch-mode-hook)
  (hayabusa-mode t))

(defun hayabusa--isearch-mode-hook ()
  (if hayabusa-insert-enable-when-isearch-mode
      (define-key isearch-mode-map [remap isearch-printing-char] #'hayabusa-insert)
    (define-key isearch-mode-map [remap isearch-printing-char] nil)))

;;; query
(defun hayabusa--insert-function-query ()
  (or (hayabusa--replace-backward-char-query)
      (hayabusa--insert-char-query)))

(defun hayabusa--replace-backward-char-query ()
  (when (hayabusa--should-replace-backward-char-p)
    (list #'hayabusa--delete-backward-char-command
	  #'hayabusa--insert-single-alt-char-command)))

(defun hayabusa--insert-char-query ()
  (cond ((hayabusa--should-insert-self-char-p)
	 (list #'hayabusa--insert-self-char-command))
	((hayabusa--should-insert-single-self-char-p)
	 (list #'hayabusa--insert-single-self-char-command))
	((hayabusa--should-insert-alt-char-p)
	 (list #'hayabusa--insert-alt-char-command))
	((hayabusa--should-insert-single-alt-char-p)
	 (list #'hayabusa--insert-single-alt-char-command))))

(defun hayabusa--alt-char (char)
  (let* ((alt-char (or (cdr (assoc char hayabusa-keys-alist))
		       (cdr (assoc (string char) hayabusa-keys-alist)))))
    (or (when (stringp alt-char) (string-to-char alt-char))
	alt-char)))

(defun hayabusa--preceding-char-in-isearch ()
  (when (and isearch-mode (> (length isearch-string) 0))
    (string-to-char (substring isearch-string -1))))

(defun hayabusa--lookup-key (key)
  (let* ((command (or (if (get-text-property (point) 'local-map)
			  (lookup-key (get-char-property (point) 'local-map) key)
			(lookup-key (current-local-map) key))
		      (lookup-key (current-global-map) key))))
    (or (command-remapping command)
	command)))


;;; predicate
(defun hayabusa--should-replace-backward-char-p ()
  (let* ((preceding-char (or (hayabusa--preceding-char-in-isearch)
			     (preceding-char))))
    (and (null current-prefix-arg)
	 (hayabusa--alt-char last-command-event)
	 (eq preceding-char last-command-event)
	 (eq last-command this-command))))

(defun hayabusa--should-insert-self-char-p ()
  (and (atom current-prefix-arg)
       (wholenump (prefix-numeric-value current-prefix-arg))))

(defun hayabusa--should-insert-single-self-char-p ()
  (and (consp current-prefix-arg)
       (wholenump (prefix-numeric-value current-prefix-arg))
       (not (zerop (% (truncate (log (abs (prefix-numeric-value current-prefix-arg)) 4)) 2)))))

(defun hayabusa--should-insert-alt-char-p ()
  (and (atom current-prefix-arg)
       (not (wholenump (prefix-numeric-value current-prefix-arg)))))

(defun hayabusa--should-insert-single-alt-char-p ()
  (and (consp current-prefix-arg)
       (or (not (wholenump (prefix-numeric-value current-prefix-arg)))
	   (zerop (% (truncate (log (abs (prefix-numeric-value current-prefix-arg)) 4)) 2)))))

(provide 'hayabusa)

;;; hayabusa.el ends here
