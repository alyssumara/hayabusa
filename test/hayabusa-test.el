(require 'hayabusa)
(require 'ert)
(require 'ert-x)

(ert-deftest test-hayabusa-mode ()
  (with-temp-buffer
    (hayabusa-mode t)
    (should (eq t hayabusa-mode))))

(ert-deftest test-hayabusa-replace-char ()
  (with-temp-buffer
    (let* ((hayabusa-keys-alist '((?j . ?-))))
      (ert-simulate-command (list #'hayabusa-insert nil ?j))
      (should (string= "j" (buffer-substring (point-min) (point-max))))
      (ert-simulate-command (list #'hayabusa-insert nil ?j))
      (should (string= "-" (buffer-substring (point-min) (point-max))))
      (ert-simulate-command (list #'hayabusa-insert nil ?j))
      (should (string= "-j" (buffer-substring (point-min) (point-max))))
      (ert-simulate-command (list #'ignore))

      (should (string= "-jj" (buffer-substring (point-min) (point-max)))))))

(ert-deftest test-hayabusa-insert-alt-char ()
  (with-temp-buffer
    (let* ((hayabusa-keys-alist '((?j . ?-))))
      (ert-simulate-command (list #'hayabusa-insert '- ?j))
      (should (string= "-" (buffer-substring (point-min) (point-max))))
      (ert-simulate-command (list #'hayabusa-insert -1 ?j))
      (should (string= "--" (buffer-substring (point-min) (point-max))))
      (ert-simulate-command (list #'hayabusa-insert -2 ?j))
      (should (string= "----" (buffer-substring (point-min) (point-max)))))))

(ert-deftest test-hayabusa-self-insert ()
  (with-temp-buffer
    (let* ((hayabusa-keys-alist '((?j . ?-))))
      (ert-simulate-command (list #'hayabusa-insert 1 ?j))
      (should (string= "j" (buffer-substring (point-min) (point-max))))
      (ert-simulate-command (list #'hayabusa-insert 1 ?j))
      (should (string= "jj" (buffer-substring (point-min) (point-max)))))))

(ert-deftest test-hayabusa-insert-interactively ()
  (with-temp-buffer
    (let* ((hayabusa-keys-alist '((?j . ?-))))
      (let* ((last-command-event ?j)
	     (current-prefix-arg nil))
	(call-interactively #'hayabusa-insert))
      (should (string= "j" (buffer-substring (point-min) (point-max))))

      (let* ((last-command-event ?j)
	     (current-prefix-arg nil))
	(call-interactively #'hayabusa-insert))
      (should (string= "-" (buffer-substring (point-min) (point-max))))

      (let* ((last-command-event ?j)
	     (current-prefix-arg nil))
	(call-interactively #'hayabusa-insert))
      (should (string= "-j" (buffer-substring (point-min) (point-max))))

      (ert-simulate-command (list #'ignore))

      (let* ((last-command-event ?j)
	     (current-prefix-arg nil))
	(call-interactively #'hayabusa-insert))
      (should (string= "-jj" (buffer-substring (point-min) (point-max)))))))


(ert-deftest test-hayabusa-single-insert ()
  (with-temp-buffer
    (let* ((hayabusa-keys-alist '((?j . ?-))))
      (let* ((current-prefix-arg (list 4)))
	(ert-simulate-command (list #'hayabusa-insert nil ?j))
      (should (string= "j" (buffer-substring (point-min) (point-max)))))

      (let* ((current-prefix-arg (list 64))
	     (last-command-event ?j))
	(call-interactively #'hayabusa-insert))
      (should (string= "jj" (buffer-substring (point-min) (point-max)))))))

(ert-deftest test-hayabusa-insert-in-isearch ()
  (with-temp-buffer
    (let* ((hayabusa-keys-alist '((?j . ?-))))
      (call-interactively #'isearch-forward)
      (setq isearch-string "")
      (ert-simulate-command (list #'hayabusa-insert nil ?j))
      (should (string= "j" isearch-string))
      (ert-simulate-command (list #'hayabusa-insert nil ?j))
      (should (string= "-" isearch-string)))))

