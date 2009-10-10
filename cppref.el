;; Install
;;
;; (require 'cppref-mode)
;; (add-hook 'c++-mode-hook
;; 	  #'(lambda ()	      
;;            (setq cppref-docroot "/usr/share/doc/cppref")
;;            (cppref-add-docdir-list "/usr/share/doc/libboost-doc/HTML/doc/html")
;;	      (define-key c++-mode-map "\M-h" 'cppref)))
;;

(eval-when-compile
  (require 'w3m-load))

(defvar cppref-docroot nil)
(defvar cppref-docdir-list '())
(defvar cppref-index-alist '())

(defvar cppref-mode-map nil)
(unless cppref-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "q" 'cppref-quit)
    (define-key map "\r" 'cppref-select-choice)
    (setq cppref-mode-map map)))

(defun cppref-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map cppref-mode-map)
  (setq major-mode 'cppref-mode)
  (setq mode-name "*cppref*")
  (run-hooks 'cppref-mode-hook))

(defun cppref-set-select-property (begin end property)
  (put-text-property begin 
		     end 
		     'path
		     property))

(defun cppref-extract-func (pair)
  (let ((str ""))
    (setq str (substring (cdr pair) (+ 1 (length (car pair)))))
    (setq str (replace-regexp-in-string "\\.html" "" str))
    (let ((fields (split-string str "/"))
	  (flag t))
      (concat 
       " - "
       (mapconcat 
	(lambda (factor)
	  (concat 
	   (if flag (progn (setq flag nil) "") " >> ")
	   factor)) fields "")))))


(defun cppref-select-write-buf (found)
  (insert "You are here: C++ Reference >> Search results\n")
  (mapc (lambda (choice)
	  (let ((begin (point))
		(output (cppref-extract-func choice)))
	    (insert output "\n")
	    (cppref-set-select-property begin (point) (cdr choice))))
	found))

(defun cppref-select-buf (found)
  (let ((buffer (get-buffer-create "*cppref*")))
    (set-buffer buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (cppref-select-write-buf found)
    (pop-to-buffer buffer)
    (goto-char (point-min))
    (setq buffer-read-only t)
    (cppref-mode)))

(defun cppref-select-choice ()
  (interactive)
  (let ((path (get-text-property (point) 'path)))
    (if path (cppref-w3m-open path)
      (cppref-w3m-open (concat cppref-docroot "/start.html")))))

(defun cppref-quit ()
  (interactive)
  (kill-buffer)
  (delete-window))

(defun cppref-w3m-open (path)
  (let ((buffer (current-buffer)))
    (unless (string= (buffer-name buffer) "*cppref*")
      (let ((new-buffer) (get-buffer-create "*cppref*"))
	(print (buffer-name buffer))
	(pop-to-buffer new-buffer)))
    (w3m-find-file path)))

(defun cppref-get-docroot ()
  (unless cppref-docroot
    (setq cppref-docroot (getenv "CPPREF_DOCROOT"))))

(defun cppref-dir-walker (dir func)
  (mapc
   (lambda (file) 
     (let ((path (concat dir "/" file)))
       (if (file-directory-p path)
	   (unless (or (string= file ".")
		       (string= file ".."))
	     (cppref-dir-walker path func))
	 (funcall func path))))
   (directory-files dir)))

(defun cppref-add-docdir-list (dir)
  (push dir cppref-docdir-list))

(defun cppref-make-index ()
  (cppref-get-docroot)
  (cppref-add-docdir-list cppref-docroot)
  (mapc 
   (lambda (docroot)
     (let ((docroot-index nil))
       (cppref-dir-walker 
	docroot
	(lambda (path)
	  (push path docroot-index)))
       (push (cons docroot docroot-index) cppref-index-alist)))
   cppref-docdir-list))

(defun cppref-search-func (func-name)
  (when (null cppref-index-alist) (cppref-make-index))
  (let (found '())
    (mapc (lambda (docroot)
	    (mapc
	     (lambda (path)
	       (when (string-match func-name path)
		 (push (cons (car docroot) path) found)))
	     (cdr docroot)))
	  cppref-index-alist)
    found))

(defun cppref-thing-at-point ()
  (let* ((begin (save-excursion (skip-chars-backward "a-zA-Z0-9_:") (point)))
	 (end (save-excursion (skip-chars-forward "a-zA-z0-9_:") (point)))
	 (func-name (buffer-substring begin end)))
    (when (string= func-name "") 
      (setq func-name (read-from-minibuffer "cppref function-name: ")))
    func-name))

(defun cppref ()
  (interactive)
  (let* ((func-name (cppref-thing-at-point))
	 (found (cppref-search-func func-name)))
    (cond ((null found) (princ "not found"))
	  ((< 1 (length found)) (cppref-select-buf found))
	  (t (cppref-w3m-open (cdr (car found)))))))


(provide 'cppref)
