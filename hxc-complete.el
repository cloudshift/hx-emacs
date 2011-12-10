
(defvar hxc-hxml nil)

(defun hxc-init (p)
  (setq hxc-hxml nil)
  (if (hxc-read-hxml)
      (hxc-msg (format "Welcome, new project is \"%s\"\nClasspath: %s" (car p) hxc-hxml))))

(defun hxc-complete (char)
  (insert char)
  (backward-char)
  (hxc-get-info char))

(defun hxc-get-info (char)
    (save-buffer)
    (let ((raw (hxc-get-raw)))
      (when raw
        (if (string= char ".")
            (hxc-complete-dot raw)
          (hxc-complete-paren raw)))))

(defun hxc-get-raw ()
  (let* ((fn (hxc-file-name))
         (pkg (hxc-package))
         (cmd (hxc-build-compile-string pkg (buffer-name))))
    (hxc-msg cmd)
    (shell-command-to-string cmd)))

(defun hxc-complete-dot (raw)
  (let ((mylist (hxc-parse-methods raw)))
      (when (length mylist)
        (progn
          (let ((selection (ido-completing-read "-> "
            (mapcar (lambda (el) (car el)) mylist))))
          (forward-char)
          (when selection
            (insert selection)))))))

(defun hxc-complete-paren (raw)
  (forward-char)
  (let* ((contents (hxc-parse-type raw))
         (byline (replace-regexp-in-string "\\([a-zA-Z0-9]+\\s-:\\)" "\n\\1"
                               (replace-in-string
                                (replace-in-string contents "&gt;" ">") "&lt;" "<"))))
    (hxc-msg
     (mapconcat (lambda (el)
                  (replace-regexp-in-string "->\s*$" "" el))
                (split-string byline "\n") "\n"))))
    
(defun hxc-msg (text)
  (with-current-buffer (get-buffer-create "*haXe*")
    (erase-buffer)
    (insert text)))

(defun hxc-build-compile-string (pkg tmpFile)
  (concat
   (format "cd %s && " (hxc-prj-dir)) 
   "haxe "
   " --cache prj.cache "
   (hxc-conditional-comps)
   (hxc-klass pkg)
   (hxc-read-hxml)
   " --display " tmpFile "@" (number-to-string (point))))

(defun hxc-conditional-comps ()
  (let ((bs (buffer-string)))
       (when (string-match "hxc:\\s-\\(.*\\)" bs)
         (concat (match-string 1 bs) " "))))

(defun hxc-parse-methods (s)
  (delq nil
        (mapcar
         (lambda (el)
           (when (string-match "n=\"\\(.*\\)\"><t>\\(.*\\)<\/t>" el)
             (list (match-string 1 el) (match-string 2 el))))
         (split-string s "\n"))))

(defun hxc-parse-type (s)
  (when (string-match "<type>\\(.*\n\\)*</type>" s)
    (match-string 1 s)))

(defun hxc-klass (pkg)
  (concat (when pkg (concat pkg "."))
          (file-name-nondirectory
           (file-name-sans-extension (buffer-name)))))

(defun hxc-package ()
  (let ((bs (buffer-string)))
  (when (string-match "package\\s-\\(.*?\\);" bs)
    (match-string 1 bs))))

(defun hxc-rel-path (cp)
  (replace-regexp-in-string "\\." "/" cp))

(defun hxc-file-name ()
  (file-name-nondirectory (buffer-name)))

(defun hxc-read-hxml ()
  (unless hxc-hxml
    (let ((buildFile (concat (hxc-prj-dir) "/build.hxml")))
      (if (file-exists-p buildFile)
        (with-temp-buffer
          (insert-file buildFile)
          (delete-non-matching-lines "^-cp\\|^-lib")
          (setq hxc-hxml
                (concat " "
                        (mapconcat 'identity
                                   (delete-dups
                                    (split-string (buffer-string) "\n")) " "))))
        (hxc-msg (format "Build file does not exist:%s" buildFile)))))
  hxc-hxml)
  
(defun hxc-prj-dir ()
  (cadr prj-current))

(provide 'hxc-complete)