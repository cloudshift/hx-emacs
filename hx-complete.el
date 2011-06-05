
(defvar hxml nil)

(defun hx-init (p)
  (setq hxml nil)
  (if (readHxml)
      (hx-msg (format "Welcome, new project is \"%s\"\nClasspath: %s" (car p) hxml))))

(defun hx-complete (char)
  (insert char)
  (backward-char)
  (hx-get-info char))

(defun hx-get-info (char)
    (save-buffer)
    (let ((raw (hx-get-raw)))
      (when raw
        (if (string= char ".")
            (hx-complete-dot raw)
          (hx-complete-paren raw)))))

(defun hx-get-raw ()
  (let* ((fn (fileName))
         (pkg (package))
         (cmd (hx-build-compile-string pkg (buffer-name))))
    (shell-command-to-string cmd)))

(defun hx-complete-dot (raw)
  (let ((mylist (parseMethods raw)))
      (when (length mylist)
        (progn
        
;          (insert (mapconcat (lambda (el) (apply 'format "%s %s" el)) mylist "\n")))
;        
;        (insert (mapconcat 'identity
;                (delq nil (mapcar (lambda (el)
;                    (when (string= selection (car el)) (cadr el))) mylist)) " ")))

        (let ((selection (ido-completing-read "-> "
              (mapcar (lambda (el) (car el)) mylist))))
          (forward-char)
          (when selection
            (insert selection)))))))

(defun hx-complete-paren (raw)
  (forward-char)
  (let* ((contents (parseType raw))
         (byline (replace-regexp-in-string "\\([a-zA-Z0-9]+\\s-:\\)" "\n\\1"
                               (replace-in-string
                                (replace-in-string contents "&gt;" ">") "&lt;" "<"))))
    (hx-msg
     (mapconcat (lambda (el)
                  (replace-regexp-in-string "->\s*$" "" el))
                (split-string byline "\n") "\n"))))
    
(defun hx-msg (text)
  (with-current-buffer (get-buffer-create "*haXe*")
    (erase-buffer)
    (insert text)))

(defun hx-build-compile-string (pkg tmpFile)
  (concat
   (format "cd %s && " (prjDir)) 
   "haxe "
   (klass pkg)
   (readHxml)
   " --display " tmpFile "@" (number-to-string (point))))

(defun parseMethods (s)
  (delq nil
        (mapcar
         (lambda (el)
           (when (string-match "n=\"\\(.*\\)\"><t>\\(.*\\)<\/t>" el)
             (list (match-string 1 el) (match-string 2 el))))
         (split-string s "\n"))))

(defun parseType (s)
  (when (string-match "<type>\\(.*\n\\)*</type>" s)
    (match-string 1 s)))

(defun klass (pkg)
  (concat (when pkg (concat pkg "."))
          (file-name-nondirectory
           (file-name-sans-extension (buffer-name)))))

(defun package ()
  (let ((bs (buffer-string)))
  (when (string-match "package\\s-\\(.*?\\);" bs)
    (match-string 1 bs))))

(defun rel-path (cp)
  (replace-regexp-in-string "\\." "/" cp))

(defun fileName ()
  (file-name-nondirectory (buffer-name)))

(defun readHxml ()
  (unless hxml
    (let ((buildFile (concat (prjDir) "build.hxml")))
      (if (file-exists-p buildFile)
        (with-temp-buffer
          (insert-file buildFile)
          (delete-non-matching-lines "^-cp.*")
          (setq hxml
                (concat " "
                        (mapconcat 'identity
                                   (delete-dups
                                    (split-string (buffer-string) "\n")) " "))))
        (hx-msg (format "Build file does not exist:%s" buildFile)))))
  hxml)
  
(defun prjDir ()
  (cadr prj-current))

(defun gen-haxe-docs ()
  (interactive)
  (cd (prjDir))
  (let ((cmd (concat
   "haxe "
   (klass (package))
   (readHxml)
   "-js blah.js"
   "--no-output"
   "-xml api.xml")))
    (hx-msg cmd)
    (shell-command-to-string cmd)))

;(defun prjDir ()
;  (car (delq nil
;        (mapcar
;         (lambda (el)
;           (if (string= (car el) prj-last-open)
;               (car (cdr el))
;             nil)
;           ) prj-list))))


(provide 'hx-complete)