
; Author: Ritchie Turner (blackdog@cloudshift.cl)
; Notes:
;    You need to fire up your haxe server with haxe --wait 6000 (change hxc-port accordingly)
;    Still need to match help.
;    Parse type on ( seems to be redundant as its provided in method sig anyway.

(defvar hxc-hxml nil)
(defvar hxc-char ".")
(defvar hxc-raw nil)
(defvar hxc-eol "\n")
(defvar hxc-got-resp nil)
(defvar hxc-port 6000)

(defun hxc-init (p)
  (setq hxc-hxml nil)
  (if (hxc-read-hxml)
      (hxc-msg (format "Welcome, new project is \"%s\"\nClasspath: %s" (car p) hxc-hxml))))

(defun hxc-complete (char)
  (setq hxc-char char)
  (insert char)
  (backward-char)
  (save-buffer)
  (hxc-connect))

(defun hxc-process-filter (proc raw)
  (setq hxc-raw (concat raw hxc-raw))
  (when (string-match "<\/list>" hxc-raw)
    (setq hxc-got-resp t)))

(defun hxc-connect ()
  (let* ((fn (hxc-file-name))
         (pkg (hxc-package))
         (cmd (hxc-build-compile-string pkg (buffer-name))))

    (setq hxc-raw "")
    (setq hxc-got-resp nil)

    (let ((proc (open-network-stream "hxccomp" nil "127.0.0.1" hxc-port)))
      (set-process-filter proc 'hxc-process-filter)
      (process-send-string proc cmd)
      (process-send-string proc "\000")
      (process-send-eof proc)
      (catch 'loop
        (dotimes (i 3)
          (accept-process-output proc 0.3)
          (when hxc-got-resp
            (delete-process proc)
            (hxc-process-raw)
            (throw 'loop i)
            )))
      (unless hxc-got-resp
        (delete-process proc))
      )))
          
(defun hxc-process-raw ()
  (when hxc-raw
    (if (string= hxc-char ".")
        (hxc-complete-dot hxc-raw)
      (hxc-complete-paren hxc-raw))))

(defun hxc-complete-dot (raw)
  (let ((methodlist (hxc-parse-methods raw)))
    (when (length methodlist)
      (let ((selection (ido-completing-read "-> "
                                            (mapcar (lambda (el) (car el)) methodlist))))
        (forward-char)
        (when selection
          (let ((sig (hxc-lookup-signature selection methodlist))
                (help (hxc-lookup-help selection methodlist)))
            (message  sig)
            (hxc-msg (concat sig "\n\n" help ))
            (insert (hxc-modify-by-sig sig selection))))))))

(defun hxc-lookup-signature (selection methodlist)
  (replace-regexp-in-string "&lt;" "<"
                            (replace-regexp-in-string "&gt;" ">"
                                                      (cadr (assoc selection methodlist)))))

(defun hxc-lookup-help (selection methodlist)
  (cadr (cdr (assoc selection methodlist))))

(defun hxc-modify-by-sig (sig selection)
  (let ((ss (split-string sig "->" t)))
    (cond
     ((string= (hxc-chomp (car ss)) "Void") (concat selection "()"))
     (t selection))))

(defun hxc-complete-paren (raw)
  (forward-char)
  (let* ((contents (hxc-parse-type raw))
         (byline (replace-regexp-in-string "\\([a-zA-Z0-9]+\\s-:\\)" "\n\\1"
                     (replace-regexp-in-string "&lt;" "<"
                            (replace-regexp-in-string "&gt;" ">" contents)))))
    (hxc-msg
     (mapconcat (lambda (el)
                  (replace-regexp-in-string "->\s*$" "" el))
                (split-string byline "\n") "\n"))))
    
(defun hxc-msg (text)
  (with-current-buffer (get-buffer-create "*haXe*")
    (erase-buffer)
    (insert text)))

(defun hxc-build-cwd ()
  (concat "--cwd "
          (expand-file-name (substring (hxc-prj-dir) 0 -1))
          hxc-eol))

(defun hxc-build-compile-string (pkg tmpFile)
  (concat
   (hxc-build-cwd)
   (hxc-conditional-comps)
   (hxc-read-hxml)
   (concat "-main " (hxc-klass pkg) hxc-eol)
   (concat "--display " tmpFile "@" (number-to-string (point)) hxc-eol)))

(defun hxc-conditional-comps ()
  (let ((bs (buffer-string)))
       (when (string-match "hxc:\\s-\\(.*\\)" bs)
         (concat (match-string 1 bs) hxc-eol))))

(defun hxc-parse-methods (s)
  (delq nil
        (mapcar
         (lambda (el)
           (when (string-match "n=\"\\(.*\\)\"><t>\\(.*\\)<\/t>" el)
             (list (match-string 1 el) (match-string 2 el) (match-string 3 el) )))
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
    (let ((buildFile (concat (hxc-prj-dir) "build.hxml")))
      (if (file-exists-p buildFile)
        (with-temp-buffer
          (insert-file buildFile)
          (delete-non-matching-lines "^-cp\\|^-lib")
          (setq hxc-hxml
                (mapconcat 'identity
                           (delete-dups
                            (split-string (buffer-string) hxc-eol)) hxc-eol)))
        
        (hxc-msg (format "Build file does not exist:%s" buildFile)))))
  hxc-hxml)
  
(defun hxc-prj-dir ()
  (cadr prj-current))

(defun hxc-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(provide 'hxc-complete)