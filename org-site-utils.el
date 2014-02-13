(require 'cl)

(require 'ht)

(require 'mustache)

(require 'org-site-vars)

(defun file-to-string (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun mustache-file-render (file context)
  (mustache-render (file-to-string file) context))

(defun org-html-get-body (org-html)
  (let ((body-regexp "<body>\\(.\\|\n\\)*</body>"))
    (car (s-match body-regexp org-html))))

(defun org-org-get-file-title (org-file)
     (with-temp-buffer
      (insert-file-contents org-file)
      (setq opt-list (org-export--get-inbuffer-options)))
      (car(plist-get opt-list :title)))
     ; (setq opt-plist (org-infile-export-plist))
      ;(or (plist-get opt-plist :title)
       ; (file-name-sans-extension
        ; (file-name-nondirectory filename)))))

(defun org-html-get-body-toc (org-html)
  (let ((org-html-body (org-html-get-body org-html))
        (toc-regexp "<div id=\"table-of-.*\">\\(.\\|\n\\)*?</div>\n</div>"))
    (car (s-match toc-regexp org-html-body))))

(defun org-html-get-body-content (org-html)
  (let ((org-html-body (org-html-get-body org-html))
        (body-regexp "</?body?>")
        (title-regexp "<h1 class=\"title\">.*?</h1>")
        (toc-regexp "<div id=\"table-of-.*\">\\(.\\|\n\\)*?</div>")
        (text-toc-regexp "<div id=\"text-table-of-.*\">\\(.\\|\n\\)*?</div>"))
    (s-trim
     (reduce
      #'(lambda (regexp string)
          (replace-regexp-in-string regexp "" string))
      (list body-regexp title-regexp toc-regexp text-toc-regexp)
      :initial-value org-html-body
      :from-end t))))

(defun org-org-get-file-properties (org-file)
  (let ((org-file-string-list
         (s-lines (file-to-string org-file)))
        (prop-regexp "^#\\+\\(.*?\\):[ \t]+\\(.*\\)")
        (prop-dict (ht-create)))
    (dolist (line org-file-string-list)
      (setq match-data (s-match prop-regexp line))
      (setq prop-key (nth 1 match-data))
      (setq prop-value (nth 2 match-data))
      (ht-set prop-dict prop-key prop-value))
    (ht-remove prop-dict nil)
    prop-dict))

(defun org-org-get-file-tags (org-file)
  (let ((tags (ht-get (org-org-get-file-properties org-file)
                      "TAGS")))
    (if tags
        ;(s-split-words tags)
(split-string tags ";")
      nil)))

(defun org-org-get-file-mtime (org-file)
  (let* ((attrs (file-attributes org-file))
         (mtime (nth 5 attrs))
         (org-file-prop-dict (org-org-get-file-properties org-file))
         (org-file-date (ht-get org-file-prop-dict "DATE")))
    (if org-file-date
        (setq mtime
              (apply #'encode-time
                     (org-parse-time-string org-file-date))))
    mtime))

(defun org-org-have-math (org-file-or-string)
   (let ((org-file-string
         (if (file-exists-p org-file-or-string)
             (file-to-string org-file-or-string)
           org-file-or-string)))
    (-any? (lambda (e)
             (setq re (nth 1 e))
             (s-matches? re org-file-string))
           org-latex-regexps)))

(defun org-org-get-file-category (org-file)
   (let ((CATEGORY (ht-get (org-org-get-file-properties org-file)
                          "CATEGORY")))
    (if CATEGORY
        (s-trim CATEGORY)
      nil)))

(defun org-site-new-project (&optional project-directory)
   (interactive "GProject directory: ")
  (unless project-directory
    (setq project-directory default-directory))
  (unless (file-exists-p project-directory)
    (make-directory project-directory))
  (setq old-default-directory default-directory)
  (unwind-protect
      (progn
        (cd project-directory)
        (make-directory "post")
(make-directory "images")
(make-directory "sourcecodes")
        (copy-file (expand-file-name "org-site-config.el"
                                     org-site-load-directory)
                   project-directory)
        (cd old-default-directory))))

(defun org-site-load-project (&optional project-directory)
   (interactive
   (list (read-directory-name "Project directory: " org-site-project-directory)))
  (unless project-directory
    (setq project-directory default-directory))
  (setq old-default-directory default-directory)
  (unwind-protect
      (progn
        (cd project-directory)
        (load-file "org-site-config.el"))
    (cd old-default-directory))
  (setq org-site-project-directory project-directory))

(defun org-site-load-template (theme template)
  (expand-file-name
   (format "template/%s/%s" theme template)
   (or org-site-load-directory default-directory)))

(defun org-site-get-static-dir ()
  (file-name-as-directory
   (expand-file-name "static"
                     org-site-load-directory)))

(defun org-site-new-org-file (org-file &optional view-org-file)
  (if (file-exists-p org-file)
      (error "File already exists, please type a new file."))
  (let ((buffer (find-file-noselect org-file)))
    (set-buffer buffer)
    ;(org-insert-export-options-template)
    (insert "#+Title: temp\n")
    (insert "#+DATE: " (format-time-string "%Y-%m-%d"))
    (insert "\n")
    (insert "#+OPTIONS: toc:nil num:nil\n")
    (insert "#+CATEGORY: \n")
    (insert "#+TAGS: \n")
    (save-buffer)
    (if view-org-file
        (switch-to-buffer buffer)
      (kill-buffer buffer))))

 (defun org-site-new-post (org-file)
    (interactive
     (list (read-file-name
            "file name: "
             (concat org-site-project-directory (concat "post/" (format-time-string "%Y-%m-%d-"))
                               ))))
    (org-site-new-org-file org-file t))

(defun org-site-new-wiki (org-file)
  (interactive
   (list (read-file-name
          "file name: "
          (file-name-as-directory
           (expand-file-name "wiki"
                             org-site-project-directory)))))
  (org-site-new-org-file org-file))

(defun org-site-render (template context)
  (mustache-file-render
   (org-site-load-template org-site-template template)
   context))

(defun org-site-generate-preamble ()
  (let ((context
         (ht-from-plist
          `("site-title" ,org-site-title
            "site-url" ,org-site-url
            "nav-post" "post/"
            "nav-wiki" "wiki/"
            "nav-categories" "categories.html"
            "nav-tags" "tags.html"
            "nav-about" "about.html"
            "enable-google-search" ,org-site-enable-google-search))))
    (org-site-render "preamble.html" context)))

(defun org-site-generate-comment ()
  (let ((context
         (ht-from-plist
          `("disqus-shortname" ,org-site-disqus-shortname))))
    (org-site-render "comment.html" context)))

(defun org-site-generate-meta-info ()
  (let ((context
         (ht-from-plist
          `("post-date" "post-date"
            "update-date" "update-date"
            "tags" "tags"
            "author-name" ,org-site-author-name))))
    (org-site-render "meta-info.html" context)))

(defun org-site-generate-footer ()
  (let ((context
         (ht-from-plist
          `("author-email" ,org-site-author-email
            "author-name" ,org-site-author-name))))
    (org-site-render "footer.html" context)))

(defun org-site-generate-postamble ()
  (let ((context
         (ht-from-plist
          `("footer" ,(org-site-generate-footer)))))
    (if org-site-enable-meta-info
        (ht-set context
                "meta-info"
                (org-site-generate-meta-info)))
    (org-site-render "postamble.html" context)))

(provide 'org-site-utils)
