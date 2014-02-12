(require 'org-site-utils)

(defadvice org-export-as-html (around org-site-export-as-html disable)
  (let* ((preamble (org-site-generate-preamble))
         (postamble (org-site-generate-postamble))
         (to-buffer 'string)
         (body-only nil)
         (opt-plist
          (org-export-process-option-filters
           (org-combine-plists (org-default-export-plist)
                               ext-plist
                               (org-infile-export-plist))))
         (title (org-html-expand
                 (or (plist-get opt-plist :title)
                     (and (not body-only)
                          (not
                           (plist-get opt-plist :skip-before-1st-heading))
                          (org-export-grab-title-from-buffer))
                     (and buffer-file-name
                          (file-name-sans-extension
                           (file-name-nondirectory buffer-file-name)))
                     "UNTITLED")))

         (html-extension (plist-get opt-plist :html-extension))
         (pub-dir (plist-get opt-plist :publishing-directory))
         (filename (expand-file-name
                    (concat
                     (file-name-sans-extension
                      (file-name-nondirectory buffer-file-name))
                     "." html-extension)))
         ; Avoid any auto-insert stuff for the new file
         (auto-insert nil)
         (base-url nil)
         (org-file-string (substring-no-properties (buffer-string))))

    ad-do-it

    (setq have-math (org-org-have-math org-file-string))
    (setq org-html (substring-no-properties (car kill-ring)))
    (setq content (org-html-get-body-content org-html))
    (setq toc (org-html-get-body-toc org-html))

    (setq ftname
          (concat (file-name-as-directory pub-dir)
                  (and (string-match (regexp-quote base-dir) filename)
                       (substring filename (match-end 0)))))

    (setq buffer (find-file-noselect ftname))
    (set-buffer buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (fundamental-mode)
    (org-install-letbind)
    (and (fboundp 'set-buffer-file-coding-system)
         (set-buffer-file-coding-system coding-system-for-write))
        (setq base-url
              "/"

              bootstrap-min-css (concat (concat "static/" org-site-theme) "/bootstrap.min.css")

              bootstrap-responsive-min-css (concat (concat "static/" org-site-theme) "/bootstrap-responsive.min.css")

              jquery-min-js "static/js/jquery-2.0.0.min.js"

              bootstrap-min-js "static/js/bootstrap.min.js")
    
	(let ((context
           (ht-from-plist
            `("title" ,title
              "base-url" ,base-url
              "bootstrap-min-css" ,bootstrap-min-css
              "bootstrap-responsive-min-css" ,bootstrap-responsive-min-css
              "jquery-min-js" ,jquery-min-js
              "bootstrap-min-js" ,bootstrap-min-js
              "have-math" ,have-math
              "preamble" ,preamble
              "content" ,content
              "postamble" ,postamble))))
      (ht-set context "enable-toc"
              (if (and toc org-site-enable-toc)
                  (list (ht-from-plist `("toc" ,toc)))
                nil))
      (if org-site-enable-comment
          (ht-set context
                  "comment"
                  (org-site-generate-comment)))
      (insert (org-site-render "page.html" context)))

    (save-buffer)
    (kill-buffer (current-buffer))))

(defadvice org-html-publish-to-html (around org-site-export-to-html disable)
   (let* ((preamble (org-site-generate-preamble))
         (postamble (org-site-generate-postamble))
         (title (org-publish-find-title filename))
         (html-extension "html")
	 (filenameOnly 
                    (concat
                     (file-name-sans-extension
                      (file-name-nondirectory filename))
                     ".html" )))
    ad-do-it
    (setq have-math (org-org-have-math filename))
    (setq org-html (substring-no-properties (car kill-ring)))
    (setq content (org-html-get-body-content org-html))
    (setq toc (org-html-get-body-toc org-html))
    (setq ftname
          (concat (file-name-as-directory pub-dir) filenameOnly))
    (setq buffer (find-file-noselect ftname))
    (delete-file ftname)
    (set-buffer buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (fundamental-mode)
   ; (org-install-letbind)
    (and (fboundp 'set-buffer-file-coding-system)
         (set-buffer-file-coding-system coding-system-for-write))
        (setq base-url
              "/"

              bootstrap-min-css (concat (concat "static/" org-site-theme) "/bootstrap.min.css")

             bootstrap-responsive-min-css (concat (concat "static/" org-site-theme) "/bootstrap-responsive.min.css")

              jquery-min-js "static/js/jquery-2.0.0.min.js"

             bootstrap-min-js "static/js/bootstrap.min.js")
    (let ((context
           (ht-from-plist
            `("base-url" ,base-url
              "bootstrap-min-css" ,bootstrap-min-css
              "bootstrap-responsive-min-css" ,bootstrap-responsive-min-css
              "jquery-min-js" ,jquery-min-js
              "bootstrap-min-js" ,bootstrap-min-js
              "have-math" ,have-math
              "preamble" ,preamble
              "content" ,content
              "postamble" ,postamble))))
      (ht-set context "enable-toc"
              (if (and toc org-site-enable-toc)
                  (list (ht-from-plist `("toc" ,toc)))
                nil))
      (if title
   	  (ht-set context "title" title))
      (ht-set context "enable-title" t)
      (cond ((equalp title "Home")(ht-set context "enable-title" nil)) ((equalp title "Tags")(ht-set context "enable-title" nil)) ((equalp title "Categories")(ht-set context "enable-title" nil)) )
      (if org-site-enable-comment
          (ht-set context
                  "comment"
                  (org-site-generate-comment)))
      (insert (org-site-render "page.html" context)))
    (save-buffer)
    (kill-buffer (current-buffer))))

(provide 'org-site-hack)
;;; org-site-hack.el ends here


