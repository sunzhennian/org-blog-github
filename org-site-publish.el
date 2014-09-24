(require 's)
(require 'ox-publish)
(require 'org-site-utils)
(require 'org-site-hack)

(defun org-site-publish-get-base-files (base-dir &optional extension)
  (let ((org-publish-sitemap-requested nil)
        (org-publish-temp-files nil)
        (match (concat "^[^\\.].*\\.\\("
                       (if extension extension "org")
                       "\\)$")))
    (org-publish-get-base-files-1 base-dir t match "index" nil)
    org-publish-temp-files))


(defun org-site-generate-index-all (base-dir site-sub-dir)
  (let* ((sub-dir (file-name-as-directory
                   (expand-file-name site-sub-dir base-dir)))
         (absolute-org-files (org-site-publish-get-base-files sub-dir)))
    (setq path-title-mtime-org-files
          (mapcar (lambda (filename)
                    (list (concat site-sub-dir
                                  "/"
                                  (s-chop-prefix sub-dir filename))
                          (org-org-get-file-title filename)
                          (org-org-get-file-mtime filename)))
                  absolute-org-files))
    (setq path-title-mtime-org-files
          (sort path-title-mtime-org-files
                #'(lambda (path-title-mtime1 path-title-mtime2)
                    (not (time-less-p (nth 2 path-title-mtime1)
                                      (nth 2 path-title-mtime2))))))
   (setq n_list 20.0)
   (setq N_all  (length path-title-mtime-org-files))
   (setq N (ceiling (/ N_all n_list)))
   (unless (file-exists-p (expand-file-name "index/" base-dir))
      (make-directory (expand-file-name "index/" base-dir)))
   (setq i 0)
   (setq index-file "index.org")
    (while (< i N)
    (setq tmp-i i)
    (setq tmp-i (1+ tmp-i))
    (setq prefix (number-to-string tmp-i))
    (setq index-file-next (concat "index/index-"  prefix))
    (setq index-file-next-html (concat index-file-next ".html"))
    (setq index-file-next (concat index-file-next ".org"))
    (setq prefix (number-to-string (- tmp-i 2)))
    (if (equal i 1)
    (setq index-file-pre "index")
     (setq index-file-pre (concat "index/index-"  prefix)))
    (setq index-file-pre-html (concat index-file-pre ".html"))
    (setq j 0)
    (with-temp-buffer
      (insert "#+TITLE: Home\n")
      (insert "#+BEGIN_HTML \n <div class=\"span10\" style=\"margin-left:0px\">\n<table class=\"table table-striped table-hover \">\n<tbody>\n ")
      (while (< j n_list)
      (setq tmp (ceiling (+ (* n_list i) j)))
      (cond ( (< tmp N_all)
      (setq  path-title-mtime (nth tmp  path-title-mtime-org-files))
	(setq filenameHTML
                    (concat
                     (file-name-sans-extension (car path-title-mtime))
                     ".html" ))
	(insert (format "<tr>\n<td><a href=%s>%s</a></td>\n<td>%s</td></tr>" filenameHTML  (cadr path-title-mtime) (format-time-string "%Y-%m-%d" (caddr path-title-mtime))
        ))))
      (setq j (1+ j))
      )
      (insert "</tbody>\n</table>")
      (if (> N 1)
      (if (equal i 0) (insert (format "<ul class=\"pager\">\n<li><a href=%s>Next</a></li>\n</ul>\n" index-file-next-html))
	(if (equal i (1- N))
	     (insert (format "<ul class=\"pager\">\n<li><a href=%s>Previous</a></li>\n</ul>\n" index-file-pre-html))
	     (insert (format "<ul class=\"pager\">\n<li><a href=%s>Previous</a></li>\n<li><a href=%s>Next</a></li>\n</ul>\n" index-file-pre-html index-file-next-html)))))
      (insert "</div>\n#+END_HTML")
      (when (file-writable-p (concat base-dir index-file))
        (write-region (point-min)
                      (point-max)
                     (concat base-dir index-file))))
      (setq i (1+ i))
      (setq index-file index-file-next)
   )))
  
(defun org-site-generate-index (base-dir site-sub-dir)
  (let* ((sub-dir (file-name-as-directory
                   (expand-file-name site-sub-dir base-dir)))
         (sub-index (expand-file-name "index.org" sub-dir))
         (absolute-org-files (org-site-publish-get-base-files sub-dir)))
    (setq path-title-mtime-org-files
          (mapcar (lambda (filename)
                    (list (concat site-sub-dir
                                  "/"
                                  (s-chop-prefix sub-dir filename))
                          (org-org-get-file-title filename)
                          (org-org-get-file-mtime filename)))
                  absolute-org-files))
    (setq path-title-mtime-org-files
          (sort path-title-mtime-org-files
                #'(lambda (path-title-mtime1 path-title-mtime2)
                    (not (time-less-p (nth 2 path-title-mtime1)
                                      (nth 2 path-title-mtime2))))))
    (with-temp-buffer
      (insert (format "#+TITLE: %ss\n" (capitalize site-sub-dir)))
      (dolist (path-title-mtime path-title-mtime-org-files)
        (insert (format "- %s :: [[file:%s][%s]]\n"
                        (format-time-string "%Y-%m-%d" (caddr path-title-mtime))
                        (car path-title-mtime)
                        (cadr path-title-mtime))))
      (when (file-writable-p sub-index)
        (write-region (point-min)
                      (point-max)
                      sub-index)))))

(defun org-site-generate-tags (base-dir site-sub-dir)
    "Generate necessary \"tags.org\" for posts."
    (let* ((sub-dir (file-name-as-directory
                     (expand-file-name site-sub-dir base-dir)))
           (absolute-org-files (org-site-publish-get-base-files sub-dir))
           (tags (ht-create)))
      (dolist (org-file absolute-org-files)
        (dolist (tag (org-org-get-file-tags org-file))
          (let ((path-title-pair
                 (cons (concat site-sub-dir
                               "/"
                               (s-chop-prefix sub-dir org-file))
                       (org-org-get-file-title org-file)))
                (ht-tag (ht-get tags tag)))
            (ht-set tags tag (cons path-title-pair ht-tag)))))
     (setq tag-keys-origin (ht-keys tags))
     (setq tag-keys-origin (sort tag-keys-origin (lambda (a b) (string< (downcase a) (downcase b)))))
     (setq tag-keys tag-keys-origin)
    (setq tags-file (expand-file-name "tags.org" base-dir))
    (with-temp-buffer
         (insert "#+OPTIONS: num:nil toc:nil\n")
         (insert (format "#+TITLE: Tags\n** \n"))
          (dolist (tag tag-keys)
          (insert (format "- [[file:%s][%s (%s)]]\n" (concat (concat "tags/" tag) ".org") tag (length (ht-get tags tag)))))
        (when (file-writable-p tags-file)
          (write-region (point-min)
                        (point-max)
                        tags-file)))
     (unless (file-exists-p (expand-file-name "tags/" base-dir))
      (make-directory (expand-file-name "tags/" base-dir)))
   (dolist (tag (ht-keys tags))
       (setq tags-file (concat (concat base-dir "tags/")(concat tag ".org")))
       (with-temp-buffer
         (insert "#+OPTIONS: num:nil toc:nil\n")
         (insert (format "#+TITLE: Tags\n"))
         (insert (format "*** Tag: %s\n" tag))
         (dolist (path-title-pair (ht-get tags tag))
            (insert (format "- [[file:%s][%s]]\n"
                            (car path-title-pair)
                            (cdr path-title-pair))))
         (when (file-writable-p tags-file)
          (write-region (point-min)
                        (point-max)
                        tags-file))))
         ))

(defun org-site-generate-categories (base-dir site-sub-dir)
    "Generate necessary \"categories.org\" for posts."
    (let* ((sub-dir (file-name-as-directory
                     (expand-file-name site-sub-dir base-dir)))
           (categories-file (expand-file-name "categories.org" base-dir))
           (absolute-org-files (org-site-publish-get-base-files sub-dir))
           (categories (ht-create)))
      (dolist (org-file absolute-org-files)
        (let* ((path-title-pair
                (cons (concat site-sub-dir
                              "/"
                              (s-chop-prefix sub-dir org-file))
                      (org-org-get-file-title org-file)))
               (category (org-org-get-file-category org-file))
               (ht-category (ht-get categories category)))
          (if category
              (ht-set categories category (cons path-title-pair ht-category)))))
      (unless (file-exists-p (expand-file-name "categories" base-dir))
      (make-directory (expand-file-name "categories" base-dir)))
       (setq category-keys-origin (ht-keys categories))
     (setq category-keys (sort category-keys-origin (lambda (a b) (string< (downcase a) (downcase b)))))
      (with-temp-buffer
        (insert "#+OPTIONS: num:nil toc:nil\n")
        (insert (format "#+TITLE: Categories\n** \n"))
        (dolist (category category-keys)
          (insert (format "- [[file:%s][%s (%s)]]\n" (concat (concat "categories/" category) ".org") category (length (ht-get categories category))))
          (dolist (path-title-pair (ht-get categories category))
            ))
        (when (file-writable-p categories-file)
          (write-region (point-min)
                        (point-max)
                        categories-file)))
      (dolist (category (ht-keys categories))
       (setq category-file (concat (concat base-dir "categories/")(concat category ".org")))
       (with-temp-buffer
         (insert "#+OPTIONS: num:nil toc:nil\n")
         (insert (format "#+TITLE: Categories\n"))
         (insert (format "*** Category: %s\n" category))
         (dolist (path-title-pair (ht-get categories category))
            (insert (format "- [[file:%s][%s]]\n"
                            (car path-title-pair)
                            (cdr path-title-pair))))
         (when (file-writable-p category-file)
          (write-region (point-min)
                        (point-max)
                        category-file))))
       ))

(defun org-site-pre-publish (base-dir)
  (org-site-generate-index-all base-dir "post")
;;  (org-site-generate-tags base-dir "post")
  (org-site-generate-categories base-dir "post"))

(defun org-site-post-publish (base-dir)
  (let ((post-index (expand-file-name "index.org" base-dir))
;;        (tags (expand-file-name "tags.org" base-dir))
        (categories (expand-file-name "categories.org" base-dir)))
    (unless org-site-debug
;;      (delete-directory (expand-file-name "tags" base-dir) t)
      (delete-directory (expand-file-name "categories" base-dir) t)
      (delete-directory (expand-file-name "index" base-dir) t)
      (dolist (org-file (list post-index  categories))
        (delete-file org-file)))))

(defun org-site-publish7 (project-dir &optional republish localhost)
   ;; (list (read-directory-name "Project directory: " org-site-project-directory)
   ;;       (y-or-n-p "Force republish all? ")
   ;;       (unless (s-matches? "localhost" org-site-url)
   ;;         (y-or-n-p "Force publish in localhost mode? ")))
  (org-site-load-project project-dir)

  (unless org-site-html-publish-dir
    (setq org-site-html-publish-dir
          (expand-file-name "publish"
                            org-site-project-directory)))

  (if (file-exists-p org-site-html-publish-dir)
      (progn
        (unless (file-directory-p org-site-html-publish-dir)
          (error "%s exists but is not a directory"
                 org-site-html-publish-dir))
        (if republish
            (dolist (file-or-dir
                     (directory-files org-site-html-publish-dir t))
              ;; do not delete '.', '..' and '.git', '.gitignore'
              (unless (s-matches? "\\.$\\|\\.git" file-or-dir)
                (if (file-directory-p file-or-dir)
                    (delete-directory file-or-dir t)
                  (delete-file file-or-dir))))))
    (make-directory org-site-html-publish-dir))

  (if localhost
      (setq org-site-url "localhost"))

  (org-site-pre-publish project-dir)
  ;; Enable and activate the monkey-patched `org-export-as-html'.
  (ad-enable-advice 'org-export-as-html 'around 'org-site-export-as-html)
  (ad-activate 'org-export-as-html)
  (setq org-publish-project-alist
        `(("org-site"
           :components ("org-site-html" "org-site-static"))file-exists-p
          ("org-site-html"
           :base-directory ,project-dir
           :base-extension "org"
           :publishing-directory ,org-site-html-publish-dir
           :recursive t
           :publishing-function org-publish-org-to-html
           :export-with-tags nil
           :headline-levels 4
           :table-of-contents ,org-site-enable-toc
           :section-numbers t
           :sub-superscript nil
           :todo-keywords nil
           :convert-org-links t
           :author ,org-site-author-name
           :email ,org-site-author-email
           :creator-info nil
           :html-preamble nil
           :html-postamble nil
           :style nil
           :style-include-default nil
           :style-include-scripts nil
           :script nil
           :timestamp t
           :exclude-tags ("noexport" "todo")
           :auto-preamble t
           :preparation-function nil
           :completion-function nil)
          ("org-site-static"
           :base-directory ,"images"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|otf"
           :publishing-directory ,(file-name-as-directory
                                   (expand-file-name "images"
                                                     org-site-html-publish-dir))
           :recursive t
           :publishing-function org-publish-attachment)))
  (message "-------------->%s" republish)
  (org-publish-all republish)
  (ad-disable-advice 'org-export-as-html 'around 'org-site-export-as-html)
  (ad-deactivate 'org-export-as-html)
  (org-site-post-publish project-dir))


(defun org-site-publish8 (project-dir &optional republish localhost)
   ;; (list (read-directory-name "Project directory: " org-site-project-directory)
   ;;       (y-or-n-p "Force republish all? ")
   ;;       (unless (s-matches? "localhost" org-site-url)
  ;;         (y-or-n-p "Force publish in localhost mode? ")))
  (org-site-load-project project-dir)
  (unless org-site-html-publish-dir
    (setq org-site-html-publish-dir
          (expand-file-name "publish"
                            org-site-project-directory)))
 ; (if (file-exists-p "~/.org-timestamps")
 ;            (delete-directory "~/.org-timestamps" t))
  (if (file-exists-p org-site-html-publish-dir)
      (progn
        (unless (file-directory-p org-site-html-publish-dir)
          (error "%s exists but is not a directory"
                 org-site-html-publish-dir))
        (if republish
	     (dolist (file-or-dir
                     (directory-files org-site-html-publish-dir t))
              ;; do not delete '.', '..' and '.git', '.gitignore'
              (unless (s-matches? "\\.$\\|\\.git" file-or-dir)
                (if (file-directory-p file-or-dir)
                    (delete-directory file-or-dir t)
                  (delete-file file-or-dir))))))
    (make-directory org-site-html-publish-dir))

  (if localhost
      (setq org-site-url "localhost"))
  (org-site-pre-publish project-dir)
  ;; Enable and activate the monkey-patched `org-export-as-html'.
  (ad-enable-advice 'org-html-publish-to-html 'around 'org-site-export-to-html)
  (ad-activate 'org-html-publish-to-html)
  (setq org-publish-project-alist
        `(("org-site"
           :components ("org-site-html" "org-site-static" "org-site-images"))
          ("org-site-html"
	  :base-directory ,project-dir
         :base-extension "org"
         :publishing-directory ,org-site-html-publish-dir
         :recursive t
         :publishing-function org-html-publish-to-html
         :export-with-tags nil
         :headline-levels 4
         :html-preamble nil
         :html-postamble nil
	 :with-timestamps nil
	 :section-numbers nil
	 :with-author nil
	 :with-date nil
	 :with-creator nil)
          ("org-site-static"
           :base-directory ,(org-site-get-static-dir)
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|otf"
           :publishing-directory ,(file-name-as-directory
                                   (expand-file-name "static"
                                                     org-site-html-publish-dir))
           :recursive t
           :publishing-function org-publish-attachment)
	  ("org-site-images"
           :base-directory ,(expand-file-name "images" project-dir)
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|otf"
           :publishing-directory ,(file-name-as-directory
                                   (expand-file-name "images"
                                                     org-site-html-publish-dir))
           :recursive t
           :publishing-function org-publish-attachment)))
  (org-publish-all republish)
  (ad-disable-advice 'org-html-publish-to-html 'around 'org-site-export-to-html)
  (ad-deactivate 'org-html-publish-to-html)
  (org-site-post-publish project-dir))


(defun org-site-publish (project-dir &optional republish localhost)
(interactive
   (list (read-directory-name "Project directory: " org-site-project-directory)
         (y-or-n-p "Force republish all? ")
         (unless (s-matches? "localhost" org-site-url)
           (y-or-n-p "Force publish in localhost mode? "))))
(setq org-v (string-to-number (org-version)))
(if (< org-v 8)
(org-site-publish7 project-dir)
(org-site-publish8 project-dir)
))

(provide 'org-site-publish)

