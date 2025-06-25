;;; Time-stamp: <2025-06-25T14:00:54+0200 mpiotrow>
;;; -*- coding: utf-8 -*-

;;; Inspired by grab-mac-link and org-mac-link, but simpler (I only
;;; use Safari and the Finder) and using JavaScript for Applications
;;; (JXA) instead of AppleScript.  This allows us to get a structured
;;; value back from the application, so we don't have to construct a
;;; return string with a special marker and then remove it in Elisp.

(require 'bindat)

;;;

(setq mxp-mac-osa-scripts
      '((finder . "var Finder = Application('Finder');
var itemList = Finder.selection();
var ret = [];

itemList.forEach(
	item => { ret.push([ decodeURI(item.url()), item.displayedName() ]); }
);

ret;")
        (safari . "var Safari = Application('Safari');
var tab = Safari.windows[0].currentTab;

[ decodeURI(tab.url()), tab.name() ];")
        
        (fontbook . "
var FontBook = Application('Font Book');
var itemList = FontBook.selection();
var ret = [];

itemList.forEach(
	item => { ret.push([ item.postscriptName(),
		  item.familyName(),
		  item.styleName() ]);
		});
ret;")

        (mail . "
var Mail = Application('Mail');
var itemList = Mail.selection();
var ret = [];

itemList.forEach(
	// item => { ret.push([ 'message://' + item.messageId(), item.subject() ]); }
	item => { ret.push([ 'message:<' + item.messageId() + '>', item.subject() ]); }
);

ret;
")))

;;;

(defun mxp-mac-query-app (app)
  (mxp-mac-unpack
   (mac-osa-script (alist-get app mxp-mac-osa-scripts) "JavaScript" nil t)))

(defun mxp-mac-grab-link (app &optional link-type)
  "Prompt for an application to grab a link from.
When done, go grab the link, and insert it at point.

[FIXME] With a prefix argument, instead of \"insert\", save it to
kill-ring. For org link, save it to `org-stored-links', then
later you can insert it via `org-insert-link'.

[FIXME] If called from lisp, grab link from APP and return it (as a
string) with LINK-TYPE.  APP is a symbol and must be one of
'(safari finder), LINK-TYPE is also a symbol and must be one of
'(plain markdown org), if LINK-TYPE is omitted or nil, plain link
will be used."
  (interactive
   (let ((apps
          '((?s . safari)
            (?f . finder)
            (?m . mail)))
         (link-types
          '((?p . plain)
            (?m . markdown)
            (?o . org)
            (?h . html)
            (?l . latex)
            (?b . bibtex)))
         (propertize-menu
          (lambda (string)
            "Propertize substring between [] in STRING."
            (with-temp-buffer
              (insert string)
              (goto-char 1)
              (while (re-search-forward "\\[\\(.+?\\)\\]" nil 'no-error)
                (replace-match (format "[%s]" (propertize (match-string 1) 'face 'bold))))
              (buffer-string))))
         input app link-type)
     (let ((message-log-max nil))
       (message (funcall propertize-menu
                         "Grab link from [s]afari [f]inder [m]ail:")))
     (setq input (read-char-exclusive))
     (setq app (cdr (assq input apps)))
     (let ((message-log-max nil))
       ;; bibtex only really makes sense in bibtex-mode, so we don't list it here
       (message (funcall propertize-menu
                         (format "Grab link from %s as a [p]lain [m]arkdown [o]rg [h]tml [l]atex link:" app))))
     (setq input (read-char-exclusive))
     (setq link-type (cdr (assq input link-types)))
     (list app link-type)))

  (setq link-type
        (or link-type
            (cond ((derived-mode-p 'org-mode) 'org)
                  ((derived-mode-p 'markdown-mode) 'markdown)
                  ((derived-mode-p 'html-mode) 'html)
	          ((derived-mode-p 'latex-mode) 'latex)
                  ((derived-mode-p 'bibtex-mode) 'bibtex)
	          (t 'plain))))
  
  (unless (and (memq app '(safari finder mail))
               (memq link-type '(plain org markdown html latex bibtex)))
    (error "Unknown app %s or link-type %s" app link-type))

  ;; this is the original code from `grab-mac-link'
  ;; (let* ((grab-link-func (intern (format "grab-mac-link-%s-1" app)))
  ;;        (make-link-func (intern (format "grab-mac-link-make-%s-link" link-type)))
  ;;        (link (apply make-link-func (funcall grab-link-func))))
  ;;   (when (called-interactively-p 'any)
  ;;     (if current-prefix-arg
  ;;         (if (eq link-type 'org)
  ;;             (let* ((res (funcall grab-link-func))
  ;;                    (link (car res))
  ;;                    (desc (cadr res)))
  ;;               (push (list link desc) org-stored-links)
  ;;               (message "Stored: %s" desc))
  ;;           (kill-new link)
  ;;           (message "Copied: %s" link))
  ;;       (insert link)))
  ;;   link)

  ;; Need to think about the interfaces.
  (let ((response   (mxp-mac-query-app app))
        (insert-fun (intern (format "mxp-mac-grab-insert-%s" link-type))))
    (if (listp (car response))
        (dolist (item (mxp-mac-query-app app))
          (funcall insert-fun (car item) (cadr item)))
      (funcall insert-fun  (car response) (cadr response))
      )
    )
  )

;;; Utility functions

(defun mxp-mac-grab-insert-plain (url &optional desc)
  (push-mark)
  (insert url))

(defun mxp-mac-grab-insert-markdown (url desc)
  (push-mark)
  (insert "[" desc "](" url ")"))

(defun mxp-mac-grab-insert-org (url desc)
  (push-mark)
  (insert "[[" url "][" desc "]]"))

(defun mxp-mac-grab-insert-html (url desc)
  (push-mark)
  (insert "<a href=\"" url "\">" desc "</a>"))

(defun mxp-mac-grab-insert-latex (url &optional desc)
  ;; [TODO] Currently, when getting a file path, we try turning it
  ;; into a relative path.  However, \url{} doesn't really make sense
  ;; then.  Local paths are probably almost always images, but should
  ;; we just insert \includegraphics{}, or let the user choose?
  (let* ((path (string-remove-prefix "file://" url))
         (relative-path (string-remove-prefix
                         (url-basepath (buffer-file-name)) path)))
    (push-mark)
    (if desc
        (insert "\\href{" relative-path "}{" desc "}")    
      (insert "\\url{" relative-path "}"))))

(defun mxp-mac-grab-insert-bibtex (url &optional desc)
  (let* ((path (string-remove-prefix "file://" url))
         (filetype (cond
                    ((string-suffix-p ".pdf" path t) "PDF")
                    ((string-suffix-p ".djvu" path t) "DJVU")))
         (abbrev-path ""))
    (push-mark)
    (if (string-equal url path)         ; i.e., not a file URL
        (bibtex-make-field `("url" "" ,url nil) t t)
      (if (string-match (concat "\\(" abbreviated-home-dir "\\)") path)
          (setq abbrev-path (replace-match "~/" nil nil path 1)))
    
      (bibtex-make-field
       `("file" "" ,(concat ":" abbrev-path ":" filetype) nil) t t))))

(defun mxp-mac-unpack (obj)
  "Unpack the results from `mac-osa-script' when VALUE-FORM is t.

This function currently handles only lists of strings and longs."
(let ((type  (car obj))
        (value (cdr obj)))
    ;; lists
    (if (string-equal "list" type)
        (cl-map 'list
                (lambda (x) (mxp-mac-unpack x))
                value)
      ;; other types
      (cond
       ((string-equal "utxt" type)
        (decode-coding-string value 'utf-16le))
       ((string-equal "long" type)
        ;; Unsigned to signed
        (let* ((u32 (bindat-get-field
                     (bindat-unpack '((u u32r)) value) 'u)))
          (if (> u32 #x7fffffff)
              (logior -4294967296 u32) u32))
        ))
      )))

;;; This is already OK

(defun mxp-grab-file-relative ()
  "Return name (with path) of selected file or directory in Finder.

If the file or directory shares a path prefix with the current buffer,
return a relative name.

If more than one file is selected, just return the first one."

  (string-remove-prefix
   (url-basepath (buffer-file-name))
   (string-remove-prefix "file://" (caar (mxp-mac-query-app 'finder)))))

;;;

(provide 'mxp-mac-grab)
