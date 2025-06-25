;;; mxp-mac-grab.el --- Grab links from macOS applications and insert them into Emacs -*- lexical-binding: t -*-

;; Author: Michael Piotrowski <mxp@dynalabs.de>
;; Maintainer: Michael Piotrowski <mxp@dynalabs.de>
;; Description: Grab links from macOS applications and insert them into Emacs
;; Keywords: convenience hypermedia
;; Version: 0.1
;; URL: https://github.com/mxpiotrowski/mxp-mac-grab.el
;; Package-Requires: bindat

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides one primary interactive function,
;; `mxp-mac-grab-link', which obtains a link from certain macOS
;; applications and inserts it at point in a variety of formats.

;; This package requires Mitsuharu Yamamoto's Emacs Mac port (see URL
;; https://bitbucket.org/mituharu/emacs-mac/).

;; Supported applications:
;;
;; - Safari (the URL of the frontmost tab in the frontmost window)
;; - Finder (the filename (with path) of the currently selected file
;;   or directory in the frontmost window)
;; - Mail (message ID of the currently selected message)
;; - Preview (the filename (with path) of the document in the frontmost
;;   window)

;; The following link formats are supported:
;;
;; - plain:    https://www.wikipedia.org/
;; - markdown: [Wikipedia](https://www.wikipedia.org/)
;; - org:      [[https://www.wikipedia.org/][Wikipedia]]
;; - html:     <a href="https://www.wikipedia.org/">Wikipedia</a>
;; - bibtex:   url = {https://www.wikipedia.org/}
;;             file = {:wikipedia.pdf:PDF}
;;
;; There is only one interactive function, `mxp-mac-grab-link'.  It
;; first prompts for the app and then for the format of the link to
;; insert into the Emacs buffer.  Simply hitting return selects the
;; default format.  For Org, Markdown, HTML, LaTeX, and BibTeX
;; buffers, the default is to insert a link in the corresponding
;; format.  For BibTeX, this is either a file or a url field.  For all
;; other modes, the default is plain.

;; This package is inspired by the grab-mac-link (URL
;; `https://github.com/xuchunyang/grab-mac-link.el') and org-mac-link
;; (URL `https://gitlab.com/aimebertrand/org-mac-link') packages.  The
;; main differences are:
;;
;; - it only supports Safari, Finder, Mail, and Preview (this is all I need);
;; - it uses JavaScript for Applications (JXA) instead of AppleScript.
;;   This allows us to get a structured value back from the
;;   application, so we don't have to construct a return string with a
;;   special marker and then remove it in Elisp.
;;
;; You can select multiple files in Finder or messages in Mail, but
;; currently you just get all links on a single line.

;;; Code:

(require 'bindat)

;;; The JXA scripts

(defvar mxp-mac-osa-scripts
  '((finder . "
var Finder = Application('Finder');
var itemList = Finder.selection();
var ret = [];

itemList.forEach(
	item => { ret.push([ decodeURI(item.url()), item.displayedName() ]); }
);

ret;")

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

ret;")
    
    (safari . "
var Safari = Application('Safari');
var tab = Safari.windows[0].currentTab;

[ decodeURI(tab.url()), tab.name() ];")
        
    (preview . "
var app = Application('Preview');
var doc = app.windows[0].document();
[ 'file://' + doc.path(), doc.name() ];"))

  "Plist of JXA scripts used by `mxp-mac-query-app' to obtain a link
to the current item.")

;;; Functions

(defun mxp-mac-query-app (app)
  "Query macOS application APP using the corresponding script from `mxp-mac-osa-scripts'.

Return one or more links (in the latter case, as a list).  Each
link is a list, where the car is the URL and the cdr is the
description."
  (mxp-mac-unpack
   (mac-osa-script (alist-get app mxp-mac-osa-scripts) "JavaScript" nil t)))

(defun mxp-mac-grab-link (app &optional link-format)
  "Prompt for an application to grab a link from.
When done, go grab the link, and insert it at point.

If called from lisp, grab link from APP and return it (as a
string) with LINK-FORMAT.  APP is a symbol and must be one of
'(safari finder mail preview), LINK-FORMAT is also a symbol and
must be one of '(plain markdown org html latex bibtex), if
LINK-FORMAT is omitted or nil, plain link will be used."
  (interactive
   (let ((apps
          '((?s . safari)
            (?f . finder)
            (?m . mail)
            (?p . preview)))
         (link-formats
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
         input app link-format)
     (let ((message-log-max nil))
       (message (funcall propertize-menu
                         "Grab link from [s]afari [f]inder [m]ail [p]review:")))
     (setq input (read-char-exclusive))
     (setq app (cdr (assq input apps)))
     (let ((message-log-max nil))
       ;; bibtex only really makes sense in bibtex-mode, so we don't list it here
       (message (funcall propertize-menu
                         (format "Grab link from %s as a [p]lain [m]arkdown [o]rg [h]tml [l]atex link:" app))))
     (setq input (read-char-exclusive))
     (setq link-format (cdr (assq input link-formats)))
     (list app link-format)))

  (setq link-format
        (or link-format
            (cond ((derived-mode-p 'org-mode) 'org)
                  ((derived-mode-p 'markdown-mode) 'markdown)
                  ((derived-mode-p 'html-mode) 'html)
	          ((derived-mode-p 'latex-mode) 'latex)
                  ((derived-mode-p 'bibtex-mode) 'bibtex)
	          (t 'plain))))
  
  (unless (and (memq app '(safari finder mail preview))
               (memq link-format '(plain org markdown html latex bibtex)))
    (error "Unknown app %s or link-format %s" app link-format))

  ;; [TODO] Think about the interfaces.
  (let ((response   (mxp-mac-query-app app))
        (insert-fun (intern (format "mxp-mac-grab-insert-%s" link-format))))
    (if (listp (car response))
        (dolist (item (mxp-mac-query-app app))
          (funcall insert-fun (car item) (cadr item)))
      (funcall insert-fun  (car response) (cadr response)))))

;;; Convenience function useful for writing small utility functions

(defun mxp-grab-file-relative ()
  "Return name (with path) of selected file or directory in Finder.

If the file or directory shares a path prefix with the current buffer,
return a relative name.

If more than one file is selected, just return the first one."

  (string-remove-prefix
   (url-basepath (buffer-file-name))
   (string-remove-prefix "file://" (caar (mxp-mac-query-app 'finder)))))

;;; Formating functions

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

;;; OSA helper function

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
              (logior -4294967296 u32) u32)))))))

(provide 'mxp-mac-grab)

;;; mxp-mac-grab.el ends here

