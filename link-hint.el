;; link-hint.el --- Use avy to open, copy, etc. visible links. -*- lexical-binding: t -*-

;; Author: Fox Kiester <noct@openmailbox.org>
;; URL: https://github.com/noctuid/link-hint.el
;; Keywords: convenience url avy link links hyperlink
;; Package-Requires: ((avy "0.3.0") (emacs "24.1") (cl-lib "0.5"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This packages provides commands for operating on visible links with avy. It
;; is inspired by link hinting from vim-like browsers and browser plugins such
;; as pentadactyl. For example, `link-hint-open-link' will use avy to select and
;; open a visible link. A link is not limited to a url but can also be a file
;; link, button, org link, info link, help link, mu4e attachment, mailto
;; address, etc. Commands are also provided for copying links to the kill ring
;; (and optionally the clipboard and/or primary) and for opening multiple links
;; at once like with pentadactyl's "g;" or qutebrowser's "--rapid" flag. It is
;; possible for the user to add both new link types and new link actions.

;; For more information, see the link-hint.el README/documentation.

;;; Code:
(require 'cl-lib)
(require 'avy)
(require 'url-util)
(require 'browse-url)
(require 'goto-addr)
(require 'ffap)
(require 'rx)

;; * Options
(defgroup link-hint nil
  "Provides commands for operating on visible links with avy."
  :group 'convenience
  :prefix 'link-hint-)

(defcustom link-hint-types
  '(link-hint-shr-url
    ;; mode specific
    link-hint-org-link
    link-hint-mu4e-url
    link-hint-mu4e-attachment
    link-hint-help-link
    link-hint-info-link
    link-hint-package-link
    link-hint-package-keyword-link
    link-hint-package-install-link
    link-hint-compilation-link
    link-hint-w3m-link
    link-hint-woman-button
    ;; link-hint-customize-widget
    ;; generic
    link-hint-button
    link-hint-text-url
    link-hint-file-link)
  "Link types to check for."
  :group 'link-hint
  :type '(repeat :tag "Link type" symbol))

(defcustom link-hint-action-messages
  '(:copy "Copied"
    :open "Opened"
    :browse-url "Browsed")
  "Plist of action to description message pairs."
  :group 'link-hint
  :type 'list)

(defcustom link-hint-message #'message
  "The funtion to use to message information or nil."
  :group 'link-hint
  :type '(choice
          (function :tag "Function to use to message")
          (const :tag "Don't message" nil)))

(defcustom link-hint-url-regexp
  goto-address-url-regexp
  "Regexp used to determine what constitutes a text url.
Defaults to `goto-address-url-regxp'. Note that this is used for text urls in
modes that don't have some mechanism for supporting urls. This won't affect
link-hint's behavior in `org-mode' or modes that use shr.el for urls, for
example."
  :group 'link-hint
  :type 'regexp)

(defcustom link-hint-maybe-file-regexp
  (rx (or bol blank) (zero-or-one "~") "/" (1+ not-newline))
  "Regexp used to determine what constitutes a potential file link."
  :group 'link-hint
  :type 'regexp)

(defcustom link-hint-delete-trailing-paren t
  "Whether to delete a ) at the end of a url.
This is a workaround for emacs libraries including unwanted parens in urls.
See issue #15 for more information."
  :group 'link-hint
  :type 'boolean)

;; ** Avy Settings
;;  these only have an effect if bound by the user
(defvar link-hint-avy-style)

(defvar link-hint-avy-keys)

(defvar link-hint-avy-all-windows)

(defvar link-hint-avy-all-windows-alt)

(defvar link-hint-avy-background)

(defvar link-hint-avy-ignored-modes)

;; * Link Finding Helper Functions
(defun link-hint--find-regexp (search-regexp &optional start-bound end-bound)
  "Find the first occurrence of SEARCH-REGEXP.
Only search the range between just after START-BOUND and END-BOUND."
  (save-excursion
    (let ((start-bound (or start-bound (window-start)))
          (end-bound (or end-bound (window-end))))
      (goto-char start-bound)
      (condition-case err
          (forward-char)
        ('error nil))
      (when (re-search-forward search-regexp end-bound t)
        (match-beginning 0)))))

(defun link-hint--next-regexp (search-regexp &optional bound)
  "Find the next occurrence of SEARCH-REGEXP.
Only search the range between just after the point and BOUND."
  (link-hint--find-regexp search-regexp (point) bound))

;; only using for woman since need `next-single-char-property-change' (slow)
(defun link-hint--find-button (&optional start-bound end-bound)
  "Find the first button location.
Only search the range between just after START-BOUND and END-BOUND."
  (let ((start-bound (or start-bound (window-start)))
        (end-bound (or end-bound (window-end)))
        button)
    (save-restriction
      (narrow-to-region start-bound end-bound)
      (setq button (next-button (point)))
      (when button
        (button-start button)))))

(defun link-hint--next-button (&optional bound)
  "Find the next button location.
Only search the range between just after the point and BOUND."
  (link-hint--find-button (point) bound))

(defun link-hint--next-widget (&optional bound)
  "Find the next widget location. Currently only used for custom mode.
Only search the range between just after the point and BOUND."
  (setq bound (or bound (window-end)))
  (save-excursion
    (save-restriction
      (narrow-to-region (point) bound)
      (condition-case err
          (progn (widget-forward 1)
                 (point))
        ('error nil)))))

(defun link-hint--find-property-with-value
    (property value &optional start-bound end-bound)
  "Find the first location where PROPERTY has VALUE.
If VALUE is nil, find the first location where PROPERTY exists. Only search the
range from between just after the START-BOUND and END-BOUND."
  (let ((start-bound (or start-bound (window-start)))
        (end-bound (or end-bound (window-end)))
        first-non-match-pos)
    (setq first-non-match-pos
          (funcall (if value
                       #'text-property-not-all
                     #'text-property-any)
                   start-bound end-bound property value))
    (when first-non-match-pos
      (funcall (if value
                   #'text-property-any
                 #'text-property-not-all)
               first-non-match-pos end-bound property value))))

(defun link-hint--property-text
    (property &optional before-bound after-bound)
  "Return all text around the point for which PROPERTY stays the same.
Search only before the point to BEFORE-BOUND and after the point to AFTER-BOUND.
If the property does not change in this range, return the text between
BEFORE-BOUND and AFTER-BOUND."
  ;; TODO are these default bounds reasonable?
  ;; links can be split with a newline (e.g. Info-mode)
  (let ((before-bound (or before-bound
                          (save-excursion
                            (goto-char (window-start))
                            (forward-line -5)
                            (point))))
        (after-bound (or after-bound
                         (save-excursion
                           (goto-char (window-end))
                           (forward-line 5)
                           (point)))))
    (buffer-substring-no-properties
     (previous-single-property-change (1+ (point)) property nil before-bound)
     (next-single-property-change (point) property nil after-bound))))

(defun link-hint--next-property-with-value (property value &optional end-bound)
  "Find the next location where PROPERTY has VALUE.
Only search the range from between just after the point and END-BOUND."
  (link-hint--find-property-with-value property value (point) end-bound))

(defun link-hint--find-property (property &optional start-bound end-bound)
  "Find the first location where PROPERTY exists.
Only search the range from between just after the START-BOUND and the
END-BOUND."
  (link-hint--find-property-with-value property nil start-bound end-bound))

(defun link-hint--next-property (property &optional bound)
  "Find the next location where PROPERTY exists.
Only search the range from between just after the point and BOUND."
  (link-hint--find-property property (point) bound))

;; * Link Type Definition
(defun link-hint- (symbol)
  "Preface SYMBOL with \"link-hint-\"."
  (intern (format "link-hint-%S" symbol)))

;;;###autoload
(defun link-hint-define-type (name &rest properties)
  "Add a new type of link to link-hint.el."
  (declare (indent defun))
  (let ((type (link-hint- name)))
    (cl-loop for (prop value)
             on properties by 'cddr
             do (put type prop value))))

;; ** Helpers
(defun link-hint--min (&rest numbers)
  "Find the minimum from the list NUMBERS, ignoring nil values."
  (setq numbers (remq nil numbers))
  (when numbers
    (apply #'min numbers)))

(defun link-hint--var-valid-p (var)
  "Return t if VAR is bound and true or is the current major mode."
  (or (eq var major-mode)
      (and (boundp var) (symbol-value var))))

(defun link-hint--type-valid-p (type)
  "Return whether TYPE is a valid type for the current buffer.
This is done by checking that all its predicates hold, that at least one of its
variables is bound and true or the current major-mode, and that none of its
\"not\" variables are true (if bound) or the current major-mode."
  (let ((predicates (get type :predicates))
        (vars (get type :vars))
        (not-vars (get type :not-vars)))
    ;; note: cl-every will always return t for a null seq but cl-some will not
    ;; however, this behavior is not documented (hence or)
    (and (or (null predicates)
             (cl-every (lambda (pred)
                         (funcall pred))
                       predicates))
         (or (null vars)
             (cl-some #'link-hint--var-valid-p vars))
         (or (null not-vars)
             (not (cl-some #'link-hint--var-valid-p not-vars))))))

;; ** Text Url
(defun link-hint--next-text-url (&optional bound)
  "Find the next text url.
Only search the range between just after the point and BOUND."
  (link-hint--next-regexp link-hint-url-regexp bound))

(defun link-hint--text-url-at-point-p ()
  "Return the text url at the point or nil."
  (let ((url (url-get-url-at-point)))
    (and url
         (string-match link-hint-url-regexp url)
         (match-string 0 url))))

(defun link-hint--process-url (url _action)
  "Return URL without any trailing parentheses."
  (if link-hint-delete-trailing-paren
      (replace-regexp-in-string ")*$" "" url)
    url))

(link-hint-define-type 'text-url
  :next #'link-hint--next-text-url
  :at-point-p #'link-hint--text-url-at-point-p
  :not-vars '(org-mode
              ;; modes that use shr.el
              mu4e-view-mode
              elfeed-show-mode
              eww-mode)
  :parse #'link-hint--process-url
  :open #'browse-url
  :open-multiple t
  :copy #'kill-new)

;; ** File Link
(defun link-hint--find-file-link (&optional start-bound end-bound)
  "Find the first file link.
Only search the range between just after START-BOUND and END-BOUND."
  (save-excursion
    (let ((start-bound (or start-bound (window-start)))
          (end-bound (or end-bound (window-end)))
          file-link-pos)
      (goto-char start-bound)
      (while (and
              (setq file-link-pos
                    (link-hint--find-regexp link-hint-maybe-file-regexp
                                            (point) end-bound))
              (progn
                (goto-char file-link-pos)
                (when (looking-at (rx blank))
                  (forward-char)
                  (setq file-link-pos (point)))
                t)
              (not (ffap-file-at-point))))
      (when (and file-link-pos
                 (ffap-file-at-point))
        file-link-pos))))

(defun link-hint--next-file-link (&optional bound)
  "Find the next file link.
Only search the range between just after the point and BOUND."
  (link-hint--find-file-link (point) bound))

(link-hint-define-type 'file-link
  :next #'link-hint--next-file-link
  :at-point-p #'ffap-file-at-point
  ;; TODO consider making file links opt-in (use :vars)
  :not-vars '(org-mode Info-mode)
  :open #'find-file-at-point
  :copy #'kill-new)

;; ** Generic Button
(defun link-hint--next-button (&optional bound)
  "Find the next button.
Only search the range between just after the point and BOUND."
  (link-hint--next-property 'button bound))

(defun link-hint--button-at-point-p ()
  "Return the button at the point or nil."
  (let ((button (button-at (point))))
    (when button
      (button-label button))))

(link-hint-define-type 'button
  :next #'link-hint--next-button
  :at-point-p #'link-hint--button-at-point-p
  ;; TODO add more
  :not-vars '(woman-mode)
  :open #'push-button
  :copy #'kill-new)

;; ** Shr Url
(defun link-hint--next-shr-url (&optional bound)
  "Find the next shr url.
Only search the range between just after the point and BOUND."
  ;; `shr-next-link' just uses text properties as well
  (link-hint--next-property 'shr-url bound))

(defun link-hint--shr-url-at-point-p ()
  "Return the shr url at the point or nil."
  (get-text-property (point) 'shr-url))

(link-hint-define-type 'shr-url
  :next #'link-hint--next-shr-url
  :at-point-p #'link-hint--shr-url-at-point-p
  ;; would need a comprehensive list of all modes that use shr.el
  ;; :vars
  :open #'browse-url
  :open-multiple t
  :copy #'kill-new)

;; ** Org link
(defun link-hint--next-org-link (&optional bound)
  "Find the next org link.
Only search the range between just after the point and BOUND."
  (link-hint--next-property 'htmlize-link bound))

(defun link-hint--org-link-at-point-p ()
  "Return the org link at the point or nil."
  ;; note: org uses :uri even for elisp forms, shell commands, etc.
  ;; note: org doesn't provide a property for the text in [[url][text]]
  (plist-get (get-text-property (point) 'htmlize-link) :uri))

(defun link-hint--open-org-link (uri)
  "Open the org link URI."
  ;; org-open-at-point won't work e.g. for =http://address.com= even
  ;; though `org-next-link' will jump to it
  (condition-case err
      (org-open-at-point)
    ('error (org-open-link-from-string uri))))

(link-hint-define-type 'org-link
  :next #'link-hint--next-org-link
  :at-point-p #'link-hint--org-link-at-point-p
  :vars '(org-mode)
  :open #'link-hint--open-org-link
  :open-multiple t
  :copy #'kill-new)

;; ** Mu4e Url
(defun link-hint--next-mu4e-url (&optional bound)
  "Find the next mu4e url.
Only search the range between just after the point and BOUND."
  (link-hint--next-property 'mu4e-url bound))

(defun link-hint--mu4e-url-at-point-p ()
  "Return the mu4e url at the point or nil."
  (get-text-property (point) 'mu4e-url))

(defun link-hint--open-mu4e-url (url)
  "Open the mu4e URL."
  ;; note: browse-url also supports mailto
  (mu4e~view-browse-url-from-binding url))

(link-hint-define-type 'mu4e-url
  :next #'link-hint--next-mu4e-url
  :at-point-p #'link-hint--mu4e-url-at-point-p
  :vars '(mu4e-view-mode)
  :open #'link-hint--open-mu4e-url
  :open-multiple t
  :copy #'kill-new)

;; ** Mu4e Attachment
(defun link-hint--next-mu4e-attachment (&optional bound)
  "Find the next mu4e attachment.
Only search the range between just after the point and BOUND."
  (link-hint--next-property 'mu4e-attnum bound))

(defun link-hint--mu4e-attachment-at-point-p ()
  "Return the mu4e attachment number at the point or nil."
  (get-text-property (point) 'mu4e-attnum))

(defun link-hint--open-mu4e-attachment (attnum)
  "Open the mu4e attachment having number ATTNUM."
  (mu4e-view-open-attachment nil attnum))

(defun link-hint--copy-mu4e-attachment (attnum)
  "Save the mu4e attachment having number ATTNUM."
  (mu4e-view-save-attachment-single nil mu4e-att))

(link-hint-define-type 'mu4e-attachment
  :next #'link-hint--next-mu4e-attachment
  :at-point-p #'link-hint--mu4e-attachment-at-point-p
  :vars '(mu4e-view-mode)
  :open #'link-hint--open-mu4e-attachment
  :copy #'link-hint--copy-mu4e-attachment)

;; ** Help Link
(defun link-hint--next-help-link (&optional bound)
  "Find the next help link.
Only search the range between just after the point and BOUND."
  (link-hint--next-property 'help-args bound))

(defun link-hint--help-link-at-point-p ()
  "Return the name of the help link at the point or nil."
  (let ((help-link (get-text-property (point) 'help-args)))
    (when help-link
      (format "%s" help-link))))

(link-hint-define-type 'help-link
  :next #'link-hint--next-help-link
  :at-point-p #'link-hint--help-link-at-point-p
  :vars '(help-mode)
  :open #'link-hint--open-button
  :copy #'kill-new)

;; ** Info Link
(defun link-hint--next-info-link (&optional bound)
  "Find the next info link.
Only search the range between just after the point and BOUND."
  ;; Info-next-reference doesn't work for all links and uses
  ;; `next-single-char-property-change'
  (link-hint--min (link-hint--next-property-with-value
                   'font-lock-face 'info-xref bound)
                  ;; visited links have a different face
                  (link-hint--next-property-with-value
                   'font-lock-face 'info-xref-visited bound)))

(defun link-hint--info-link-at-point-p ()
  "Return the name of the info link at the point or nil."
  ;; (Info-extract-pointer "next")
  (when (or (eq (get-text-property (point) 'font-lock-face)
                'info-xref)
            (eq (get-text-property (point) 'font-lock-face)
                'info-xref-visited))
    ;; note: Info-mode doesn't use buttons
    (link-hint--property-text 'font-lock-face)))

(defun link-hint--open-info-link (_)
  "Open the info link at the point."
  (Info-follow-nearest-node))

(link-hint-define-type 'info-link
  :next #'link-hint--next-info-link
  :at-point-p #'link-hint--info-link-at-point-p
  :vars '(Info-mode)
  :open #'link-hint--open-info-link
  :copy #'kill-new)

;; ** Package Link
(defun link-hint--next-package-link (&optional bound)
  "Find the next package link.
Only search the range between just after the point and BOUND."
  ;; (link-hint--next-property-with-value
  ;;  'action 'package-menu-describe-package bound)
  (link-hint--next-property 'package-desc bound))

(defun link-hint--package-link-at-point-p ()
  "Return the name of the package at the point or nil."
  (get-text-property (point) 'package-desc))

(defun link-hint--parse-package-link (package-desc action)
  "Alter PACKAGE-DESC so that it can be passed to the ACTION function."
  (cl-case action
    (:browse-url (cdr (assq :url (package-desc-extras package-desc))))
    ((:describe :copy) (package-desc-name package-desc))
    (t package-desc)))

(link-hint-define-type 'package-link
  :next #'link-hint--next-package-link
  :at-point-p #'link-hint--package-link-at-point-p
  :vars '(package-menu-mode paradox-menu-mode)
  :parse #'link-hint--parse-package-link
  ;; doesn't work properly for all packages
  ;; :open #'package-menu-describe-package
  :open #'push-button
  :browse-url #'browse-url
  :browse-multiple t
  :copy #'kill-new)

;; ** Package Home Page Link (paradox only)
;; TODO

;; ** Package Keyword Link
(defun link-hint--next-package-keyword-link (&optional bound)
  "Find the next package keyword link.
Only search the range between just after the point and BOUND."
  ;; (link-hint--next-property-with-value
  ;;  'action 'package-keyword-button-action bound)
  (link-hint--next-property 'package-keyword))

(defun link-hint--package-keyword-link-at-point-p ()
  "Return the name of the package keywords at the point or nil."
  (get-text-property (point) 'package-keyword))

(link-hint-define-type 'package-keyword-link
  :next #'link-hint--next-package-keyword-link
  :at-point-p #'link-hint--package-keyword-link-at-point-p
  :vars '(help-mode)
  :open #'push-button
  :copy #'kill-new)

;; ** Package Install Link
(defun link-hint--next-package-install-link (&optional bound)
  "Find the next package installation link.
Only search the range between just after the point and BOUND."
  (link-hint--next-property-with-value
   'action 'package-install-button-action bound))

(defun link-hint--package-link-at-point-p ()
  "If there is a package link at the point, return its name."
  (when (eq (get-text-property (point) 'action)
            'package-install-button-action)
    (package-desc-name (get-text-property (point) 'package-desc))))

(link-hint-define-type 'package-install-link
  :next #'link-hint--next-package-install-link
  :at-point-p #'link-hint--package-link-at-point-p
  :vars '(help-mode)
  :open #'push-button
  :open-message "Installed")

;; ** Compilation Link
(defun link-hint--next-compilation-link (&optional bound)
  "Find the next compilation link.
Only search the range between just after the point and BOUND."
  (link-hint--next-property 'compilation-message bound))

(defun link-hint--compilation-link-at-point-p ()
  "Return the compilation link message at the point or nil."
  (get-text-property (point) 'compilation-message))

(link-hint-define-type 'compilation-link
  :next #'link-hint--next-compilation-link
  :at-point-p #'link-hint--compilation-link-at-point-p
  :vars '(compilation-mode)
  ;; no simple way to get message for copying
  :open #'compile-goto-error)

;; ** W3M Link
(defun link-hint--next-w3m-link (&optional bound)
  "Find the next w3m link.
Only search the range between just after the point and BOUND."
  ;; `w3m-goto-next-link' also uses text properties
  (link-hint--next-property 'w3m-href-anchor bound))

(defun link-hint--w3m-link-at-point-p ()
  "Return the w3m link at the point or nil."
  (get-text-property (point) 'w3m-href-anchor))

(link-hint-define-type 'w3m-link
  :next #'link-hint--next-w3m-link
  :at-point-p #'link-hint--w3m-link-at-point-p
  :vars '(w3m-mode)
  :open #'w3m-view-this-url
  :copy #'kill-new)

;; ** Woman Button
;; separate type due to performance issues
(defun link-hint--next-woman-button (&optional bound)
  "Find the next woman link.
Only search the range between just after the point and BOUND."
  ;; this is slow (`next-single-char-property-change') but necessary for woman
  (link-hint--next-button bound))

(link-hint-define-type 'woman-button
  :next #'link-hint--next-woman-button
  :at-point-p #'link-hint--button-at-point-p
  :vars '(woman-mode)
  :open #'push-button
  :copy #'kill-new)

;; ** Customize Widget
(defun link-hint--next-customize-widget (&optional bound)
  "Find the next package customize widget.
Only search the range between just after the point and BOUND."
  (link-hint--next-widget bound))

;; (customize-link (and (eq major-mode 'Custom-mode)
;;                      (button-at (point))))
;; (customize-field (and (eq major-mode 'Custom-mode)
;;                       (eq (car (widget-tabable-at))
;;                           'editable-field)))

;; (defun link-hint--open-customize-widget ()
;;   "Open the customize widget at the point."
;;   (Custom-newline (point)))

(link-hint-define-type 'customize-widget
  :next #'link-hint--next-customize-widget
  :at-point-p #'link-hint--button-at-point-p
  :vars '(Custom-mode)
  :open #'push-button
  :copy #'kill-new)

;; * Avy/Action Helper Functions
(defun link-hint--collect (start end type)
  "Between START and END in the current buffer, collect all links of TYPE.
If the link TYPE does not satisfy the necessary predicates, return nil."
  (when (link-hint--type-valid-p type)
    (save-excursion
      (goto-char start)
      (let ((current-window (get-buffer-window))
            (next-func (get type :next))
            (at-point-p (get type :at-point-p))
            links
            link-pos)
        ;; as all "next-" functions are designed to look after the point,
        ;; check if there is a link at the point the first time, in order
        ;; to catch links that are at the start bound,
        ;; TODO explain this better and retest
        ;; as the eol of an invisible line can be visible in org buffers,
        ;; don't do this if the point is at the eol
        ;; TODO make this a do while instead if find a way around this
        (when (and (not (looking-at (rx eol)))
                   (funcall at-point-p))
          (push (list :pos (point)
                      :win current-window
                      :args (funcall at-point-p)
                      :type type)
                links))
        (while (setq link-pos (funcall next-func end))
          (goto-char link-pos)
          (push (list :pos link-pos
                      :win current-window
                      :args (funcall at-point-p)
                      :type type)
                links))
        links))))

;; WORKAROUND for avy--find-visible-regions sometimes excluding visible ranges
;; (which may be org's fault)
;; (defun avy--next-invisible-point ()
;;   "Return the next closest point with 'invisible property."
;;   (let ((s (point)))
;;     (while (and (not (= (point-max) (setq s (next-overlay-change s))))
;;                 (let ((invisible-property (get-char-property s 'invisible)))
;;                   (or (not invisible-property)
;;                       (equal invisible-property 'org-link)))))
;;     s))

;; other way
;; modified version of avy--find-visible-regions

;; TODO retest avy--find-visible-regions and make a bug report if necessary
;; TODO consistency (start end vs beg end)
(defun link-hint--find-visible-regions (rbeg rend)
  "Return a list of all visible regions between RBEG and REND."
  (setq rbeg (max rbeg (point-min)))
  (setq rend (min rend (point-max)))
  (when (< rbeg rend)
    (let (visibles beg)
      (save-excursion
        (save-restriction
          (narrow-to-region rbeg rend)
          (setq beg (goto-char (point-min)))
          (while (not (= (point) (point-max)))
            (goto-char (or (link-hint--next-property 'invisible)
                           (point-max)))
            (push (cons beg (point)) visibles)
            (setq beg (goto-char
                       (or (next-single-property-change
                            (point)
                            'invisible)
                           (point-max)))))
          (nreverse visibles))))))

(defun link-hint--equal (x y)
  "Return whether links X and Y are equal based on their window and position."
  (and (= (plist-get x :pos)
          (plist-get y :pos))
       (eq (plist-get x :win)
           (plist-get y :win))))

(defun link-hint--<  (x y)
  "Return whether link X's position is before link Y's."
  (< (plist-get x :pos) (plist-get y :pos)))

(defun link-hint--collect-visible-links ()
  "Collect all visible links in the current buffer."
  (let (all-link-positions)
    (dolist (bounds (link-hint--find-visible-regions (window-start)
                                                     (window-end)))
      (dolist (type link-hint-types)
        (setq all-link-positions
              (append all-link-positions
                      (link-hint--collect (car bounds) (cdr bounds) type)))))
    (sort (cl-delete-duplicates all-link-positions
                                :test #'link-hint--equal
                                ;; types earlier in `link-hint-types' have
                                ;; higher priority
                                :from-end t)
          #'link-hint--<)))

(cl-defun link-hint--get-links ()
  "Return a list of all visible links (potentially in multiple windows)."
  (let ((avy-all-windows (if (boundp 'link-hint-avy-all-windows)
                             link-hint-avy-all-windows
                           avy-all-windows))
        (avy-all-windows-alt (if (boundp 'link-hint-avy-all-windows-alt)
                                 link-hint-avy-all-windows-alt
                               avy-all-windows-alt))
        (avy-ignored-modes (if (boundp 'link-hint-avy-ignored-modes)
                               link-hint-avy-ignored-modes
                             avy-ignored-modes))
        links)
    (avy-dowindows current-prefix-arg
      (setq links (append links (link-hint--collect-visible-links))))
    (if links
        links
      (when link-hint-message
        (funcall link-hint-message "No links found."))
      nil)))

(defun link-hint--process (links)
  "Select a link from LINKS using avy.
If there is only one link in LINKS, return it."
  (let ((avy-background (if (boundp 'link-hint-avy-background)
                            link-hint-avy-background
                          avy-background))
        (avy-keys (if (boundp 'link-hint-avy-keys)
                      link-hint-avy-keys
                    avy-keys))
        ;; prevent window from shifting avy overlays out of view
        (scroll-margin 0))
    (if (cdr links)
        (save-selected-window
          (let* ((avy-action #'identity)
                 (pos (avy--process
                       (mapcar (lambda (x) (cons (plist-get x :pos)
                                                 (plist-get x :win)))
                               links)
                       (avy--style-fn (if (boundp 'link-hint-avy-style)
                                          link-hint-avy-style
                                        avy-style)))))
            (if (numberp pos)
                (cl-find (list :pos pos :win (get-buffer-window))
                         links
                         :test #'link-hint--equal)
              (when link-hint-message
                (funcall link-hint-message "Aborted link selection."))
              nil)))
      (car links))))

(defun link-hint--apply (func args &optional parser action)
  "Try to call FUNC with ARGS.
If PARSER is specified, first change ARGS by passing PARSER ARGS and ACTION.
First try `apply'. If there is an error (ARGS is the wrong number of arguments
for FUNC), `funcall' FUNC with ARGS. Finally, call FUNC alone."
  (when parser
    (setq args (funcall parser args action)))
  ;; TODO is there a way to know how many arguments a function takes?
  (condition-case err
      (apply func args)
    ('error (condition-case err
                (funcall func args)
              ('error (funcall func))))))

(defun link-hint--message (action &optional link-description type)
  "Display a message about an ACTION performed on a link.
`link-hint-message' holds the function to use to message (or is nil if there
should be no messaging). LINK-DESCRIPTION can either be a number corresponding
to the number of links acted upon or a description of a single link. Custom
messages for ACTION can be created by altering `link-hint-action-messages' or
adding an :<action>-message property to the link TYPE."
  (when link-hint-message
    (funcall link-hint-message "%s %s"
             (or (when type
                   (get type (intern (format "%s-message" action))))
                 (plist-get link-hint-action-messages action)
                 (format "Called %s on" action))
             (concat (or (when (numberp link-description)
                           (format "%s links" link-description))
                         link-description
                         "a link")
                     "."))))

(defun link-hint--action (action link)
  "Take ACTION on LINK.
If the point/window are not intentionally changed by the action, restore them."
  (let* ((original-win (get-buffer-window))
         link-buffer
         link-buffer-original-pos
         (link-pos (plist-get link :pos))
         (link-win (plist-get link :win))
         (type (plist-get link :type))
         (parser (get type :parse))
         (args (plist-get link :args))
         (link-description (link-hint--apply
                            (or (get type :describe) #'identity)
                            args
                            parser
                            action))
         ret)
    (select-window link-win)
    (setq link-buffer (current-buffer)
          link-buffer-original-pos (point))
    (goto-char link-pos)
    (setq ret (link-hint--apply (get type action) args parser action))
    ;; TODO doesn't work
    (cond ((and (eq (current-buffer) link-buffer)
                (= (point) link-pos))
           ;; when buffer and position don't change, restore position and window
           ;; (no side effects have occurred)
           (goto-char link-buffer-original-pos)
           (select-window original-win))
          ((not (eq (current-buffer) link-buffer))
           ;; when buffer changes, restore position in old buffer
           (with-current-buffer link-buffer
             (goto-char link-buffer-original-pos)))
          ;; when buffer doesn't change but position does (e.g. local org
          ;; link), do nothing
          )
    (link-hint--message action link-description type)
    ret))

(defun link-hint--links-action (action links)
  "Take ACTION on every link in LINKS."
  (dolist (link links)
    (let (link-hint-message)
      (link-hint--action action link)))
  (link-hint--message action (length links)))

(defun link-hint--valid-types (&rest properties)
  "Return a list of valid link hint types based on PROPERTIES."
  (cl-loop for type in link-hint-types
           when (cl-every (lambda (prop) (get type prop))
                          properties)
           collect type))

(defun link-hint--one (action)
  "Take ACTION on one visible link selected with avy."
  (let* ((link-hint-types (link-hint--valid-types action))
         (links (link-hint--get-links))
         link)
    (when links
      (setq link (link-hint--process links))
      (when link
        (link-hint--action action link)))))

(defun link-hint--multiple (action)
  "Take ACTION on multiple visible links selected with avy."
  (let* ((multiple-action (intern (format "%s-multiple" action)))
         (link-hint-types (link-hint--valid-types action multiple-action))
         (links (link-hint--get-links))
         link
         chosen-links)
    (while (setq link (link-hint--process links))
      (push link chosen-links))
    (link-hint--links-action action (nreverse chosen-links))))

(defun link-hint--all (action)
  "Take ACTION on all visible links."
  (let* ((multiple-action (intern (format "%s-multiple" action)))
         (link-hint-types (link-hint--valid-types action multiple-action))
         (links (link-hint--get-links)))
    (link-hint--links-action action links)))

;; * User Commands
;; ** Avy Commands
;;;###autoload
(defun link-hint-open-link ()
  "Use avy to open a visible link."
  (interactive)
  (avy-with link-hint-open-link
    (link-hint--one :open)))

;;;###autoload
(defun link-hint-copy-link ()
  "Copy a visible link of a supported type to the kill ring with avy.
`select-enable-clipboard' and `select-enable-primary' can be set to non-nil
values to copy the link to the clipboard and/or primary as well."  (interactive)
  (avy-with link-hint-copy-link
    (link-hint--one :copy)))

;;;###autoload
(defun link-hint-open-multiple-links ()
  "Use avy to open multiple visible links at once."
  (interactive)
  (avy-with link-hint-open-multiple-links
    (link-hint--multiple :open)))

;;;###autoload
(defun link-hint-copy-multiple-links ()
  "Use avy to copy multiple visible links at once to the kill ring."
  (interactive)
  (avy-with link-hint-copy-multiple-links
    (link-hint--multiple :copy)))

;;;###autoload
(defun link-hint-open-all-links ()
  "Open all visible links."
  (interactive)
  (avy-with link-hint-open-all-links
    (link-hint--all :open)))

;;;###autoload
(defun link-hint-copy-all-links ()
  "Copy all visible links."
  (interactive)
  (avy-with link-hint-copy-all-links
    (link-hint--all :copy)))

;; ** At Point Commands
(defun link-hint--get-link-at-point ()
  "Return the link with the highest priority at the point or nil."
  (let (args)
    (cl-dolist (type link-hint-types)
      (when (and (link-hint--type-valid-p type)
                 (setq args (funcall (get type :at-point-p))))
        (cl-return (list :pos (point)
                         :win (get-buffer-window)
                         :args args
                         :type type))))))

(defun link-hint--action-at-point (action)
  "Take ACTION on the highest priority link at the point."
  (let ((link-hint-types (link-hint--valid-types action))
        (link (link-hint--get-link-at-point)))
    (if link
        (link-hint--action action link)
      (when link-hint-message
        (funcall link-hint-message
                 "There is no link supporting the %s action at the point."
                 action)))))

;;;###autoload
(defun link-hint-open-link-at-point ()
  "Open the link with the highest priority at the point."
  (interactive)
  (link-hint--action-at-point :open))

(defun link-hint-copy-link-at-point ()
  "Copy the link with the highest priority at the point."
  (interactive)
  (link-hint--action-at-point :copy))

(provide 'link-hint)
;;; link-hint.el ends here
