;;; link-hint.el --- Use avy to open or copy visible urls.

;; Author: Fox Kiester <noct@openmailbox.org>
;; URL: https://github.com/noctuid/link-hint.el
;; Keywords: url
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
;; This packages gives commands for operating on visible links with avy. It is
;; inspired by link-hinting from vim-like browsers and browser plugins such as
;; pentadactyl. For example, `link-hint-open-link' will use avy to select and
;; open a link in the current buffer. A link can be a text, shr, mu4e or org
;; (htmlize) url. Mu4e attachments and mailto addresses, help mode links, and
;; info mode links are also considered to be links. The user can set
;; `link-hint-ignore-types' to can change what is considered a link. Commands
;; are also provided for copying links to the kill ring (and optionally the
;; clipboard and/or primary) and for opening multiple urls at once like with
;; pentadactyl's "g;".

;; For more information see the README in the github repo.

;;; Code:
(require 'avy)
(require 'url-util)
(require 'browse-url)
(unless (boundp 'link-hint-url-regexp)
  (require 'goto-addr))
(require 'ffap)
;; (require 'rx)

;;; Options
(defgroup link-hint nil
  "Gives commands for operating on visible links with avy."
  :group 'convenience
  :prefix 'link-hint)

(defcustom link-hint-url-regexp
  goto-address-url-regexp
  "Regexp used to determine what constitutes a text url.
Defaults to `goto-address-url-regxp'."
  :group 'link-hint
  :type 'regexp)

(defcustom link-hint-maybe-file-regexp
  (rx (or bol blank) (zero-or-one "~") "/" (1+ not-newline))
  "Regexp used to determine what constitutes a potential file link."
  :group 'link-hint
  :type 'regexp)

(defcustom link-hint-avy-style
  avy-style
  "Method for displaying avy overlays.
Defaults to `avy-style'."
  :group 'link-hint
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At Full" at-full)
          (const :tag "Post" post)
          (const :tag "De Bruijn" de-bruijn)))

(defcustom link-hint-avy-keys
  avy-keys
  "Keys used for selecting urls.
Defaults to `avy-keys'."
  :group 'link-hint
  :type '(repeat :tag "Keys" (choice (character :tag "char"))))

(defcustom link-hint-avy-all-windows
  avy-all-windows
  "Determine the list of windows to consider in search of links.
Defaults to `avy-all-windows'."
  :type
  '(choice
    (const :tag "All Frames" all-frames)
    (const :tag "This Frame" t)
    (const :tag "This Window" nil)))

(defcustom link-hint-avy-all-windows-alt
  ;; until can depend on new release of avy
  (if (boundp 'avy-all-windows-alt)
      avy-all-windows-alt
    nil)
  "The alternative `link-hint-avy-all-windows' for use with
\\[universal-argument]. Defaults to `avy-all-windows-alt'."
  :type '(choice
          (const :tag "All windows on the current frame" t)
          (const :tag "All windows on all frames" all-frames)))

(defcustom link-hint-avy-background avy-background
  "When non-nil, a gray background will be added during the selection.
Defaults to `avy-background'."
  :type 'boolean)

(defcustom link-hint-ignored-modes avy-ignored-modes
  "List of modes to ignore when searching for links.
Defaults to `avy-ignored-modes'.")

(defconst link-hint-all-types
  '(text-url
    file-link
    shr-url
    org-link
    mu4e-url
    mu4e-mailto
    mu4e-attachment
    help-link
    info-link
    package-description-link
    package-keyword-link
    package-install-link
    compilation-link
    w3m-link
    customize-link
    woman-button-link
    other-button-link)
  "List containing all suported link types.")

(define-widget 'link-hint-link-type-set 'lazy
  "A set of link types supported by link-hint."
  :type '(set
          (const :tag "Text url" text-url)
          (const :tag "File link openable with ffap" file-link)
          (const :tag "Simple HTML Renderer url" shr-url)
          (const :tag "Org mode links" org-link)
          (const :tag "Mu4e url" mu4e-url)
          (const :tag "Mu4e mailto address" mu4e-mailto)
          (const :tag "Mu4e attachment" mu4e-attachment)
          (const :tag "Help mode link" help-link)
          (const :tag "Info mode link" info-link)
          (const :tag "Package.el menu links" package-description-link)
          (const :tag "Package.el keyword buttons" package-keyword-link)
          (const :tag "Package.el install buttons" package-install-link)
          (const :tag "Compilation mode link" compilation-link)
          (const :tag "W3M link" w3m-link)
          (const :tag "Customize link" customize-link)
          (const :tag "WoMan button link" woman-button-link)
          (const :tag "Other button link" other-button-link)))

(defcustom link-hint-ignore-types
  nil
  "Types to be ignored when selecting a url with avy."
  :group 'link-hint
  :type 'link-hint-link-type-set)

(defvar link-hint-copy-ignore-types
  '(help-link
    info-link
    package-description-link
    package-keyword-link
    package-install-link
    compilation-link
    customize-link
    woman-button-link
    other-button-link)
  "Link types that the copy action will ignore.
It defaults to the unsupported types.")

(defcustom link-hint-act-on-multiple-ignore-types
  '(file-link
    mu4e-mailto
    mu4e-attachment
    help-link
    info-link
    compilation-link
    w3m-link
    customize-link
    woman-button-link
    other-button-link)
  "Types of links to ignore with commands that act on multiple visible links.
Thes commands are `link-hint-open-multiple-links' and
`link-hint-copy-multiple-links'."
  :group 'link-hint
  :type 'link-hint-link-type-set)

(defcustom link-hint-act-on-all-ignore-types
  '(file-link
    mu4e-mailto
    mu4e-attachment
    help-link
    info-link
    compilation-link
    customize-link
    w3m-link
    woman-button-link
    other-button-link)
  "Types of links to ignore with commands that act on all visible links.
These commands are `link-hint-open-all-links' and
`link-hint-copy-all-links'."
  :group 'link-hint
  :type 'link-hint-link-type-set)

(defcustom link-hint-message t
  "Whether to message information for commands."
  :group 'link-hint
  :type 'boolean)

;;; Link Finding Helper Functions
(defun link-hint--find-regexp (search-regexp &optional start-bound end-bound)
  "Find the first occurrence of SEARCH-REGEXP.
Only the range between just after START-BOUND and the END-BOUND will be
searched."
  (save-excursion
    (let ((start-bound (or start-bound (window-start)))
          (end-bound (or end-bound (window-end))))
      (goto-char start-bound)
      (condition-case err
          (right-char)
        ('error nil))
      (when (re-search-forward search-regexp end-bound t)
        (match-beginning 0)))))

(defun link-hint--next-regexp (search-regexp &optional end-bound)
  "Find the next occurrence of SEARCH-REGEXP.
Only the range between just after the point and END-BOUND will be searched."
  (link-hint--find-regexp search-regexp (point) end-bound))

(defun link-hint--find-file-link (&optional start-bound end-bound)
  "Find the first file link.
Only the range between just after START-BOUND and the END-BOUND will be
searched."
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
                  (right-char)
                  (setq file-link-pos (point)))
                t)
              (not (ffap-file-at-point))))
      (when (and file-link-pos
                 (ffap-file-at-point))
        file-link-pos))))

(defun link-hint--next-file-link (&optional end-bound)
  "Find the next file link.
Only the range between just after the point and END-BOUND will be searched."
  (link-hint--find-file-link (point) end-bound))

;; only using for woman since need `next-single-char-property-change' (slow)
(defun link-hint--find-button (&optional start-bound end-bound)
  "Find the first button location.
Only the range between just after START-BOUND and the END-BOUND will be
searched."
  (let ((start-bound (or start-bound (window-start)))
        (end-bound (or end-bound (window-end)))
        button)
    (save-restriction
      (narrow-to-region start-bound end-bound)
      (setq button (next-button (point)))
      (when button
        (button-start button)))))

(defun link-hint--next-button (&optional end-bound)
  "Find the next button location.
Only the range between just after the point and END-BOUND will be searched."
  (link-hint--find-button (point) end-bound))

(defun link-hint--next-widget (&optional end-bound)
  "Find the next widget location. Currently only used for custom mode.
Only the range between just after the point and END-BOUND will be searched."
  (setq end-bound (or end-bound (window-end)))
  (save-excursion
    (save-restriction
      (narrow-to-region (point) end-bound)
      (condition-case err
          (progn (widget-forward 1)
                 (point))
        ('error nil)))))

(defun link-hint--find-property (property &optional start-bound end-bound)
  "Find location where PROPERTY exists.
Only the range from between just after the START-BOUND and the END-BOUND will be
searched."
  (save-excursion
    (let ((start-bound (or start-bound (window-start)))
          (end-bound (or end-bound (window-end)))
          first-pos-without-prop
          first-pos-with-prop)
      (goto-char start-bound)
      (setq first-pos-without-prop
            (text-property-any (point) end-bound property nil))
      (when first-pos-without-prop
        (goto-char first-pos-without-prop)
        (setq first-pos-with-prop
              (text-property-not-all (point) end-bound property nil)))
      (when first-pos-with-prop
        first-pos-with-prop))))

(defun link-hint--next-property (property &optional end-bound)
  "Find the next location where PROPERTY exists.
Only the range from between just after the point and END-BOUND will be
searched."
  (link-hint--find-property property (point) end-bound))

;; TODO get rid of code duplication here
(defun link-hint--find-property-with-value
    (property value &optional start-bound end-bound)
  "Find location where PROPERTY exists.
Only the range from between just after the START-BOUND and the END-BOUND will be
searched."
  (save-excursion
    (let ((start-bound (or start-bound (window-start)))
          (end-bound (or end-bound (window-end)))
          first-pos-without-prop
          first-pos-with-prop)
      (goto-char start-bound)
      (setq first-pos-without-prop
            (text-property-not-all (point) end-bound property value))
      (when first-pos-without-prop
        (goto-char first-pos-without-prop)
        (setq first-pos-with-prop
              (text-property-any (point) end-bound property value)))
      (when first-pos-with-prop
        first-pos-with-prop))))

(defun link-hint--next-property-with-value (property value &optional end-bound)
  "Find the first location where PROPERTY has VALUE.
Only the range from between just after the point and the END-BOUND will be
searched. When VALUE is not found, nil will be returned."
  (link-hint--find-property-with-value property value (point) end-bound))

(defun link-hint--not-ignored-p (type)
  "Return t if TYPE is not ignored else nil."
  (not (member type link-hint-ignore-types)))

(defun link-hint--min (numbers)
  "Find the minimum from the list NUMBERS, ignoring nil values."
  (let (number-list
        number)
    (while (> (length numbers) 0)
      (setq number (pop numbers))
      (when number
        (push number number-list)))
    (when (> (length number-list) 0)
      (apply #'min number-list))))

;; TODO: reduce redundancy here
(defun link-hint--next-link-pos (&optional end-bound)
  "Find the closest link of all types that are not ignored.
Only the range between just after the point and END-BOUND will be searched."
  (let* ((end-bound (or end-bound (window-end)))
         (text-url-pos (when (and (link-hint--not-ignored-p 'text-url)
                                  (not (equal major-mode 'org-mode)))
                         (link-hint--next-regexp link-hint-url-regexp
                                                 end-bound)))
         (file-link-pos (when (link-hint--not-ignored-p 'file-link)
                          (link-hint--next-file-link end-bound)))
         (shr-url-pos (when (link-hint--not-ignored-p 'shr-url)
                        (link-hint--next-property 'shr-url end-bound)))
         (org-link-pos (when (link-hint--not-ignored-p 'org-link)
                         (link-hint--next-property 'htmlize-link end-bound)))
         (mu4e-link-pos (link-hint--next-property 'mu4e-url end-bound))
         (mu4e-link-text (when mu4e-link-pos
                           (plist-get (text-properties-at mu4e-link-pos) 'mu4e-url)))
         (mu4e-url-pos
          (when (and mu4e-link-text
                     (link-hint--not-ignored-p 'mu4e-url)
                     (not (string-prefix-p "mailto" mu4e-link-text)))
            mu4e-link-pos))
         (mu4e-mailto-pos
          (when (and mu4e-link-text
                     (link-hint--not-ignored-p 'mu4e-mailto)
                     (string-prefix-p "mailto" mu4e-link-text))
            mu4e-link-pos))
         (mu4e-att-pos
          (when (not (member 'mu4e-attachment link-hint-ignore-types))
            (link-hint--next-property 'mu4e-attnum end-bound)))
         (help-link-pos (when (link-hint--not-ignored-p 'help-link)
                          (link-hint--next-property 'help-args end-bound)))
         (info-link-pos (when (link-hint--not-ignored-p 'info-link)
                          (link-hint--next-property-with-value
                           'font-lock-face 'info-xref end-bound)))
         (info-link-visited-pos
          (when (link-hint--not-ignored-p 'info-link)
            (link-hint--next-property-with-value
             'font-lock-face 'info-xref-visited end-bound)))
         (package-description-link-pos
          (when (link-hint--not-ignored-p 'package-description-link)
            (link-hint--next-property-with-value
             'action 'package-menu-describe-package)))
         (package-keyword-link-pos
          (when (link-hint--not-ignored-p 'package-keyword-link)
            (link-hint--next-property-with-value
             'action 'package-keyword-button-action)))
         (package-install-link-pos
          (when (link-hint--not-ignored-p 'package-install-link)
            (link-hint--next-property-with-value
             'action 'package-install-button-action)))
         (compilation-link-pos
          (when (link-hint--not-ignored-p 'compilation-link)
            (link-hint--next-property 'compilation-message end-bound)))
         (w3m-link-pos
          (when (link-hint--not-ignored-p 'w3m-link)
            ;; alternatively could use w3m-goto-next-link
            (link-hint--next-property 'w3m-href-anchor)))
         (woman-button-link-pos
          ;; next-single-char-property-change is slow but is necessary for
          ;; see also buttons in woman mode
          (when (and (equal major-mode 'woman-mode)
                     (link-hint--not-ignored-p 'woman-button-link))
            (link-hint--next-button end-bound)))
         (customize-link-pos
          (when (link-hint--not-ignored-p 'customize-link)
            (link-hint--next-widget end-bound)))
         (other-button-link-pos
          (when (link-hint--not-ignored-p 'other-button-link)
            (link-hint--next-property 'button end-bound)))
         (closest-pos
          (link-hint--min (list text-url-pos
                                file-link-pos
                                shr-url-pos
                                org-link-pos
                                mu4e-url-pos
                                mu4e-mailto-pos
                                mu4e-att-pos
                                help-link-pos
                                info-link-pos
                                info-link-visited-pos
                                package-description-link-pos
                                package-keyword-link-pos
                                package-install-link-pos
                                compilation-link-pos
                                w3m-link-pos
                                woman-button-link-pos
                                customize-link-pos
                                other-button-link-pos))))
    (when closest-pos
      closest-pos)))

(defun link-hint--collect (start-bound end-bound get-next-link)
  "Collect all links between START-BOUND and END-BOUND.
GET-NEXT-LINK will be repeatedly called with END-BOUND as an argument."
  (save-excursion
    (goto-char start-bound)
    (let ((current-window (get-buffer-window))
          (link-pos (funcall get-next-link end-bound))
          link-positions)
      ;; as all "next-" functions are designed to look after the point,
      ;; check if there is a link at the point the first time in order
      ;; to catch links that are at the start bound
      ;; as the eol of an invisible line can be visible in org buffers,
      ;; don't do this if the point is at the eol
      (when (and (not (looking-at (rx eol)))
                 (link-hint--link-at-point-p))
        (push (cons (point) current-window) link-positions))
      (while link-pos
        (push (cons link-pos current-window) link-positions)
        (goto-char link-pos)
        (setq link-pos (funcall get-next-link end-bound)))
      (nreverse link-positions))))

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

(defun link-hint--collect-visible-links ()
  "Collect all visible links."
  (let (link-positions)
    (dolist (pair (link-hint--find-visible-regions
                   (window-start) (window-end)))
      (setq link-positions
            (append link-positions
                    (link-hint--collect (car pair) (cdr pair)
                                        #'link-hint--next-link-pos))))
    link-positions))

;;; Commands for Operating on a Link at the Point
(defmacro link-hint--types-at-point-let-wrapper (body)
  "Wrap BODY in let statement that checks for supported types at the point."
  `(let* ((text-properties (text-properties-at (point)))
          (shr-url (plist-get text-properties 'shr-url))
          (org-link (plist-get text-properties 'htmlize-link))
          (text-url (when (looking-at link-hint-url-regexp)
                      (url-get-url-at-point)))
          (file-link (ffap-file-at-point))
          ;; will work for attachments in addition to mail-tos and urls
          (mu4e-url (plist-get text-properties 'mu4e-url))
          (mu4e-att (plist-get text-properties 'mu4e-attnum))
          (help-link (plist-get text-properties 'help-args))
          (info-link (or (equal (plist-get text-properties 'font-lock-face)
                                'info-xref)
                         (equal (plist-get text-properties 'font-lock-face)
                                'info-xref-visited)))
          (package-description-link
           (equal (plist-get text-properties 'action)
                  'package-menu-describe-package))
          (package-keyword-link
           (equal (plist-get text-properties 'action)
                  'package-keyword-button-action))
          (package-install-link
           (equal (plist-get text-properties 'action)
                  'package-install-button-action))
          (compilation-link
           (plist-get text-properties 'compilation-message))
          (w3m-link
           (plist-get text-properties 'w3m-href-anchor))
          (customize-link (and (eq major-mode 'Custom-mode)
                               (button-at (point))))
          (other-button-link (button-at (point))))
     ,body))

;; TODO consider having variables in let-wrapper cover everything in all-types
(defun link-hint--link-at-point-p ()
  (link-hint--types-at-point-let-wrapper
   (when (or shr-url
             org-link
             text-url
             file-link
             mu4e-url
             mu4e-att
             help-link
             info-link
             package-description-link
             package-keyword-link
             package-install-link
             compilation-link
             w3m-link
             customize-link
             other-button-link)
     t)))

;;;###autoload
(defun link-hint-open-link-at-point ()
  "Open a link of any supported type at the point."
  (interactive)
  (let (no-restore-position
        link-text)
    (setq
     link-text
     (link-hint--types-at-point-let-wrapper
      (cond (shr-url (browse-url shr-url)
                     shr-url)
            ;; org-open-at-point won't work e.g. for =http://address.com= even
            ;; though (org-next-link) will jump to it
            (org-link
             (let ((uri (plist-get org-link :uri)))
               (condition-case err
                   (org-open-at-point)
                 ('error (org-open-link-from-string
                          uri)))
               ;; all external links have a colon except for file:
               ;; other than file links, links without a colon refer to
               ;; headings in the same file (the point should not be restored)
               (unless (and (string-match
                             (rx (or (and bol (or "/" "./" "~/"))
                                     (and (0+ anything) ":" (0+ anything))))
                             uri)
                            (not (string-match (rx bol "id:") uri)))
                 (setq no-restore-position t))
               uri))
            (text-url (browse-url text-url)
                      text-url)
            ;; distinguish between opening in browser and view-atachment?
            (mu4e-url (mu4e~view-browse-url-from-binding)
                      mu4e-url)
            (mu4e-att (mu4e-view-open-attachment nil mu4e-att)
                      nil)
            ((or help-link
                 package-keyword-link
                 package-install-link)
             (push-button)
             nil)
            (info-link (Info-follow-nearest-node)
                       nil)
            (package-description-link (package-menu-describe-package)
                                      nil)
            (compilation-link (compile-goto-error)
                              nil)
            (w3m-link (w3m-view-this-url)
                      (setq no-restore-position t)
                      ;; TODO: actually get the url
                      nil)
            (customize-link (Custom-newline (point))
                            nil)
            ;; lowest precedence
            (other-button-link (push-button)
                               nil)
            (file-link (find-file-at-point file-link)
                       file-link)
            ;; TODO maybe make this an error
            (t (message "There is no supported link at the point.")))))
    (list no-restore-position "Opened" link-text)))

;;;###autoload
(defun link-hint-copy-link-at-point ()
  "Copy a link of any supported type at the point.
See the default value of `link-hint-copy-ignore-types' for the unsupported
types."
  (interactive)
  (list
   nil
   "Copied"
   (link-hint--types-at-point-let-wrapper
    (cond (shr-url (kill-new shr-url))
          (org-link (kill-new (plist-get org-link :uri)))
          (text-url (kill-new text-url))
          (file-link (kill-new (ffap-file-at-point)))
          (mu4e-url (kill-new mu4e-url))
          (mu4e-att (mu4e-view-save-attachment-single nil mu4e-att)
                    nil)
          (t (message "There is no supported link at the point."))))))

;;; Avy Commands
(defun link-hint--link-action
    (action &optional require-multiple-links get-links)
  "Jump to a url using avy and execute ACTION.
When REQUIRE-MULTIPLE-LINKS is non-nil, this function will return nil if there
is only one visible link. When GET-LINKS is non-nil, the list of visible links
will be returned instead of calling avy then ACTION."
  (let ((saved-pos (point))
        (saved-win (get-buffer-window))
        (avy-all-windows link-hint-avy-all-windows)
        (avy-all-windows-alt link-hint-avy-all-windows-alt)
        (avy-background link-hint-avy-background)
        (avy-ignored-modes link-hint-ignored-modes)
        (avy-keys link-hint-avy-keys)
        ;; prevent window from shifting avy overlays out of view
        (scroll-margin 0)
        link-positions)
    (avy-dowindows current-prefix-arg
      (setq link-positions
            (append link-positions (link-hint--collect-visible-links))))
    (cond ((not link-positions)
           (when link-hint-message
             (message "No links found.")))
          ((and require-multiple-links
                (not (cdr link-positions)))
           (when link-hint-message
             (message "Only one link found. Multiple links required.")))
          (t
           (if get-links
               link-positions
             ;; FIXME: should this be user configurable?
             ;; shouldn't do this for links that open new windows
             ;; (especially if those windows disappear on losing focus)
             ;; (save-selected-window..)
             ;; save-excursion
             (cond ((> (length link-positions) 1)
                    (avy--process link-positions
                                  (avy--style-fn link-hint-avy-style)))
                   (t
                    (select-window (cdar link-positions))
                    (goto-char (caar link-positions))))
             (let* ((values (funcall action))
                    (no-restore-position (when (consp values)
                                           (car values)))
                    (action-message (when (consp values)
                                      (cadr values)))
                    ;; only non-nil for copyable links
                    (link-text (when (consp values)
                                 (caddr values))))
               (unless no-restore-position
                 (with-selected-window saved-win
                   (goto-char saved-pos)))
               (when (and link-hint-message
                          (not require-multiple-links))
                 (message "%s %s"
                          (or action-message
                              (format "Called `%s' on" action))
                          (or (concat link-text ".")
                              "a link.")))
               ;; for multiple-link-action, as action is #'point
               values))))))

;;;###autoload
(defun link-hint-open-link ()
  "Use avy to select and open a visible link."
  (interactive)
  (link-hint--link-action #'link-hint-open-link-at-point))

;; reason for emacs 24.1 dependency:
;;;###autoload
(defun link-hint-copy-link ()
  "Copy a visible link of a supported type to the kill ring with avy.
`select-enable-clipboard' and `select-enable-primary' can be set to non-nil
values to copy the link to the clipboard and/or primary as well. See the
default value of `link-hint-copy-ignore-types' for the unsupported types.
When selecting a mu4e attachment with this, it will prompt for a location to
save (since this is the closest behaviour to copying)."
  (interactive)
  (let ((link-hint-ignore-types (append link-hint-ignore-types
                                        link-hint-copy-ignore-types)))
    (link-hint--link-action #'link-hint-copy-link-at-point)))

(defun link-hint--multiple-link-action (action)
  "Move point to a link selected by avy and execute ACTION.
The point will be returned to its previous location afterwards.
This function will not do anything if only one link is visible."
  (let ((link-hint-ignore-types
         (append link-hint-ignore-types
                 link-hint-act-on-multiple-ignore-types))
        current-point
        point-list
        (num-links 0)
        action-message)
    (while (setq current-point
                 (ignore-errors
                   (link-hint--link-action #'point t)))
      (push current-point point-list))
    (when point-list
      (save-excursion
        (dolist (point (nreverse point-list))
          (goto-char point)
          (let (link-hint-message)
            (setq action-message (cadr (funcall action))))
          (cl-incf num-links)))
      (when link-hint-message
        (message "%s %d links."
                 (or action-message (format "Called `%s' on" action))
                 num-links)))))

;;;###autoload
(defun link-hint-open-multiple-links ()
  "Use avy to select and open multiple visible links at once.
The links will be opened as soon as a non-hint key (a key not appearing in an
overlay) is pressed. More than one link must be visible for this command to have
an effect."
  (interactive)
  (link-hint--multiple-link-action #'link-hint-open-link-at-point))

;;;###autoload
(defun link-hint-copy-multiple-links ()
  "Use avy to select and copy multiple, visible links at once to the kill ring.
See `link-hint-copy-link' for more information. More than one supported link
must be visible for this command to have an effect."
  (interactive)
  (let ((link-hint-ignore-types (append link-hint-ignore-types
                                        link-hint-copy-ignore-types)))
    (link-hint--multiple-link-action #'link-hint-copy-link-at-point)))

(defun link-hint--all-links-action (action)
  "Call ACTION on the location of every visible link in the buffer.
The point will be returned to its previous location afterwards."
  ;; * so ignored types takes effect for link-hint--link-action
  (let* ((link-hint-ignore-types
          (append link-hint-ignore-types
                  link-hint-act-on-all-ignore-types))
         (point-list (link-hint--link-action nil nil t))
         (num-links 0)
         action-message)
    (save-excursion
      (dolist (point (nreverse point-list))
        (goto-char (car point))
        (let (link-hint-message)
          (setq action-message (cadr (funcall action))))
        (cl-incf num-links)))
    (when link-hint-message
      (message "%s %d links."
               (or action-message (format "Called `%s' on" action))
               num-links))))

;;;###autoload
(defun link-hint-open-all-links ()
  "Open all visible links."
  (interactive)
  (link-hint--all-links-action #'link-hint-open-link-at-point))

;;;###autoload
(defun link-hint-copy-all-links ()
  "Copy all visible links of a supported type.
See `link-hint-copy-link' for more information."
  (interactive)
  (let ((link-hint-ignore-types (append link-hint-ignore-types
                                        link-hint-copy-ignore-types)))
    (link-hint--all-links-action #'link-hint-copy-link-at-point)))

(provide 'link-hint)
;;; link-hint.el ends here
