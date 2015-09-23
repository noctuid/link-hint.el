;;; link-hint.el --- Use avy to open or copy visible urls.

;; Author: Lit Wakefield <noct@openmailbox.org>
;; URL: https://github.com/noctuid/link-hint.el
;; Keywords: url
;; Package-Requires: ((avy "0.3.0") (emacs "24.1"))
;; Version: 0.1

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

(defconst link-hint-all-types-list
  '(text-url
    shr-url
    htmlize-url
    mu4e-url
    mu4e-mailto
    mu4e-attachment
    help-link
    info-link)
  "List containing all suported link types.")

(define-widget 'link-hint-link-type-set 'lazy
  "A set of link types supported by link-hint."
  :type '(set
          (const :tag "Text url" text-url)
          (const :tag "Simple HTML Renderer url" shr-url)
          (const :tag "Htmlize (org mode) url" htmlize-url)
          (const :tag "Mu4e url" mu4e-url)
          (const :tag "Mu4e mailto address" mu4e-mailto)
          (const :tag "Mu4e attachment" mu4e-attachment)
          (const :tag "Help mode link" help-link)
          (const :tag "Info mode link" info-link)))

(defcustom link-hint-ignore-types
  nil
  "Types to be ignored when selecting a url with avy."
  :group 'link-hint
  :type 'link-hint-link-type-set)

(defcustom link-hint-act-on-multiple-ignore-types
  '(mu4e-mailto mu4e-attachment help-link info-link)
  "Types of links to ignore with commands that act on multiple visible links.
Thes commands are `link-hint-open-multiple-links' and
`link-hint-copy-multiple-links'."
  :group 'link-hint
  :type 'link-hint-link-type-set)

(defcustom link-hint-act-on-all-ignore-types
  '(mu4e-mailto mu4e-attachment help-link info-link)
  "Types of links to ignore with commands that act on all visible links.
These commands are `link-hint-open-all-links' and
`link-hint-copy-all-links'."
  :group 'link-hint
  :type 'link-hint-link-type-set)

(defun link-hint--find-text-url (&optional start-bound end-bound)
  "Find the first visible plain text url location.
Only the range between just after START-BOUND and the END-BOUND will be searched."
  (save-excursion
    (let* ((start-bound (or start-bound (window-start)))
           (end-bound (or end-bound (window-end)))
           (match-pos
            (progn (goto-char start-bound)
                   (right-char)
                   (re-search-forward link-hint-url-regexp end-bound t)))
           (match-save-pos (when match-pos (match-beginning 0))))
      (while (and match-pos
                  (invisible-p (1- match-pos)))
        (setq match-pos
              (re-search-forward link-hint-url-regexp end-bound t))
        (when match-pos
          (setq match-save-pos (match-beginning 0))))
      (when (and match-save-pos
                 (not (= match-save-pos start-bound))
                 (not (invisible-p match-save-pos)))
        match-save-pos))))

(defun link-hint--next-text-url (&optional end-bound)
  "Find the next visible plain text url location.
Only the range between just after the point and END-BOUND will be searched."
  (if end-bound
      (link-hint--find-text-url (point) end-bound)
    (link-hint--find-text-url (point))))

(defun link-hint--find-property (property &optional start-bound end-bound)
  "Find visible location where PROPERTY exists.
Only the range from between just after the START-BOUND and the END-BOUND will be
searched."
  (save-excursion
    (let* ((start-bound (or start-bound (window-start)))
           (end-bound (or end-bound (window-end)))
           (match-pos (goto-char
                       (next-single-property-change start-bound property
                                                    nil end-bound))))
      (while (not (or (and (plist-get (text-properties-at (point)) property)
                           (not (invisible-p (point))))
                      ;; next-single-property returns limit if match not found
                      (= match-pos end-bound)))
        (setq match-pos
              (goto-char
               (next-single-property-change (point) property nil end-bound))))
      (when (plist-get (text-properties-at (point)) property)
        (point)))))

(defun link-hint--next-property (property &optional end-bound)
  "Find the next visible location where PROPERTY exists.
Only the range from between just after the point and END-BOUND will be
searched."
  (if end-bound
      (link-hint--find-property property (point) end-bound)
    (link-hint--find-property property (point))))

(defun link-hint--find-property-with-value
    (property value &optional start-bound end-bound)
  "Find the first visible location where PROPERTY has VALUE.
Only the range from between just after the START-BOUND and the END-BOUND will be
searched. When VALUE is not found, nil will be returned."
  (save-excursion
    (let ((start-bound (or start-bound (window-start)))
          (end-bound (or end-bound (window-end)))
          (next-change-pos (next-single-property-change start-bound property
                                                        nil end-bound))
          last-change-pos
          match-pos)
      (while (and (not match-pos)
                  (not (equal next-change-pos last-change-pos)))
        (setq last-change-pos next-change-pos)
        (goto-char next-change-pos)
        (if (and (equal (plist-get (text-properties-at (point)) property)
                        value)
                 (not (invisible-p (point))))
            (setq match-pos (point))
          (setq next-change-pos
                (next-single-property-change (point) property
                                             nil end-bound))))
      (when match-pos
        match-pos))))

(defun link-hint--next-property-with-value (property value &optional end-bound)
  "Find the first visible location where PROPERTY has VALUE.
Only the range from between just after the point and the END-BOUND will be
searched. When VALUE is not found, nil will be returned."
  (if end-bound
      (link-hint--find-property-with-value property value (point) end-bound)
    (link-hint--find-property-with-value property value (point))))

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

(defun link-hint--next-link-pos (&optional end-bound)
  "Find the closest visible link of all types that are not ignored.
Only the range between just after the point and END-BOUND will be searched."
  (let* ((end-bound (or end-bound (window-end)))
         (text-url-pos (when (not (member 'text-url link-hint-ignore-types))
                         (link-hint--next-text-url end-bound)))
         (shr-url-pos (when (not (member 'shr-url link-hint-ignore-types))
                        (link-hint--next-property 'shr-url end-bound)))
         (htmlize-url-pos
          (when (not (member 'htmlize-url link-hint-ignore-types))
            (link-hint--next-property 'htmlize-link end-bound)))
         (mu4e-link-pos (link-hint--next-property 'mu4e-url end-bound))
         (mu4e-link-text (when mu4e-link-pos
                           (plist-get (text-properties-at mu4e-link-pos) 'mu4e-url)))
         (mu4e-url-pos
          (when (and mu4e-link-text
                     (not (member 'mu4e-url link-hint-ignore-types))
                     (not (string-prefix-p "mailto" mu4e-link-text)))
            mu4e-link-pos))
         (mu4e-mailto-pos
          (when (and mu4e-link-text
                     (not (member 'mu4e-mailto link-hint-ignore-types))
                     (string-prefix-p "mailto" mu4e-link-text))
            mu4e-link-pos))
         (mu4e-att-pos
          (when (not (member 'mu4e-attachment link-hint-ignore-types))
            (link-hint--next-property 'mu4e-attnum end-bound)))
         (help-link-pos (when (not (member 'help-link link-hint-ignore-types))
                          (link-hint--next-property 'help-args end-bound)))
         (info-link-pos
          (when (not (member 'info-link link-hint-ignore-types))
            (link-hint--next-property-with-value
             'font-lock-face 'info-xref end-bound)))
         (info-link-visited-pos
          (when (not (member 'info-link link-hint-ignore-types))
            (link-hint--next-property-with-value
             'font-lock-face 'info-xref-visited end-bound)))
         (closest-pos
          (link-hint--min (list text-url-pos
                                shr-url-pos
                                htmlize-url-pos
                                mu4e-url-pos
                                mu4e-mailto-pos
                                mu4e-att-pos
                                help-link-pos
                                info-link-pos
                                info-link-visited-pos))))
    (when closest-pos
      closest-pos)))

;;;###autoload
(defun link-hint-open-link-at-point ()
  "Open a link of any supported type at the point."
  (interactive)
  (let* ((text-properties (text-properties-at (point)))
         (shr-url (plist-get text-properties 'shr-url))
         (htmlize-url (plist-get text-properties 'htmlize-link))
         (text-url (looking-at link-hint-url-regexp))
         (mu4e-url (plist-get text-properties 'mu4e-url))
         (mu4e-att (plist-get text-properties 'mu4e-attnum))
         ;; will work for attachments in addition to mail-tos and urls
         (help-link (plist-get text-properties 'help-args))
         (info-link (or (equal (plist-get text-properties 'font-lock-face)
                               'info-xref)
                        (equal (plist-get text-properties 'font-lock-face)
                               'info-xref-visited))))
    (cond (shr-url (browse-url shr-url))
          (htmlize-url (browse-url (cadr htmlize-url)))
          (text-url (browse-url-at-point))
          (mu4e-url (mu4e~view-browse-url-from-binding))
          (mu4e-att (mu4e-view-open-attachment nil mu4e-att))
          ;; distinguish between opening in browser and view-atachment?
          (help-link (push-button))
          (info-link (Info-follow-nearest-node))
          (t (message "There is no supported link at the point.")))))

;;;###autoload
(defun link-hint-copy-link-at-point ()
  "Copy a link of any supported type at the point."
  (interactive)
  (let* ((text-properties (text-properties-at (point)))
         (shr-url (plist-get text-properties 'shr-url))
         (htmlize-url (plist-get text-properties 'htmlize-link))
         (text-url (looking-at link-hint-url-regexp))
         (mu4e-url (plist-get text-properties 'mu4e-url))
         (mu4e-att (plist-get text-properties 'mu4e-attnum)))
    (cond (shr-url (kill-new shr-url))
          (htmlize-url (kill-new (cadr htmlize-url)))
          (text-url (let ((url (url-get-url-at-point)))
                      (when url (kill-new url))))
          (mu4e-url (kill-new mu4e-url))
          (mu4e-att (mu4e-view-save-attachment-single nil mu4e-att))
          (t (message "There is no supported link at the point.")))))

(defun link-hint--link-action
    (action &optional require-multiple-links get-links)
  "Jump to a url using avy and execute ACTION.
When REQUIRE-MULTIPLE-LINKS is non-nil, this function will return nil if there
is only one visible link. When GET-LINKS is non-nil, the list of visible links
will be returned instead of calling avy then ACTION."
  (save-excursion
    (goto-char (1- (window-start)))
    (let* ((end-bound (window-end))
           (current-link (link-hint--next-link-pos end-bound))
           (current-window (get-buffer-window))
           link-positions
           ;; prevent window from shifting avy overlays out of view
           (scroll-margin 0)
           avy-all-windows)
      (while current-link
        (push (cons current-link current-window) link-positions)
        (goto-char current-link)
        (setq current-link (link-hint--next-link-pos end-bound)))
      (when (and link-positions
                 (if require-multiple-links
                     (> (length link-positions) 1)
                   t))
        (if get-links
            link-positions
          (avy--process (nreverse link-positions)
                        (avy--style-fn link-hint-avy-style))
          (funcall action))))))

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
values to copy the link to the clipboard and/or primary as well. Info and help
mode links are not supported. When selecting a mu4e attachment with this,
it will prompt for a location to save (since this is the closest behaviour to
copying"
  (interactive)
  (let ((link-hint-ignore-types (append '(help-link info-link)
                                        link-hint-ignore-types)))
    (link-hint--link-action #'link-hint-copy-link-at-point)))

(defun link-hint--multiple-link-action (action)
  "Move point to a link selected by avy and execute ACTION.
The point will be returned to its previous location afterwards.
This function will not do anything if only one link is visible."
  (let ((link-hint-ignore-types
         (append link-hint-ignore-types
                 link-hint-act-on-multiple-ignore-types))
        current-point
        point-list)
    (while (setq current-point
                 (ignore-errors
                   (link-hint--link-action #'point t)))
      (push current-point point-list))
    (save-excursion
      (dolist (point (nreverse point-list))
        (goto-char point)
        (funcall action)))))

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
See `link-hint-copy-link' for more information on supported types and using
the clipboard/primary. More than one link must be visible for this command to
have an effect."
  (interactive)
  (link-hint--multiple-link-action #'link-hint-copy-link-at-point))

(defun link-hint--all-links-action (action)
  "Call ACTION on the location of every visible link in the buffer.
The point will be returned to its previous location afterwards."
  ;; * so ignored types takes effect for link-hint--link-action
  (let* ((link-hint-ignore-types
          (append link-hint-ignore-types
                  link-hint-act-on-all-ignore-types))
         (point-list (link-hint--link-action nil nil t)))
    (save-excursion
      (dolist (point (nreverse point-list))
        (goto-char (car point))
        (funcall action)))))

;;;###autoload
(defun link-hint-open-all-links ()
  "Open all visible links."
  (interactive)
  (link-hint--all-links-action #'link-hint-open-link-at-point))

;;;###autoload
(defun link-hint-copy-all-links ()
  "Copy all visible links.
See `link-hint-copy-link' for more information on supported types and using
the clipboard/primary."
  (interactive)
  (link-hint--all-links-action #'link-hint-copy-link-at-point))

(provide 'link-hint)
;;; link-hint.el ends here
