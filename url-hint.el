;;; url-hint.el --- Use avy to open or copy visible urls.

;; Author: Lit Wakefield <noct@openmailbox.org>
;; URL: https://github.com/noctuid/url-hint.el
;; Keywords: url
;; Package-Requires: ((avy "0.3.0") (emacs "24.1"))
;; Version: 0.1

;;; Commentary:
;; This packages gives commands for operating on visible urls with avy. It is
;; inspired by link-hinting from vim-like browsers and browser plugins such as
;; pentadactyl. For example, `url-hint-open-url' will use avy to select and open
;; a url in the current buffer. Commands also exist to open all visible urls,
;; to copy a url to kill ring (and optionally the clipboard and/or primary),
;; and to open multiple urls at once like with pentadactyl's "g;".

;; For more information see the README in the github repo.

;;; Code:
(require 'avy)
(require 'url-util)
(require 'browse-url)
(unless (boundp 'url-hint-url-regexp)
  (require 'goto-addr))

(defgroup url-hint nil
  "Gives commands for operating on visible urls with avy."
  :group 'convenience
  :prefix 'url-hint)

(defcustom url-hint-url-regexp
  goto-address-url-regexp
  "Regexp used to determine what constitutes a url.
Defaults to `goto-address-url-regxp'."
  :group 'url-hint
  :type 'regexp)

(defcustom url-hint-avy-style
  avy-style
  "Method for displaying avy overlays.
Defaults to `avy-style'."
  :group 'url-hint
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At Full" at-full)
          (const :tag "Post" post)
          (const :tag "De Bruijn" de-bruijn)))

(defcustom url-hint-avy-keys
  avy-keys
  "Keys used for selecting urls.
Defaults to `avy-keys'."
  :group 'url-hint
  :type '(repeat :tag "Keys" (choice (character :tag "char"))))

(defun url-hint-open-url ()
  "Use avy to select a visible url to open."
  (interactive)
  (let ((start-bound (window-start))
        (end-bound (window-end))
        avy-all-windows)
    (save-excursion
      (avy--generic-jump url-hint-url-regexp nil url-hint-avy-style
                         start-bound end-bound)
      (browse-url-at-point))))

;; reason for emacs 24.1 dependency:
(defun url-hint-copy-url ()
  "Use avy to copy a visible url to the kill ring (and to the clipboard when
`select-enable-clipboard' is non-nil and to the primary selection when
`select-enable-primary' is non-nil)."
  (interactive)
  (let ((start-bound (window-start))
        (end-bound (window-end))
        avy-all-windows)
    (save-excursion
      (avy--generic-jump url-hint-url-regexp nil url-hint-avy-style
                         start-bound end-bound)
      (kill-new (url-get-url-at-point)))))

(defun url-hint-open-multiple-urls ()
  "Use avy to select multiple visible urls to open.
The urls will be opened s soon as a non-hint key (a key not appearing in an
overlay) is pressed."
  (interactive)
  (let ((start-bound (window-start))
        (end-bound (window-end))
        (start-point (point))
        urls
        avy-all-windows)
    (while (ignore-errors
             (avy--generic-jump url-hint-url-regexp nil url-hint-avy-style
                                start-bound end-bound))
      (push (url-get-url-at-point) urls)
      (goto-char start-point))
    (dolist (url (nreverse urls))
      (browse-url url))))

(defun url-hint-open-all-urls ()
  "Open all visible urls."
  (interactive)
  (let ((start-bound (window-start))
        (end-bound (window-end))
        urls)
    (save-excursion
      (goto-char start-bound)
      (while (and (re-search-forward url-hint-url-regexp end-bound t)
                  (not (invisible-p (point))))
        (push (url-get-url-at-point) urls)))
    (dolist (url (nreverse urls))
      (browse-url url))))

(provide 'url-hint)
;;; url-hint.el ends here
