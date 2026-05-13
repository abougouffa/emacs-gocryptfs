;;; gocryptfs.el --- Mount/Unmount gocryptfs vauls -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Abdelhak Bougouffa
;;
;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: July 14, 2025
;; Modified: July 16, 2025
;; Version: 0.0.2
;; Keywords: convenience files processes tools unix
;; Homepage: https://github.com/abougouffa/emacs-gocryptfs
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0

;; This file is not part of GNU Emacs.


;;; Commentary:

;; Mount and unmount gocryptfs vaults from Emacs
;;
;; This package can optionally extract the encryption key from a GPG encrypted
;; file containing the gocryptfs passphrase (See: `gocryptfs-vaults').
;; The decryption of the passphrase is performed using Emacs' `epg'.


;;; Code:

(require 'epa) ; to avoid function definition is void: `epa-passphrase-callback-function'
(require 'epg)
(autoload 'cl-every "cl-extras")

(defgroup gocryptfs nil
  "Mount and unmount gocryptfs encrypted directory from Emacs."
  :group 'tools)

(defcustom gocryptfs-command "gocryptfs"
  "The gocryptfs command."
  :group 'gocryptfs
  :type 'string)

(defcustom gocryptfs-fusermount-command (seq-find #'executable-find '("fusermount3" "fusermount"))
  "The fusermount command."
  :group 'gocryptfs
  :type 'string)

(defcustom gocryptfs-vaults '((:cipher-dir "~/.Private.cipher" :plain-dir "~/Private"))
  "A list of valuts."
  :group 'gocryptfs
  :type '(repeat
          (plist :tag "Vault"
                 :options
                 ((:plain-dir directory)
                  (:cipher-dir directory)
                  (:config-file (choice file (const :tag "Default" nil)))
                  (:gpg-passphrase-file (choice file (const :tag "Always ask" nil)))))))

(defvar gocryptfs-buffer-name " *emacs-gocryptfs*")

(defun gocryptfs--vault-cipher-dir (vault) (expand-file-name (plist-get vault :cipher-dir)))
(defun gocryptfs--vault-plain-dir (vault) (expand-file-name (plist-get vault :plain-dir)))
(defun gocryptfs--vault-config-file (vault) (when-let* ((file (plist-get vault :config-file))) (expand-file-name file)))
(defun gocryptfs--vault-gpg-passphrase-file (vault)
  (when-let* ((file (or (plist-get vault :gpg-passphrase-file)
                        (expand-file-name "passphrase-file.gpg" (gocryptfs--vault-cipher-dir vault))))
              ((file-exists-p file)))
    (expand-file-name file)))

(defun gocryptfs-get-passphrase (vault)
  "Return passphrase for VAULT from the GPG encrypted password file.

If the VAULT's :gpg-passphrase-file is not set or set but doesn't exist,
ask for the password."
  (if-let* ((gpg-file (gocryptfs--vault-gpg-passphrase-file vault)))
      (car ; The passphrase is a single line, so take only the first line
       (string-lines
        (epg-decrypt-file
         (epg-make-context)
         (expand-file-name gpg-file)
         nil)))
    (read-passwd (format "Enter password for %s: " (plist-get vault :plain-dir)))))

(defun gocryptfs-available-p ()
  "Is gocryptfs available on the current system?"
  (and (executable-find gocryptfs-command) (executable-find gocryptfs-fusermount-command) t))

(defun gocryptfs-cipher-mounted-p (vault)
  "Is VAULT chiper directory mounted?"
  (and (string-match-p (gocryptfs--vault-cipher-dir vault) (shell-command-to-string "mount")) t))

(defun gocryptfs--vault-name (vault)
  (format "%s %s %s" (gocryptfs--vault-cipher-dir vault) (propertize "->" 'face 'error) (gocryptfs--vault-plain-dir vault)))

(defun gocryptfs--read-vault ()
  (if (length= gocryptfs-vaults 1)
      (car gocryptfs-vaults)
    (let* ((choices (mapcar (lambda (vault) (cons (gocryptfs--vault-name vault) vault))
                            gocryptfs-vaults))
           (selection (completing-read "Select gocryptfs vault: " choices nil t)))
      (cdr (assoc selection choices)))))

(defun gocryptfs--call-gocryptfs (vault)
  (let* ((cipher-dir (gocryptfs--vault-cipher-dir vault))
         (plain-dir (gocryptfs--vault-plain-dir vault))
         (config-file (gocryptfs--vault-config-file vault))
         (passphrase (gocryptfs-get-passphrase vault))
         (args (append (when config-file (list "-config" config-file))
                       (list "-stdin" cipher-dir plain-dir)))
         (buffer (get-buffer-create gocryptfs-buffer-name)))
    (unwind-protect
        (progn
          (with-current-buffer buffer (erase-buffer))
          (with-temp-buffer
            (let ((coding-system-for-write 'utf-8-unix))
              (insert passphrase "\n")
              (let ((exit-code (apply #'call-process-region (point-min) (point-max) gocryptfs-command nil buffer nil args)))
                (unless (equal 0 exit-code)
                  (error "gocryptfs failed: %s" (with-current-buffer buffer (buffer-string))))))))
      (if (fboundp 'clear-string)
          (clear-string passphrase)
        (when (stringp passphrase)
          (dotimes (index (length passphrase))
            (aset passphrase index 0))))
      (setq passphrase nil))))

;;;###autoload
(defun gocryptfs-toggle-mount ()
  "Mount/Unmount gocryptfs' cipher directory."
  (interactive)
  (let ((vault (gocryptfs--read-vault)))
    (if (gocryptfs-cipher-mounted-p vault)
        (gocryptfs-umount vault)
      (gocryptfs-mount vault))))

;;;###autoload
(defun gocryptfs-mount (vault)
  "Mount gocryptfs VAULT."
  (interactive (list (gocryptfs--read-vault)))
  (unless (gocryptfs-available-p)
    (user-error "gocryptfs or fusermount not available"))
  (let ((cipher-dir (gocryptfs--vault-cipher-dir vault))
        (plain-dir (gocryptfs--vault-plain-dir vault)))
    (unless (file-directory-p cipher-dir)
      (user-error "Cipher directory %S doesn't exist" cipher-dir))
    (unless (file-directory-p plain-dir)
      (user-error "Plain directory %S doesn't exist" plain-dir))
    (when (gocryptfs-cipher-mounted-p vault)
      (user-error "Vault already mounted: %s" (gocryptfs--vault-name vault)))
    (gocryptfs--call-gocryptfs vault)
    (message "Mounted: %s" (gocryptfs--vault-name vault))))

;;;###autoload
(defun gocryptfs-umount (vault)
  "Unmount gocryptfs VAULT."
  (interactive (list (gocryptfs--read-vault)))
  (unless (gocryptfs-available-p)
    (user-error "gocryptfs or fusermount not available"))
  (let ((plain-dir (gocryptfs--vault-plain-dir vault)))
    (unless (gocryptfs-cipher-mounted-p vault)
      (user-error "Vault is not mounted: %s" (gocryptfs--vault-name vault)))
    (let ((buffer (get-buffer-create gocryptfs-buffer-name)))
      (with-current-buffer buffer (erase-buffer))
      (unless (equal 0 (call-process gocryptfs-fusermount-command nil buffer nil "-u" plain-dir))
        (user-error "Failed to unmount %S" plain-dir))
      (message "Unmounted: %s" (gocryptfs--vault-name vault)))))



(provide 'gocryptfs)
;;; gocryptfs.el ends here

;; Local Variables:
;; time-stamp-pattern: "^;; Modified: %%$"
;; time-stamp-format: "%B %d, %Y"
;; End:
