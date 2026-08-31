;;; workspaces.el --- Leverage tab-bar and project for buffer-isolated workspaces  -*- lexical-binding: t -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Mou Tong <mou.tong@qq.com>
;; Version: 1.1.0
;; Package-Requires: ((emacs "30.1"))
;; URL: https://github.com/dalugm/workspaces
;; Keywords: convenience, frames

;; Copyright (c) 2022-2023 Colin McLear
;; Copyright (c) 2024-2026 Mou Tong

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides several functions to facilitate a frame-based
;; tab workflow with one workspace per tab, integration with `project'
;; (for project-based workspaces) and buffer isolation per tab (i.e. a
;; "workspace").

;;; Acknowledgements
;; Much of the package code is inspired by:

;; - https://github.com/kaz-yos/emacs
;; - https://github.com/wamei/elscreen-separate-buffer-list/issues/8
;; - https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/
;; - https://github.com/minad/consult#multiple-sources
;; - https://github.com/florommel/bufferlo
;; - https://github.com/natecraddock/workspaces.nvim

;;; Code:

;;;; Requirements

(require 'tab-bar)
(require 'project)
(require 'subr-x)
(require 'vc)
(require 'seq)

(declare-function magit-init "magit-status")
(declare-function magit-status-setup-buffer "magit-status")

;;;; Variables

(defgroup workspaces nil
  "Manage tab/workspace buffers."
  :group 'convenience)

(defcustom workspaces-include-buffers '("*scratch*" "*Messages*")
  "Buffers that should always get included in a new workspace.
This is a list of buffer names.  `workspaces-exclude-buffers'
takes precedence over this option."
  :group 'workspaces
  :type '(repeat string))

(defcustom workspaces-exclude-buffers nil
  "Buffers that should always get excluded in a new workspace.
This is a list of buffer names.  It takes precedence over
`workspaces-include-buffers'."
  :group 'workspaces
  :type '(repeat string))

(defcustom workspaces-project-switch-commands project-switch-commands
  "Available commands when switching between projects.
Change this value if you wish to run a specific command, such as
`find-file' on project switch.  Otherwise this will default to
the value of `project-switch-commands'."
  :group 'workspaces
  :type 'sexp)

(defcustom workspaces-use-truepath nil
  "Whether to resolve symbolic links in project paths."
  :group 'workspaces
  :type 'boolean)

;;;; Create Buffer Workspace

(defvar workspaces--frame-buffer-predicates
  (make-hash-table :weakness 'key)
  "Saved and installed buffer predicates, keyed by frame.")

(defun workspaces--included-buffer-p (buffer)
  "Return non-nil when BUFFER is explicitly shared with every workspace."
  (let ((name (buffer-name buffer)))
    (and name
         (member name workspaces-include-buffers)
         (not (member name workspaces-exclude-buffers)))))

(defun workspaces--reset-buffer-list ()
  "Reset the current TAB's `buffer-list'.
Only explicitly included, non-excluded buffers are retained in
`buffer-list' and `buried-buffer-list'."
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html
  ;; The current-tab uses `buffer-list' and `buried-buffer-list'.
  ;; A hidden tab keeps these as `wc-bl' and `wc-bbl'.
  (dolist (parameter '(buffer-list buried-buffer-list))
    (set-frame-parameter
     nil parameter
     (seq-filter #'workspaces--included-buffer-p
                 (frame-parameter nil parameter)))))

(defun workspaces--tab-post-open-function (_tab)
  "Update buffer list on new tab creation."
  (workspaces--reset-buffer-list))

;;;; Filter Workspace Buffers

(defun workspaces--local-buffer-p (buffer &optional frame)
  "Return whether BUFFER is local to FRAME's current workspace."
  (let ((name (buffer-name buffer)))
    (and (not (member name workspaces-exclude-buffers))
         (or (member name workspaces-include-buffers)
             (memq buffer (frame-parameter frame 'buffer-list))))))

(defun workspaces--buffer-predicate (frame buffer)
  "Apply the workspace and saved predicates for BUFFER on FRAME."
  (let* ((entry (gethash frame workspaces--frame-buffer-predicates))
         (saved (and entry (aref entry 0))))
    (and (workspaces--local-buffer-p buffer frame)
         (or (null saved) (funcall saved buffer)))))

(defun workspaces--set-buffer-predicate (frame)
  "Add workspace filtering to FRAME's existing buffer predicate."
  (unless (gethash frame workspaces--frame-buffer-predicates)
    (let* ((saved (frame-parameter frame 'buffer-predicate))
           (installed (apply-partially #'workspaces--buffer-predicate frame)))
      (puthash frame (vector saved installed)
               workspaces--frame-buffer-predicates)
      (set-frame-parameter frame 'buffer-predicate installed))))

(defun workspaces--reset-buffer-predicate (frame)
  "Restore FRAME's buffer predicate saved by `workspaces-mode'."
  (when-let* ((entry (gethash frame workspaces--frame-buffer-predicates)))
    (when (eq (frame-parameter frame 'buffer-predicate) (aref entry 1))
      (set-frame-parameter frame 'buffer-predicate (aref entry 0)))
    (remhash frame workspaces--frame-buffer-predicates)))

(defun workspaces--tabs (&optional frame)
  "Return FRAME's canonical list of tabs."
  (tab-bar-tabs frame))

(defun workspaces--buffers-for-tab (tab &optional frame)
  "Return live buffers belonging to TAB on FRAME."
  (seq-filter #'buffer-live-p
              (if (eq 'current-tab (car tab))
                  (frame-parameter frame 'buffer-list)
                (or (alist-get 'wc-bl tab)
                    (mapcar (lambda (buffer)
                              (if (bufferp buffer)
                                  buffer
                                (get-buffer buffer)))
                            (window-state-buffers (alist-get 'ws tab)))))))

(defun workspaces--buffer-list (&optional frame)
  "Return live buffers associated with FRAME's current workspace."
  (seq-filter #'buffer-live-p
              (frame-parameter frame 'buffer-list)))

(defun workspaces--tab-buffer-alist (&optional frame)
  "Return an alist mapping each workspace on FRAME to its live buffers."
  (mapcar (lambda (tab)
            (cons (alist-get 'name tab)
                  (workspaces--buffers-for-tab tab frame)))
          (workspaces--tabs frame)))

;;;; Project Workspace Helper Functions

(defun workspaces--vc-backend (directory)
  "Return the VC backend responsible for DIRECTORY, or nil."
  (and (file-directory-p directory)
       (vc-responsible-backend directory t)))

(defun workspaces--current-name ()
  "Return the name of the current workspace."
  (alist-get 'name
             (seq-find (lambda (tab) (eq (car tab) 'current-tab))
                       (workspaces--tabs))))

(defun workspaces--list ()
  "Return a list of `tab-bar' tabs/workspaces."
  (mapcar (lambda (tab) (alist-get 'name tab))
          (workspaces--tabs)))

;;;; Interactive Functions

;;;;; Buffer Functions

(defun workspaces--kill-buffer (&optional buffer)
  "Bury and remove BUFFER from current workspace.
If BUFFER is nil, remove current buffer."
  (let ((buffer (get-buffer (or buffer (current-buffer)))))
    (cond
     ((eq buffer (window-buffer (selected-window)))
      (if (one-window-p t)
          (bury-buffer)
        (delete-window)))
     ((get-buffer-window buffer)
      (select-window (get-buffer-window buffer) t)
      (if (one-window-p t)
          (bury-buffer)
        (delete-window)))
     (t
      (message "Buffer `%s' removed from `%s' workspace."
               buffer (workspaces--current-name))))
    (bury-buffer buffer)
    (let ((buf-list (frame-parameter nil 'buffer-list))
          (buried-list (frame-parameter nil 'buried-buffer-list)))
      (set-frame-parameter nil 'buffer-list (delete buffer buf-list))
      (set-frame-parameter nil 'buried-buffer-list (delete buffer buried-list)))))

(defun workspaces-kill-buffer (buffer)
  "Remove selected BUFFER from frame's buffer list."
  (interactive
   (list
    (let ((blst (mapcar (lambda (b) (buffer-name b))
                        (workspaces--buffer-list))))
      ;; Select buffer.
      (read-buffer (format "Remove buffer from `%s' workspace: "
                           (workspaces--current-name))
                   nil t
                   (lambda (b) (member (if (stringp b) b (car b)) blst))))))
  ;; Remove buffer from current workspace's buffer list.
  (workspaces--kill-buffer buffer))

(defun workspaces-switch-to-buffer (buffer &optional norecord force-same-window)
  "Display the local buffer BUFFER in the selected window.
This is the frame/tab-local equivalent to `switch-to-buffer'.
The arguments NORECORD and FORCE-SAME-WINDOW are passed to `switch-to-buffer'."
  (interactive
   (list
    (let ((blst (delete (buffer-name)
                        (mapcar #'buffer-name (workspaces--buffer-list)))))
      (read-buffer
       "Switch to local buffer: " blst nil
       (lambda (b) (member (if (stringp b) b (car b)) blst))))))
  (switch-to-buffer buffer norecord force-same-window))

(defun workspaces--buffer-tabs (buffer &optional frame)
  "Return names of workspaces on FRAME that contain BUFFER."
  (when-let* ((buffer (and buffer (get-buffer buffer))))
    (seq-keep (lambda (entry)
                (and (memq buffer (cdr entry)) (car entry)))
              (workspaces--tab-buffer-alist frame))))

(defun workspaces-switch-buffer-and-tab (buffer &optional norecord force-same-window)
  "Switch to BUFFER and its workspace, or create BUFFER.
If BUFFER is absent from all workspace buffer lists, switch to it
in the current workspace.  NORECORD and FORCE-SAME-WINDOW are
passed to `switch-to-buffer'."
  (interactive
   (list
    (let ((blst (delete (buffer-name) (mapcar #'buffer-name (buffer-list)))))
      (read-buffer
       "Switch to tab for buffer: " blst nil
       (lambda (b) (member (if (stringp b) b (car b)) blst))))))

  (let* ((live-buffer (get-buffer buffer))
         (buffer-tabs (workspaces--buffer-tabs live-buffer)))
    (cond
     ;; Buffer belongs to exactly one workspace.
     ((and live-buffer (length= buffer-tabs 1))
      (tab-bar-switch-to-tab (car buffer-tabs))
      (workspaces-switch-to-buffer buffer norecord force-same-window))
     ;; Buffer is shared by multiple workspaces.
     ((and live-buffer (length> buffer-tabs 1))
      (tab-bar-switch-to-tab (completing-read "Select tab: " buffer-tabs))
      (workspaces-switch-to-buffer buffer norecord force-same-window))
     ;; Buffer exists but is not assigned to any workspace.
     (live-buffer
      (workspaces-switch-to-buffer buffer norecord force-same-window))
     ;; Buffer does not exist.
     ((yes-or-no-p "Buffer not found -- create a new workspace with buffer?")
      (switch-to-buffer-other-tab buffer))
     ;; Create the buffer in the current workspace.
     (t
      (switch-to-buffer buffer norecord force-same-window)))))

(defun workspaces-clear-buffers (&optional frame)
  "Clear the workspace's buffer list, except for the current buffer.
If FRAME is nil, use the current frame."
  (interactive)
  (let ((buffer (if frame
                    (with-selected-frame frame
                      (current-buffer))
                  (current-buffer))))
    (set-frame-parameter frame 'buffer-list (list buffer))
    (set-frame-parameter frame 'buried-buffer-list nil)))

;;;;; Switch or Create Workspace
;; Some convenience functions for opening/closing workspaces and buffers.
;; Some of these are just wrappers around built-in functions.
;;;###autoload
(defun workspaces-switch (&optional workspace)
  "Switch to WORKSPACE, or create it when it does not exist."
  (interactive
   (list (if-let* ((tabs (workspaces--list)))
             (completing-read "Select or create workspace: " tabs)
           (completing-read "Workspace name: " nil))))
  (if (member workspace (workspaces--list))
      (tab-bar-switch-to-tab workspace)
    (tab-new)
    (tab-rename workspace)))

;;;;; Forget Workspace
(defalias 'workspaces-forget-workspace #'project-forget-project)
(defalias 'workspaces-forget-zombie #'project-forget-zombie-projects)

;;;;; Rename Workspace
(defalias 'workspaces-rename #'tab-bar-rename-tab)

;;;;; Close Workspace & Kill Buffers
(defun workspaces-close (workspace)
  "Kill all buffers and close current WORKSPACE.
With a \\[universal-argument], select a WORKSPACE to close."
  (interactive
   (list (if (equal current-prefix-arg '(4))
             (completing-read "Close workspace: " (workspaces--list))
           (workspaces--current-name))))
  (let* ((tab-buffers (workspaces--tab-buffer-alist))
         (target (assoc-string workspace tab-buffers)))
    (unless target
      (user-error "Unknown workspace: %s" workspace))
    (when (length= tab-buffers 1)
      (user-error "Attempt to close the sole workspace"))
    (let ((other-buffers
           (mapcan (lambda (entry)
                     (unless (eq entry target)
                       (copy-sequence (cdr entry))))
                   tab-buffers)))
      (unwind-protect
          (dolist (buffer (cdr target))
            (unless (or (workspaces--included-buffer-p buffer)
                        (memq buffer other-buffers))
              (kill-buffer buffer)))
        (tab-bar-close-tab-by-name workspace)))))

;;;;; Open project in workspace.
(defun workspaces--generate-name (base-name existing-workspaces)
  "Generate a unique tab name from BASE-NAME and EXISTING-WORKSPACES."
  (let ((counter 2)
        (new-name base-name))
    (while (member new-name existing-workspaces)
      (setq new-name (format "%s<%d>" base-name counter)
            counter (1+ counter)))
    new-name))

;; Overwrite `read-directory-name' to create projects when necessary.
(defun workspaces--read-directory-name (prompt &optional dir default mustmatch)
  "Read with PROMPT and create a missing directory.
DIR, DEFAULT, and MUSTMATCH are passed to `read-directory-name'."
  (let ((dir-name (read-directory-name prompt dir default mustmatch)))
    (unless (file-directory-p dir-name)
      (if (y-or-n-p (format "Directory %s does not exist.  Create it?" dir-name))
          (make-directory dir-name t)
        (user-error "Directory does not exist: %s" dir-name)))
    dir-name))

;; Replace `project-prompt-project-dir' for project creation.
(defun workspaces--prompt-project-dir ()
  "Prompt the user for a directory that is one of the known project roots.
The project is chosen among projects known from the project list,
see `project-list-file'.
It's also possible to enter an arbitrary directory not in the list."
  (let* ((directory-choice "... (choose a dir)")
         (choices (append (project-known-project-roots)
                          (list directory-choice)))
         (project-directory ""))
    (while (string-empty-p project-directory)
      ;; If the user simply pressed RET, do this again until they don't.
      (setq project-directory
            (completing-read "Select project: " choices nil t)))
    (if (equal project-directory directory-choice)
        (workspaces--read-directory-name "Select directory: ")
      project-directory)))

(defun workspaces--normalize-directory (directory)
  "Return DIRECTORY in the canonical form used by workspaces."
  (file-name-as-directory
   (if workspaces-use-truepath
       (file-truename directory)
     (abbreviate-file-name (expand-file-name directory)))))

;;;###autoload
(defun workspaces-open (&optional project prefix)
  "Open PROJECT and its workspace with a descriptive name.

With universal argument PREFIX, always create a new workspace."
  (interactive
   (list (workspaces--prompt-project-dir) current-prefix-arg))
  (let* ((project-switch-commands workspaces-project-switch-commands)
         (project-directory (workspaces--normalize-directory project))
         (known-projects
          (mapcar #'workspaces--normalize-directory
                  (project-known-project-roots)))
         (existing-workspaces (workspaces--list))
         (workspace-name
          (if (member project-directory existing-workspaces)
              project-directory
            (workspaces--generate-name project-directory
                                       existing-workspaces)))
         (known-project (member project-directory known-projects))
         (create-workspace (or prefix
                               (not (member workspace-name
                                            existing-workspaces)))))
    (cond
     ;; If there is no workspace nor project, create both.
     ((not known-project)
      (tab-bar-new-tab)
      (tab-bar-rename-tab workspace-name)
      (delete-other-windows)
      ;; Git initialized if not version controlled.
      (let ((default-directory project-directory))
        (condition-case err
            (if (workspaces--vc-backend project-directory)
                (if (fboundp 'magit-status-setup-buffer)
                    (magit-status-setup-buffer project-directory)
                  ;; Keep one vc buffer window and one workspace buffer window.
                  (split-window)
                  (project-vc-dir))
              (if (fboundp 'magit-init)
                  (magit-init project-directory)
                (vc-create-repo 'Git)
                ;; Keep one vc buffer window and one workspace buffer window.
                (split-window)
                (project-vc-dir)))
          (error
           (message "Failed to initialize version control in %s: %s"
                    project-directory (error-message-string err)))))
      ;; Switch to workspace buffer window.
      (other-window 1)
      (project-switch-project project-directory)
      ;; Remember new project.
      (when-let* ((project (project-current nil project-directory)))
        (project-remember-project project)))

     ;; If project and workspace exists, but we want a new workspace.
     ((and known-project
           (member workspace-name existing-workspaces)
           create-workspace)
      (let ((new-workspace-name
             (workspaces--generate-name workspace-name existing-workspaces)))
        (tab-bar-new-tab)
        (tab-bar-rename-tab new-workspace-name)
        (project-switch-project project-directory)))

     ;; If project and workspace exists.
     ((and known-project (member workspace-name existing-workspaces))
      (tab-bar-switch-to-tab workspace-name)
      (project-switch-project project-directory))

     ;; If project exists, but no corresponding workspace, create a
     ;; new workspace.
     (known-project
      (tab-bar-new-tab)
      (tab-bar-rename-tab workspace-name)
      (project-switch-project project-directory))

     (t
      (message "No project found or created.")
      nil))))

;;;; Define Keymaps
(defvar-keymap workspaces-prefix-map
  :doc "Keymap for workspaces commands.
The keymap should be installed globally under a prefix."
  "b"   #'workspaces-switch-to-buffer
  "C-b" #'workspaces-switch-to-buffer
  "c"   #'workspaces-clear-buffers
  "C-c" #'workspaces-clear-buffers
  "f"   #'workspaces-forget-workspace
  "C-f" #'workspaces-forget-workspace
  "k"   #'workspaces-kill-buffer
  "C-k" #'workspaces-kill-buffer
  "l"   #'workspaces-switch
  "C-l" #'workspaces-switch
  "n"   #'workspaces-rename
  "C-n" #'workspaces-rename
  "o"   #'workspaces-open
  "C-o" #'workspaces-open
  "q"   #'workspaces-close
  "C-q" #'workspaces-close
  "s"   #'workspaces-switch
  "C-s" #'workspaces-switch
  "t"   #'workspaces-switch-buffer-and-tab
  "C-t" #'workspaces-switch-buffer-and-tab
  "z"   #'workspaces-forget-zombie
  "C-z" #'workspaces-forget-zombie)

;;;###autoload (autoload 'workspaces-prefix-map "workspaces" nil t 'keymap)
(defalias 'workspaces-prefix-map workspaces-prefix-map)

;;;###autoload
(define-minor-mode workspaces-mode
  "Minor mode for buffer-isolated workspaces."
  :global t
  (if workspaces-mode
      (progn
        (dolist (frame (frame-list))
          (workspaces--set-buffer-predicate frame))
        (add-hook 'after-make-frame-functions #'workspaces--set-buffer-predicate)
        (add-hook 'tab-bar-tab-post-open-functions
                  #'workspaces--tab-post-open-function))
    (dolist (frame (frame-list))
      (workspaces--reset-buffer-predicate frame))
    (remove-hook 'tab-bar-tab-post-open-functions
                 #'workspaces--tab-post-open-function)
    (remove-hook 'after-make-frame-functions #'workspaces--set-buffer-predicate)))

(provide 'workspaces)
;;; workspaces.el ends here
