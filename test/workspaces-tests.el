;;; workspaces-tests.el --- Tests for workspaces -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'workspaces)

(ert-deftest workspaces-local-buffer-exclusion-takes-precedence ()
  (with-temp-buffer
    (rename-buffer "workspaces-test-buffer" t)
    (let ((workspaces-include-buffers (list (buffer-name)))
          (workspaces-exclude-buffers (list (buffer-name))))
      (should-not (workspaces--local-buffer-p (current-buffer))))))

(ert-deftest workspaces-buffers-for-tab-restores-from-public-window-state ()
  (with-temp-buffer
    (rename-buffer "workspaces-state-buffer" t)
    (let ((buffer (current-buffer))
          (state (save-window-excursion
                   (switch-to-buffer (current-buffer))
                   (window-state-get))))
      (should (memq buffer
                    (workspaces--buffers-for-tab
                     `(tab (name . "hidden") (ws . ,state))))))))

(ert-deftest workspaces-switch-buffer-does-not-mutate-tab-lists ()
  (let ((first (get-buffer-create "workspaces-first"))
        (second (get-buffer-create "workspaces-second"))
        switched-tab
        switched-buffer
        prompted)
    (unwind-protect
        (cl-letf (((symbol-function 'workspaces--tab-buffer-alist)
                   (lambda (&optional _frame)
                     `(("one" . (,first)) ("two" . (,second)))))
                  ((symbol-function 'tab-bar-switch-to-tab)
                   (lambda (name) (setq switched-tab name)))
                  ((symbol-function 'workspaces-switch-to-buffer)
                   (lambda (buffer &rest _args)
                     (setq switched-buffer buffer)))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _args)
                     (setq prompted t))))
          (workspaces-switch-buffer-and-tab (buffer-name second))
          (should (equal switched-tab "two"))
          (should (equal switched-buffer (buffer-name second)))
          (should-not prompted))
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest workspaces-switches-live-unassigned-buffer-in-current-tab ()
  (let ((buffer (get-buffer-create "workspaces-unassigned"))
        switched)
    (unwind-protect
        (cl-letf (((symbol-function 'workspaces--tab-buffer-alist)
                   (lambda (&optional _frame) '(("one"))))
                  ((symbol-function 'workspaces-switch-to-buffer)
                   (lambda (name &rest _args) (setq switched name))))
          (workspaces-switch-buffer-and-tab (buffer-name buffer))
          (should (equal switched (buffer-name buffer))))
      (kill-buffer buffer))))

(ert-deftest workspaces-creates-missing-buffer-in-current-tab ()
  (let ((name "workspaces-new-buffer")
        switched)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _args) nil))
              ((symbol-function 'switch-to-buffer)
               (lambda (buffer &rest _args) (setq switched buffer))))
      (workspaces-switch-buffer-and-tab name)
      (should (equal switched name)))))

(ert-deftest workspaces-restores-existing-frame-buffer-predicate ()
  (let* ((frame (selected-frame))
         (original (frame-parameter frame 'buffer-predicate))
         (saved (lambda (_buffer) 'saved)))
    (unwind-protect
        (progn
          (set-frame-parameter frame 'buffer-predicate saved)
          (workspaces--set-buffer-predicate frame)
          (should-not (eq (frame-parameter frame 'buffer-predicate) saved))
          (workspaces--reset-buffer-predicate frame)
          (should (eq (frame-parameter frame 'buffer-predicate) saved)))
      (set-frame-parameter frame 'buffer-predicate original)
      (remhash frame workspaces--frame-buffer-predicates))))

(ert-deftest workspaces-open-switches-tab-before-project ()
  (let (events)
    (cl-letf (((symbol-function 'project-known-project-roots)
               (lambda () '("/tmp/workspaces-project/")))
              ((symbol-function 'workspaces--list)
               (lambda () '("/tmp/workspaces-project/")))
              ((symbol-function 'tab-bar-switch-to-tab)
               (lambda (_name) (push 'tab events)))
              ((symbol-function 'project-switch-project)
               (lambda (_directory) (push 'project events))))
      (workspaces-open "/tmp/workspaces-project/")
      (should (equal (nreverse events) '(tab project))))))

(ert-deftest workspaces-close-preserves-buffers-shared-with-other-tabs ()
  (let ((shared (get-buffer-create "workspaces-shared"))
        (unique (get-buffer-create "workspaces-unique"))
        closed)
    (unwind-protect
        (cl-letf (((symbol-function 'workspaces--tab-buffer-alist)
                   (lambda (&optional _frame)
                     `(("target" . (,shared ,unique))
                       ("other" . (,shared)))))
                  ((symbol-function 'tab-bar-close-tab-by-name)
                   (lambda (name) (setq closed name))))
          (workspaces-close "target")
          (should (buffer-live-p shared))
          (should-not (buffer-live-p unique))
          (should (equal closed "target")))
      (when (buffer-live-p shared)
        (kill-buffer shared))
      (when (buffer-live-p unique)
        (kill-buffer unique)))))

(ert-deftest workspaces-read-directory-refuses-missing-directory ()
  (cl-letf (((symbol-function 'read-directory-name)
             (lambda (&rest _args) "/does/not/exist/"))
            ((symbol-function 'file-directory-p) (lambda (_dir) nil))
            ((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
    (should-error (workspaces--read-directory-name "Directory: ")
                  :type 'user-error)))

(provide 'workspaces-tests)
;;; workspaces-tests.el ends here
