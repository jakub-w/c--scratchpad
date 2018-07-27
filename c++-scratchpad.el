;; -*- lexical-binding: t -*-

;; TODO: This has to be a minor mode with a keymap. Also when exiting minor
;;       mode, it should delete the temp directory.
;;       Keymap: <C-c C-c> for compile and run, <C-c C-q> for exit scratch

(defvar c++-scratchpad-template-path "~/.emacs.d/cpp-scratch-template"
  "Path to a scratchpad template directory.")

(defvar c++-scratchpad-compilation-alist
  '(("meson" . c++-scratchpad--meson-compile)
    ("cmake" . c++-scratchpad--cmake-compile))
  "List of build systems with corresponding compilation functions.")

(defvar c++-scratchpad-current-path nil
  "Path to current temporary scratchpad directory.

This variable is set locally in scratchpad buffers.")

(setq c++-scratchpad-scratch-path
      (concat (temporary-file-directory)
	      (string-trim (shell-command-to-string
			    "mktemp -du emacs-cpp-scratch-XXX"))))

(defun c++-scratchpad--tool-exists-p (name)
  (with-temp-buffer
    (eq 0 (call-process "/bin/sh" nil nil nil "-c" (concat "which " name)))))

(defun c++-scratchpad-compile ()
  "Compile using Meson or Cmake build systems.

Meson has priority but it can be redefined by rearranging
`c++-scratchpad-compilation-alist'."
  (interactive)
  ;; TODO: check if in c++-scratchpad-mode, if not, leave
  ;; (save-buffer)
  (funcall
   (catch 'found   ; returns nil if not found
     (loop for (tool . function) in c++-scratchpad-compilation-alist
	   do (when (c++-scratchpad--tool-exists-p tool)
      		(throw 'found function))))))

(defun c++-scratchpad--meson-compile ()
  (message "meson!!!"))

(defun c++-scratchpad--cmake-compile ()
  (message "cmake!!!"))

;; Maybe we sould put the directory inside /tmp/emacs<uid>/
(defun c++-scratchpad-new ()
  "Create a new, clean C++ scratchpad and pop to it."
  (interactive)
  (catch 'err
    (unless (file-exists-p c++-scratchpad-template-path)
      (throw 'err "[c++-scratchpad] Scratchpad template does not exist!"))
    (let ((current-path
	   (concat (temporary-file-directory)
		   (string-trim (shell-command-to-string
				 "mktemp -du emacs-cpp-scratch-XXX")))))
      (copy-directory c++-scratchpad-template-path
		      current-path)
      (find-file-other-window (concat current-path
				      "/main.cpp"))
      (search-forward-regexp "main(.*)")
      (search-forward "{\n")
      (c-indent-line)
      (let ((before-save-hook nil))
	(save-buffer))
      (setq-local c++-scratchpad-current-path current-path))))

;; (shell-process-cd (concat c++-scratchpad-current-path "builddir"))
;; (with-temp-buffer
;;   (unless (eq 0 (shell-command "meson builddir" (current-buffer)))
;;     (throw 'err "[c++-scratchpad] Meson isn't installed?")))
;; (shell-command "ninja")
