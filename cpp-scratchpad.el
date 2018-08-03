;;; cpp-scratchpad.el -- Scratchpad for C++  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Jakub Wojeciech

;; Author: Jakub Wojciech <jakub-w@riseup.net>
;; Maintainer:
;; Created:
;; Version:
;; Keywords: c tools
;; Package-Requires:
;; URL: https://github.com/jakub-w/cpp-scratchpad

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The `cpp-scratchpad' package provides simple and quick method to open
;; a scratchpad for testing code or prototyping in C++.

;;; Code:

(defvar cpp-scratchpad-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c")
      #'cpp-scratchpad-compile)
    (define-key map (kbd "C-c C-q")
      #'cpp-scratchpad-exit)
    map)
  "A key map for `cpp-scratchpad-mode'.")

;; TODO: Add usage documentation inside this minor mode doc.
(define-minor-mode
  cpp-scratchpad-mode
  "A minor mode used inside of cpp-scratchpad buffer. It's not designed to be
used anywhere else.

The following keys are available in `cpp-scratchpad-mode':

\\{cpp-scratchpad-mode-map}"
  nil
  " cpp-s"
  'cpp-scratchpad-mode-map)

(defvar cpp-scratchpad-template-path "~/.emacs.d/cpp-scratch-template"
  "Path to a scratchpad template directory.")

(defvar cpp-scratchpad-build-system-list
  '((:name "meson"
	   :builddir-gen-command "meson builddir"
	   :compile-command "cd builddir && ninja"
	   :compile-function cpp-scratchpad--meson-compile
	   :get-version-fun cpp-scratchpad--meson-get-version)
    (:name "cmake"
	   :builddir-gen-command "mkdir builddir && cd builddir && \
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=YES .."
	   :compile-command "make"
	   :compile-function cpp-scratchpad--cmake-compile
	   :get-version-fun cpp-scratchpad--cmake-get-version))
  "List containing build system information.

:name - Name of a build system.

:compile-command - Command to call after the build system has prepared all
                   necessary files.

:compile-function - Function used to compile a scratchpad.

:get-version-fun - Function used to determine which version of build system is
                   present on a system.")

(defun cpp-scratchpad--get-tool-prop (tool property)
  "Get PROPERTY of TOOL from `cpp-scratchpad-build-system-list'."
  (loop for tool-props in cpp-scratchpad-build-system-list
	when (string-equal tool (plist-get tool-props :name))
	return (plist-get tool-props property)))

(defun cpp-scratchpad--tool-exists-p (name)
  (eq 0 (call-process "/bin/sh" nil nil nil "-c" (concat "which " name))))

(defvar cpp-scratchpad-build-system
  (loop for tool-props in cpp-scratchpad-build-system-list
      for tool = (plist-get tool-props :name)
      if (cpp-scratchpad--tool-exists-p tool) return tool)
  "Build system chosen to build a scratchpad.

It defaults to whatever tool listed in `cpp-scratchpad-build-system-list' is
found on a system. Priority can be changed by modifying said list.")

(defvar cpp-scratchpad-current-path nil
  "Path to current temporary scratchpad directory.

This variable is set locally in scratchpad buffers.")

(defvar cpp-scratchpad-compilation-buffer nil
  "Buffer used as an output to compilation current scratchpad.")

(defun cpp-scratchpad-template-setup ()
  "Set up template directory.

Check if there is no template set up or if build systems or compilers have
changed and update the template if necessary."
  (interactive)
  ;; check if build system versions are ok
  (let ((result (cpp-scratchpad--check-tool-versions)))
    (if result (progn (message result)
		      (cpp-scratchpad--regenerate-build-files))
      (message "[cpp-scratchpad] Build system versions ok."))))

(defun cpp-scratchpad--check-tool-versions ()
  "Check if current's build system version changed."
  (catch 'regenerate
    (let ((tool-versions (concat cpp-scratchpad-template-path
				 "/tool-versions")))
      (if (not (file-exists-p tool-versions))
	  (throw 'regenerate "[cpp-scratchpad] tool-version file doesn't \
exist. Generating new files...")
	(with-temp-buffer
	  (insert-file-contents tool-versions)
	  (if (search-forward cpp-scratchpad-build-system nil t)
	      (progn
		(forward-char)
		(let ((version (buffer-substring-no-properties
				(point) (line-end-position))))
		  (message version)
		  (unless (string-equal
			   version
			   (funcall (cpp-scratchpad--get-tool-prop
				     cpp-scratchpad-build-system
				     :get-version-fun)))
		    (throw 'regenerate "[cpp-scratchpad] Build system \
version changed. Regenerating files..."))))
	    (throw 'regenerate "[cpp-scratchpad] Build system doesn't have a \
version. Regenerating files...")))))
    nil))

(defun cpp-scratchpad--regenerate-build-files ()
  "Regenerate files used to build a scratchpad and tool-versions file."
  (delete-directory (concat cpp-scratchpad-template-path "/builddir") t)
  ;; call the build system to create builddir
  (call-process "/bin/sh" nil nil nil "-c"
		(format "cd %s && %s"
			cpp-scratchpad-template-path
			(cpp-scratchpad--get-tool-prop
			 cpp-scratchpad-build-system :builddir-gen-command)))
  (make-symbolic-link "builddir/compile_commands.json"
		      (concat cpp-scratchpad-template-path
			      "/compile_commands.json")
		      t))

;; FIXME: This currently doesn't work.
(defun cpp-scratchpad-compile (&optional dont-run)
  "Compile using Meson or Cmake build systems.

Meson has priority but it can be redefined by rearranging
`cpp-scratchpad-build-system-list'."
  (interactive "P")
  ;; TODO: check if in cpp-scratchpad-mode, if not, leave
  (unless (buffer-live-p cpp-scratchpad-compilation-buffer)
    (setq-local cpp-scratchpad-compilation-buffer
		(get-buffer-create (concat
				    (string-trim-right (buffer-name) "*")
				    "-result*"))))
  (save-buffer)

  ;; don't run if dont-run set or if didn't compile for some reason
    (unless (or dont-run
	      (not (funcall (cpp-scratchpad--get-tool-prop
			 cpp-scratchpad-build-system
			 :compile-function))))
      (message "Run compiled scratchpad."))
    (pop-to-buffer cpp-scratchpad-compilation-buffer))

(defun cpp-scratchpad-compile-and-run ()
  "Compile current scratchpad and run it in the shell."
  (interactive)
  (save-buffer)

  (funcall (cpp-scratchpad--get-tool-prop cpp-scratchpad-build-system
					  :compile-function))
  (message "Run."))

(defun cpp-scratchpad--generic-get-version (tool)
  (let ((string (shell-command-to-string (concat tool " --version"))))
    (string-match "[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+" string)
    (match-string 0 string)))

;; NOTE: This evaluates slowly so it will hang emacs for a moment.
;;        For me it's about 0.21s compared to cmake's 0.012.
(defun cpp-scratchpad--meson-get-version ()
  (cpp-scratchpad--generic-get-version "meson"))

(defun cpp-scratchpad--meson-compile ()
  "Return t on success, otherwise nil."
  (assert cpp-scratchpad-compilation-buffer)
  (if (eq 0 (call-process
	     "/bin/sh" nil cpp-scratchpad-compilation-buffer nil
  	     "-c"
  	     (concat "cd " cpp-scratchpad-current-path " && "
  		     (cpp-scratchpad--get-tool-prop "meson"
  						    :compile-command))))
      t
    nil))

(defun cpp-scratchpad--cmake-get-version ()
  (cpp-scratchpad--generic-get-version "cmake"))

(defun cpp-scratchpad--cmake-compile ()
  "Return t on success, otherwise nil."
  (assert (buffer-live-p cpp-scratchpad-compilation-buffer))
  (if (eq 0 (call-process
	     "/bin/sh" nil cpp-scratchpad-compilation-buffer nil
  	     "-c"
  	     (concat "cd " cpp-scratchpad-current-path " && "
  		     (cpp-scratchpad--get-tool-prop "cmake"
  						    :compile-command))))
      t
    nil))

;; Maybe we should put the directory inside /tmp/emacs<uid>/
;; NOTE: Copying template directory may be unnecessary. It could be possible
;;       to just create symlinks to all needed files and directories.
;; TODO: number the scratchpads so that you can have several open at once
(defun cpp-scratchpad-new ()
  "Create a new, clean C++ scratchpad and pop to it."
  (interactive)
  (catch 'err
    (unless (file-exists-p cpp-scratchpad-template-path)
      (throw 'err "[cpp-scratchpad] Scratchpad template does not exist!"))
    (let ((current-path
	   (concat (temporary-file-directory)
		   (string-trim (shell-command-to-string
				 "mktemp -du emacs-cpp-scratch-XXX")))))
      (copy-directory cpp-scratchpad-template-path
		      current-path)
      (find-file-other-window (concat current-path
				      "/main.cpp"))
      (rename-buffer "*cpp-scratchpad*")
      (search-forward-regexp "main(.*)")
      (search-forward "{\n")
      (c-indent-line)
      (setq-local cpp-scratchpad-current-path current-path)
      (cpp-scratchpad-mode 1))))

(defun cpp-scratchpad-exit ()
  "Exit and delete current C++ scratchpad."
  (interactive)
  (delete-directory cpp-scratchpad-current-path t)
  (set-buffer-modified-p nil)
  (kill-current-buffer)
  (message "Exit."))

;;; cpp-scratchpad.el ends here
