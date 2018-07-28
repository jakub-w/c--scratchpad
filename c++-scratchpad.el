;; -*- lexical-binding: t -*-

;; TODO: This has to be a minor mode with a keymap. Also when exiting minor
;;       mode, it should delete the temp directory.
;;       Keymap: <C-c C-c> for compile and run, <C-c C-q> for exit scratch

(defvar c++-scratchpad-template-path "~/.emacs.d/cpp-scratch-template"
  "Path to a scratchpad template directory.")

(defvar c++-scratchpad-build-system-list
  '((:name "meson"
	   :compile-command "ninja"
	   :compile-function c++-scratchpad--meson-compile
	   :get-version-fun c++-scratchpad--meson-get-version)
    (:name "cmake"
	   :compile-command "make"
	   :compile-function c++-scratchpad--cmake-compile
	   :get-version-fun c++-scratchpad--cmake-get-version))
  "List containing build system information.

:name - Name of a build system.

:compile-command - Command to call after the build system has prepared all
                   necessary files.

:compile-function - Function used to compile a scratchpad.

:get-version-fun - Function used to determine which version of build system is
                   present on a system.")

(defun c++-scratchpad--get-tool-prop (tool property)
  "Get PROPERTY of TOOL from `c++-scratchpad-build-system-list'."
  (loop for tool-props in c++-scratchpad-build-system-list
	do (message "tool: %s" (plist-get tool-props :name))
	when (string-equal tool (plist-get tool-props :name))
	return (plist-get tool-props property)))

(defun c++-scratchpad--tool-exists-p (name)
  (eq 0 (call-process "/bin/sh" nil nil nil "-c" (concat "which " name))))

(defvar c++-scratchpad-build-system
  (loop for tool-props in c++-scratchpad-build-system-list
      for tool = (plist-get tool-props :name)
      if (c++-scratchpad--tool-exists-p tool) return tool)
  "Build system chosen to build a scratchpad.

It defaults to whatever tool listed in `c++-scratchpad-build-system-list' is
found on a system. Priority can be changed by modifying said list.")

(defvar c++-scratchpad-current-path nil
  "Path to current temporary scratchpad directory.

This variable is set locally in scratchpad buffers.")

(defun c++-scratchpad-template-setup ()
  "Set up template directory.

Check if there is no template set up or if build systems or compilers have
changed and update the template if necessary."
  (interactive)
  ;; check if build system versions are ok
  (let ((result (c++-scratchpad--check-tool-versions)))
    (if result (progn (message result)
		      (c++-scratchpad--regenerate-build-files))
      (message "[c++-scratchpad] Build system versions ok."))))

(defun c++-scratchpad--check-tool-versions ()
  "Check if current's build system version changed."
  (catch 'regenerate
    (let ((tool-versions (concat c++-scratchpad-template-path
				 "/tool-versions")))
      (if (not (file-exists-p tool-versions))
	  (throw 'regenerate "[c++-scratchpad] tool-version file doesn't \
exist. Generating new files...")
	(with-temp-buffer
	  (insert-file-contents tool-versions)
	  (if (search-forward c++-scratchpad-build-system nil t)
	      (progn
		(forward-char)
		(let ((version (buffer-substring-no-properties
				(point) (line-end-position))))
		  (message version)
		  (unless (string-equal
			   version
			   (funcall (c++-scratchpad--get-tool-prop
				     c++-scratchpad-build-system
				     :get-version-fun)))
		    (throw 'regenerate "[c++-scratchpad] Build system \
version changed. Regenerating files..."))))
	    (throw 'regenerate "[c++-scratchpad] Build system doesn't have a \
version. Regenerating files...")))))
    nil))

(defun c++-scratchpad--regenerate-build-files ()
  "Regenerate files used to build a scratchpad and tool-versions file."
  (message "Regenerate build files."))

(defun c++-scratchpad-compile ()
  "Compile using Meson or Cmake build systems.

Meson has priority but it can be redefined by rearranging
`c++-scratchpad-build-system-list'."
  (interactive)
  ;; TODO: check if in c++-scratchpad-mode, if not, leave
  ;; (save-buffer)
  (funcall
   (catch 'found   ; returns nil if not found
     (loop for (tool . function) in c++-scratchpad-compilation-alist
	   do (when (c++-scratchpad--tool-exists-p tool)
      		(throw 'found function))))))

(defun c++-scratchpad--meson-get-version ()
  (message "0.47.1"))

(defun c++-scratchpad--meson-compile ()
  (message "meson!!!"))

(defun c++-scratchpad--cmake-get-version ()
  (message "3.11.4"))

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
      (setq-local c++-scratchpad-current-path current-path))))
