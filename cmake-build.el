;;; cmake-build.el --- CMake build profiles and configurations for projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  Ryan Pavlik

;; Author: Ryan Pavlik <rpavlik@gmail.com>
;; URL: https://github.com/rpav/cmake-build.el
;; Package-Requires: ((emacs "29.1"))
;; Version: 1.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; (Fork by Paul Nelson.)

;;; Code:


(require 'cl-lib)
(require 'tramp)
(require 'project)
(require 'compile)

(defgroup cmake-build ()
  "Use CMake to build projects and run targets based on configurations."
  :group 'tools)

(defcustom cmake-build-local-options-file
  (expand-file-name "cmake-build-options.el" user-emacs-directory)
  "Path to file storing local cmake-build settings.
These include options passed to cmake, and the current config."
  :type 'file
  :group 'cmake-build)

(defcustom cmake-build-run-window-autoswitch t
  "Automatically switch between Build and Run output buffers in the visible window."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-before-run t
  "Build automatically before running app, when using `cmake-build-run`."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-display-type 'split
  "How to display cmake-build output.
`split' will split the window (using cmake-build window
splitting options), `frame' will create a new frame.  In all
cases, the buffers will be reused if they are visible, regardless
of current display type."
  :type 'symbol
  :group 'cmake-build
  :options '(split frame))

(defcustom cmake-build-raise-frame t
  "Whether to raise the frame of the build window on build.
This only applies if `cmake-build-display-type` is frame."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-override-compile-keymap t
  "Whether to use `cmake-build-run-keymap' for the compile window as well.
This more or less provides specific/consistent behavior for
quitting the frame or window."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-run-quit-frame-type 'lower
  "How to handle the run frame when quitting."
  :type 'symbol
  :group 'cmake-build
  :options '(lower delete))

(defcustom cmake-build-quit-kills-process nil
  "Non-nil means `cmake-build-quit-window' also kills the process."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-run-window-size 20
  "Size of window to split."
  :type 'integer
  :group 'cmake-build)

(defcustom cmake-build-split-threshold 40.0
  "Threshold (percentage) at which to *not* split the current window.
Beyond this threshold, we instead use the other window.  That is,
if `cmake-build-run-window-size` is greater than this percentage
of the current window, it will not be split."
  :type 'float
  :group 'cmake-build)

(defcustom cmake-build-never-split nil
  "Never split the window, instead always use the other window."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-switch-to-build nil
  "Whether to switch to build window with `cmake-build-current'."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-dir-name-function 'cmake-build-default-build-dir-function
  "Specify a function to customize the build directory name.
By default, the name is in the form `build.<profile>`."
  :type 'function
  :group 'cmake-build)

(defcustom cmake-build-export-compile-commands nil
  "Non-nil means to generate compile_commands.sjon and symlink."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-completing-read-function 'completing-read
  "Specify completing function for cmake-build.
This applies to `cmake-build-set-config' and
`cmake-build-set-cmake-profile.'  We assume that the function has
syntax compatible with `ido-completing-read' or
`ivy-completing-read.'  See the indicated cmakeb-build-set-*
functions for details."
  :type 'function
  :group 'cmake-build)

(defcustom cmake-build-project-root-function 'cmake-build-default-project-root-function
  "Return the project root."
  :type 'function
  :group 'cmake-build)

(defcustom cmake-build-project-name-function 'cmake-build-default-project-name-function
  "Return the project name."
  :type 'function
  :group 'cmake-build)

(defcustom cmake-build-run-function 'async-shell-command
  "Specify a function to use in `cmake-build-run'.
The function should accept as its first argument the shell
command to run (from `default-directory') and as its second
argument the name of the buffer to use."
  :type 'function
  :group 'cmake-build)

(defun cmake-build-default-project-root-function ()
  "Return the project root."
  (when-let ((project (project-current)))
    (project-root project)))

(defun cmake-build-default-project-name-function ()
  "Return the project name."
  (when-let ((project (project-current)))
    (project-name project)))

;;; These are very temporary and likely very host-specific variables,
;;; not something we want to constantly modify in custom.el
(defvar cmake-build-profile 'clang-release
  "Build profile name to use for `cmake-build-current`.")

(defvar cmake-build-options ""
  "Additional build options passed to cmake.
For example, \"-j 7\" for parallel builds.")

(defvar cmake-build-run-config nil
  "Alist mapping projects to run-config names.
Run configurations are a path, command, and arguments for a
particular run.")

(defvar cmake-build-project-root nil
  "Optional emacs-wide root of current project.
If nil, then use `project' to determine root buffer-locally.")

(defvar cmake-build-build-roots nil
  "Alist of build roots per-project, for out-of-source building.")

(defvar cmake-build-run-keymap (make-sparse-keymap))

(defun cmake-build-quit-window ()
  "Quit a build window.
Activated by the key `q'.  Possibly kills the process and lowers
or deletes the frame?  TODO: clarify"
  (interactive)
  (when (and cmake-build-quit-kills-process
             (get-buffer-process (current-buffer)))
    (cmake-build-kill-buffer-process))
  (if (= 1 (length (window-list)))
      (cl-case cmake-build-run-quit-frame-type
        (lower (lower-frame))
        (delete (delete-frame)))
    (delete-window)))

(defun cmake-build-kill-buffer-process (&optional buffer)
  "Kill process associated with BUFFER."
  (interactive)
  (let* ((buffer (get-buffer (or buffer (current-buffer))))
         (p (get-buffer-process buffer)))
    (when p
      (with-current-buffer buffer
        (cl-case major-mode
          (shell-mode (kill-process p))
          (compilation-mode (kill-compilation)))))))

(defun cmake-build-kill-processes ()
  "Kill processes for build and run buffers."
  (interactive)
  (cmake-build-kill-buffer-process (cmake-build--build-buffer-name))
  (cmake-build-kill-buffer-process (cmake-build--run-buffer-name)))

(let ((map cmake-build-run-keymap))
  (define-key map (kbd "q") 'cmake-build-quit-window)
  (define-key map (kbd "C-c C-c") 'cmake-build-kill-buffer-process))

(cl-defmacro cmake-build--with-file ((filename &key readp writep) &body body)
  "Execute BODY in a temporary buffer using FILENAME.
If READP is non-nil, read the file into the buffer before BODY.
If WRITEP is non-nil, write the buffer to the file after BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (prog1
         ,(if readp
              `(when (file-exists-p ,filename)
                 (insert-file-contents ,filename)
                 ,@body)
            `(progn ,@body))
       (when ,writep
         (write-region 1 (point-max) ,filename)))))

(cl-defmacro cmake-build--with-options-file ((&key readp writep) &body body)
  "Execute BODY in a temporary buffer using the local options file.
If READP is non-nil, read the file into the buffer before BODY.
If WRITEP is non-nil, write the buffer to the file after BODY."
  (declare (indent 1))
  `(cmake-build--with-file (cmake-build-local-options-file :readp ,readp :writep ,writep) ,@body))

(defun cmake-build--project-root ()
  "Return current project root path.
This is the variable `cmake-build-project-root' if set, or the
result of `cmake-build-project-root-function' otherwise."
  (or cmake-build-project-root
      (funcall cmake-build-project-root-function)))

(defun cmake-build--maybe-remote-project-root ()
  "Return current project root path, suitable for remote invocations too."
  (let* ((project-root-raw (cmake-build--project-root))
         (project-root
          (file-name-as-directory
           (if (tramp-tramp-file-p project-root-raw)
               (let ((parsed-root (tramp-dissect-file-name project-root-raw)))
                 (tramp-file-name-localname parsed-root))
             (expand-file-name project-root-raw)))))
    (file-name-concat
     project-root (or (cmake-build--source-root) ""))))

(cl-defmacro cmake-build--save-project-root (nil &body body)
  "Execute BODY with let-bound `cmake-build-project-root'."
  (declare (indent 1))
  `(let ((cmake-build-project-root (cmake-build--project-root)))
     ,@body))

(defun cmake-build--read-options ()
  "Read cmake-build-* options from local options file."
  (cmake-build--with-options-file (:readp t)
    (let* ((form (read (buffer-string)))
           (build-profile (cadr (assoc :build-profile form)))
           (build-options (cadr (assoc :build-options form)))
           (build-run-config (cadr (assoc :build-run-config form)))
           (build-project-root (cadr (assoc :build-project-root form)))
           (build-roots (cadr (assoc :build-roots form))))
      (setq cmake-build-profile (or build-profile cmake-build-profile))
      (setq cmake-build-options (or build-options cmake-build-options))
      (setq cmake-build-run-config (or build-run-config cmake-build-run-config))
      (setq cmake-build-project-root (or build-project-root cmake-build-project-root))
      (setq cmake-build-build-roots (or build-roots cmake-build-build-roots)))))

(defun cmake-build--project-data-path ()
  "Return path to project data file."
  (file-name-concat
   (file-name-as-directory (cmake-build--project-root))
   ".cmake-build.el"))

(defun cmake-build-open-project-data ()
  "Open project data file."
  (interactive)
  (find-file (cmake-build--project-data-path)))

(defun cmake-build--read-project-data ()
  "Read project data from .cmake-build.el file."
  (let ((project-data-path (cmake-build--project-data-path)))
    (cmake-build--with-file (project-data-path :readp t)
      (read (buffer-string)))))

(defun cmake-build--write-options ()
  "Write cmake-build-* options to local options file."
  (cmake-build--with-options-file (:writep t)
    (print `((:build-profile ,cmake-build-profile)
             (:build-options ,cmake-build-options)
             (:build-run-config ,cmake-build-run-config)
             (:build-project-root ,cmake-build-project-root)
             (:build-roots ,cmake-build-build-roots))
           (current-buffer))))

(defun cmake-build-get-run-config-name ()
  "Return name of current run-config."
  (when-let ((project-root (cmake-build--project-root)))
    (cdr (assoc (intern project-root) cmake-build-run-config))))

(defun cmake-build--set-run-config (config)
  "Set current run-config to CONFIG."
  (when-let ((project-root (cmake-build--project-root)))
    (setf (alist-get (intern project-root) cmake-build-run-config)
          config)))

(defun cmake-build--set-build-root (path)
  "Set current build-root to PATH."
  (when-let ((build-root (cmake-build--build-root)))
    (setf (alist-get (intern build-root) cmake-build-build-roots)
          path)))

(defun cmake-build--validity ()
  "Return t if current project can be built or run.
Otherwise, return key explaining why not."
  (cond
   ((not (cmake-build--project-root)) :data-missing)
   ((not (file-directory-p (cmake-build--get-build-dir))) :build-dir-missing)
   ((null (cmake-build--get-project-data)) :data-missing)
   (t t)))

(defun cmake-build--validate (&optional tag)
  "Return t if current project can be built or run.
Otherwise, return nil and print a message explaining why not.
TAG indicates the action being validated, e.g. `compile' or `run'."
  (not
   (cl-case (cmake-build--validity)
     (:build-dir-missing
      (message "cmake-build %s: No build dir (%s)\nDo you need to initialize CMake?"
               (or tag "compile")
               (cmake-build--get-build-dir)))
     (:data-missing
      (message "cmake-build %s: Not a valid project ; no .cmake-build.el data found (project root is %s)"
	              (or tag "compile")
	              (cmake-build--project-root)))
     (t nil))))

(defun cmake-build-project-name ()
  "Return the project name.
Specified via the defcustom `cmake-build-project-name-function'."
  (let ((default-directory (cmake-build--project-root)))
    (funcall cmake-build-project-name-function)))

(defun cmake-build--build-buffer-name (&optional _name)
  "Return the name of the build buffer."
  (concat "*Build " (cmake-build-project-name)
          "/" (symbol-name cmake-build-profile)
          ": " (symbol-name (cmake-build-get-run-config-name))
          "*"))

(defun cmake-build--run-buffer-name ()
  "Return the name of the run buffer."
  (concat "*Run " (cmake-build-project-name)
          "/" (symbol-name cmake-build-profile)
          ": " (symbol-name (cmake-build-get-run-config-name))
          "*"))

(defun cmake-build--get-project-data ()
  "Return the project data alist, coming from .cmake-build.el."
  (cmake-build--read-project-data))

(defun cmake-build--get-cmake-options ()
  "Return cmake options for current project."
  (cadr (assoc 'cmake-build-cmake-options (cmake-build--get-project-data))))

(defun cmake-build--get-cmake-profiles ()
  "Return cmake profiles for current project."
  (when (cmake-build--project-root)
    (cdr (assoc 'cmake-build-cmake-profiles
                (cmake-build--get-project-data)))))

(defun cmake-build--get-profile (&optional profile)
  "Return the cmake profile for PROFILE.
Uses either `cmake-build-profile' or the project data."
  (cdr (assoc (or profile cmake-build-profile)
              (cmake-build--get-cmake-profiles))))

(defun cmake-build--get-configs ()
  "Return configs for current project."
  (when (cmake-build--project-root)
    (cdr (assoc 'cmake-build-run-configs
                (cmake-build--get-project-data)))))

(defun cmake-build--get-config (&optional config)
  "Return a specific CONFIG for current project."
  (cdr (assoc (or config (cmake-build-get-run-config-name))
              (cmake-build--get-configs))))

(defun cmake-build--get-build-config (&optional config)
  "Return build target associated to CONFIG."
  (let ((config (cmake-build--get-config config)))
    (cdr (assoc :build config))))

(defun cmake-build--get-run-config (&optional config)
  "Return run config associated to CONFIG."
  (let ((config (cmake-build--get-config config)))
    (cdr (assoc :run config))))

(defun cmake-build--get-run-config-env (&optional config)
  "Return run config environment associated to CONFIG."
  (let ((config (cmake-build--get-config config)))
    (cdr (assoc :env config))))

(defun cmake-build--get-other-targets ()
  "Return other targets for current project."
  (cdr (assoc 'cmake-build-other-targets (cmake-build--get-project-data))))

(defun cmake-build--build-root ()
  "Return build root for the current project.
If the project has a build root specified by
`cmake-build-build-roots', then return that.  Otherwise,
use the project root."
  (or (cdr (assoc (intern (cmake-build--project-root)) cmake-build-build-roots))
      (cmake-build--project-root)))

(defun cmake-build--source-root ()
  "Return source root for the current project."
  (cadr (assoc 'cmake-build-source-root (cmake-build--get-project-data))))

(defun cmake-build-default-build-dir-function (_project-root profile)
  "Return the default build directory, `build.PROFILE'."
  (concat "build." profile))

(defun cmake-build--get-build-dir-relative (&optional subdir)
  "Return the build directory relative to the project root.
Optionally, append SUBDIR."
  (concat (funcall cmake-build-dir-name-function
                   (cmake-build--project-root)
                   (symbol-name cmake-build-profile))
          "/" (or subdir "")))

(defun cmake-build--get-build-dir (&optional _subdir)
  "Return the build directory for the current project."
  (concat (cmake-build--build-root)
	         (cmake-build--get-build-dir-relative)))

(defun cmake-build--check-build-dir ()
  "Check if the build directory exists."
  (let ((path (cmake-build--get-build-dir)))
    (if (file-directory-p path)
        t
      (message "Build directory doesn't exist: %s\nDo you need to initialize CMake?" path)
      nil)))

(defun cmake-build--get-run-command (config)
  "Get the run command associated to CONFIG.
Return the concatenation of the second and third elements of
config, except when the first element is nil, in which case
prepend the build directory."
  (if (car config)
      (concat (cadr config) " " (caddr config))
    (concat
     (cmake-build--get-build-dir-relative)
     (concat (cadr config) " " (caddr config)))))

(defun cmake-build--switch-to-buffer (buffer buffer-window other-window)
  "Conditionally switch to BUFFER in OTHER-WINDOW.
Do this only if BUFFER-WINDOW is nil and
`cmake-build-run-window-autoswitch' is non-nil."
  (if buffer-window t
    (when (and cmake-build-run-window-autoswitch
               other-window)
      (set-window-dedicated-p other-window nil)
      (set-window-buffer other-window buffer)
      (set-window-dedicated-p other-window t)
      t)))

(defun cmake-build--split-to-buffer (name other-name)
  "Display buffers NAME and OTHER-NAME, adjusting window layout."
  (let* ((window-point-insertion-type t)
         ;; Make sure we have a buffer created regardless
         (buffer (get-buffer-create name))
         (current-buffer-window (get-buffer-window))
         (new-buffer-window (get-buffer-window name))
         (other-buffer-window (and other-name (get-buffer-window other-name t)))
         (split-is-current (or (eql current-buffer-window new-buffer-window)
                               (eql current-buffer-window other-buffer-window))))
    (when (or (and other-buffer-window
                   cmake-build-run-window-autoswitch)
              (and (not cmake-build-never-split)
                   (not split-is-current)
                   (<= cmake-build-run-window-size
                       (* (/ cmake-build-split-threshold 100.0)
                          (window-total-height current-buffer-window)))))
      (unless (cmake-build--switch-to-buffer buffer (get-buffer-window buffer t) other-buffer-window)
        (when (and (not other-buffer-window)
                   (not (get-buffer-window name t)))
          (let ((window (split-window-below (- cmake-build-run-window-size))))
            (set-window-buffer window buffer)
            (set-window-dedicated-p window t))))
      t)))

(defun cmake-build--popup-buffer (name other-name)
  "Display buffers NAME and OTHER-NAME, possibly using pop-up frame."
  (let* ((buffer (get-buffer-create name))
         (_current-buffer-window (get-buffer-window buffer t))
         (other-buffer-window (and other-name (get-buffer-window other-name t)))
         (_buffer-config-name (cmake-build-get-run-config-name)))
    (unless (cmake-build--switch-to-buffer buffer (get-buffer-window buffer t) other-buffer-window)
      (display-buffer-pop-up-frame buffer default-frame-alist))
    (when cmake-build-raise-frame
      (raise-frame (window-frame (get-buffer-window buffer t))))
    t))

(defun cmake-build--display-buffer (name &optional other-name)
  "Display buffers NAME and OTHER-NAME.
The display method is determined by `cmake-build-display-type'."
  (cl-case cmake-build-display-type
    (split (cmake-build--split-to-buffer name other-name))
    (frame (cmake-build--popup-buffer name other-name))))

(cl-defun cmake-build--compile (buffer-name command &key sentinel other-buffer-name)
  "Compile COMMAND in BUFFER-NAME, with SENTINEL and OTHER-BUFFER-NAME."
  (let* ((did-split (cmake-build--display-buffer buffer-name other-buffer-name))
         (display-buffer-alist
          ;; Suppress the window only if we actually split
          (if did-split
              (cons (list buffer-name #'display-buffer-no-window)
                    display-buffer-alist)
            display-buffer-alist))
         (actual-directory default-directory))
    (if (get-buffer-process buffer-name)
        (message "Already building %s/%s"
                 (funcall cmake-build-project-name-function)
                 (symbol-name cmake-build-profile))
      (with-current-buffer buffer-name
        (setq-local compilation-directory actual-directory)
        (setq-local default-directory actual-directory))
      ;; compile saves buffers; rely on this now
      (let (compilation-buffer)
        (cl-flet ((run-compile ()
                    (setq compilation-buffer (compile (concat "time " command)))))
          (let ((w (get-buffer-window buffer-name t)))
            (if (and w (not (eql (get-buffer-window) w)))
                (if cmake-build-switch-to-build
                    (progn
                      (switch-to-buffer-other-window buffer-name)
                      (run-compile))
                  (with-selected-window w
                    (run-compile)))
              (run-compile))))
        (when sentinel
          (let ((process (get-buffer-process compilation-buffer)))
            (when (process-live-p process)
              (set-process-sentinel process
                                    (lambda (p e)
                                      (funcall sentinel p e)
                                      (compilation-sentinel p e))))))
        (with-current-buffer buffer-name
          (dolist (w (get-buffer-window-list buffer-name nil t))
            (set-window-point w (point-max)))
          (visual-line-mode 1)
          (when cmake-build-override-compile-keymap
            (use-local-map cmake-build-run-keymap)))))))

(defun cmake-build--invoke-build-current (&optional sentinel)
  "Invoke build for current project.
If SENTINEL is non-nil, use it as the process sentinel."
  (when (cmake-build--validate)
    (cmake-build--save-project-root ()
      (let* ((default-directory (cmake-build--get-build-dir))
             (config (cmake-build--get-build-config))
             (command (concat "cmake --build . " cmake-build-options " --target " (car config)))
             (buffer-name (cmake-build--build-buffer-name))
             (other-buffer-name (cmake-build--run-buffer-name)))
        (cmake-build--compile buffer-name command
                              :sentinel sentinel :other-buffer-name other-buffer-name)))))

(defun cmake-build-current ()
  "Build the current project."
  (interactive)
  (cmake-build--invoke-build-current))

(defun cmake-build--get-run-directory (config)
  "Get the run directory associated to CONFIG."
  (if (car config)
      (cmake-build--get-build-dir (car config))
    (cmake-build--maybe-remote-project-root)))

(defun cmake-build--invoke-run (config)
  "Invoke run for current project using CONFIG."
  (cmake-build--save-project-root ()
    (let* ((cmake-build-run-config config)
           (config (cmake-build--get-run-config))
           (command (cmake-build--get-run-command config))
           (default-directory (cmake-build--get-run-directory config))
           (process-environment (append
                                 (list (concat "PROJECT_ROOT="
                                               (cmake-build--maybe-remote-project-root)))
                                 (cmake-build--get-run-config-env)
                                 process-environment))
           (buffer-name (cmake-build--run-buffer-name))
           (other-buffer-name (cmake-build--build-buffer-name))
           (display-buffer-alist
            (if (cmake-build--display-buffer buffer-name other-buffer-name)
                (cons (list buffer-name #'display-buffer-no-window)
                      display-buffer-alist)
              display-buffer-alist)))
      (if (get-buffer-process buffer-name)
          (message "Already running %s/%s"
                   (funcall cmake-build-project-name-function)
                   (symbol-name cmake-build-profile))
	       (message "Command: %s\n" command)
	       (message "Directory: %s\n" default-directory)
	       (message "Project root: %s\n" (cmake-build--maybe-remote-project-root))
        (with-current-buffer buffer-name
	         (insert command))
        (funcall cmake-build-run-function command buffer-name)
        (with-current-buffer buffer-name
          (use-local-map cmake-build-run-keymap))))))

(defun cmake-build-run ()
  "Run the current project."
  (interactive)
  ;; If we switch windows, remember what project we're building
  (when (cmake-build--validate "run")
    (let* ((this-root (cmake-build--project-root))
           (this-run-config cmake-build-run-config)
           (cmake-build-project-root this-root))
      (if cmake-build-before-run
          (cmake-build--invoke-build-current
           (lambda (_process event)
             (let* ((this-root this-root)
                    (cmake-build-project-root this-root))
               (when (cl-equalp "finished\n" event)
                 (cmake-build--invoke-run this-run-config)))))
        (cmake-build--invoke-run this-run-config)))))

(defun cmake-build-debug ()
  "Run the current project in gdb."
  (interactive)
  (let* ((config (cmake-build--get-run-config))
         (command (cmake-build--get-run-command config))
         (default-directory (cmake-build--get-run-directory config)) ; check whether config should be (car config)
         (process-environment (append (cmake-build--get-run-config-env) process-environment)))
    (gdb (concat "gdb -i=mi --args " command))))

(defun cmake-build-set-options (option-string)
  "Set CMake build options to OPTION-STRING."
  (interactive
   (list
    (read-string "CMake build options: " cmake-build-options)))
  (setq cmake-build-options option-string))

(defun cmake-build-set-config (config-name)
  "Set the current config to CONFIG-NAME."
  (interactive
   (list
    (let* ((configs (cmake-build--get-configs))
           (choices (mapcar (lambda (x)
                              (symbol-name (car x)))
                            configs)))
      (intern (funcall cmake-build-completing-read-function "CMake Config: " choices nil t nil nil (symbol-name (cmake-build-get-run-config-name)))))))
  (let* ((config (cmake-build--get-config config-name)))
    (if config
        (progn
          (cmake-build--set-run-config config-name)
          (let ((build (cmake-build--get-build-config))
                (run (cmake-build--get-run-config)))
            (message "Build: %s   Run: %s"
                     (car build)
		                   (if (car run)
			                      (substring (cmake-build--get-run-command run)
                                    2)
		                     (cmake-build--get-run-command run)))))
      (message "cmake-build: %s isn't a config." config))))

(defun cmake-build-set-buffer-local-config ()
  "Set current config, buffer-locally."
  (interactive)
  (setq-local cmake-build-run-config
              (list (copy-tree
                     (rassoc (cmake-build-get-run-config-name) cmake-build-run-config))))
  (call-interactively #'cmake-build-set-config))

(defun cmake-build-set-project-root (path)
  "Set project root to PATH.
If PATH is blank, unset the project root."
  (interactive
   (list
    (let* ((default-directory (cmake-build--project-root)))
      (read-directory-name "CMake Build project root (blank to unset): "))))
  (message "Path: %s" path)
  (setq cmake-build-project-root (if (string= path "") nil path)))

(defun cmake-build-set-project-build-root (path)
  "Set project build root to PATH."
  (interactive
   (list
    (let* ((default-directory (cmake-build--build-root)))
      (read-directory-name "CMake Build build root (blank to unset): "))))
  (if (cl-equalp "" path)
      (progn
        (message "Build root reset to default")
        (cmake-build--set-build-root nil))
    (let* ((path (file-name-as-directory path))
           (existp (file-directory-p path)))
      (if existp
          (progn
            (message "Build root: %s" path)
            (cmake-build--set-build-root path))
        (message "Error: Path does not exist: %s" path)))))

(defun cmake-build-set-cmake-profile (profile-name)
  "Set the current cmake profile to PROFILE-NAME."
  (interactive
   (list
    (let* ((profiles (cmake-build--get-cmake-profiles))
           (choices (mapcar (lambda (x)
                              (symbol-name (car x)))
                            profiles)))
      (intern (funcall cmake-build-completing-read-function "CMake Profile " choices nil t nil nil (symbol-name cmake-build-profile))))))
  (let* ((profile (cmake-build--get-profile profile-name)))
    (if profile
        (progn
          (message "Config: %s  cmake %s"
                   profile-name (car profile))
          (setq cmake-build-profile profile-name))
      (message "cmake-build: %s isn't a profile." profile))))

(defun cmake-build-run-cmake ()
  "Run cmake."
  (interactive)
  (let* ((default-directory (cmake-build--get-build-dir))
         (buffer-name (cmake-build--build-buffer-name))
         (other-buffer-name (cmake-build--run-buffer-name)))
    (cmake-build--compile buffer-name "cmake ."
                          :other-buffer-name other-buffer-name)))

(defun cmake-build--create-compile-commands-symlink ()
  "Create symlink to <build>/compile_commands.json from project root."
  (let ((filename (expand-file-name "compile_commands.json" (cmake-build--project-root))))
    (when (or (file-exists-p filename)
              (file-symlink-p filename))
      (delete-file filename)))
  (make-symbolic-link (file-relative-name "compile_commands.json" (cmake-build--project-root))
                      (cmake-build--project-root)))

(defun cmake-build-clear-cache-and-configure ()
  "Clear the cache and configure."
  (interactive)
  (let ((build-dir (cmake-build--get-build-dir)))
    (unless (file-exists-p build-dir)
      (make-directory build-dir t))
    (cmake-build--save-project-root ()
      (let* ((default-directory build-dir)
             (buffer-name (cmake-build--build-buffer-name))
             (other-buffer-name (cmake-build--run-buffer-name))
             (command (concat "cmake " (cmake-build--get-cmake-options)
                              (when cmake-build-export-compile-commands " -DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
                              " " (car (cmake-build--get-profile))
                              " " (cmake-build--maybe-remote-project-root))))
        (when (file-exists-p "CMakeCache.txt")
          (delete-file "CMakeCache.txt"))
        (cmake-build--compile buffer-name command
                              :other-buffer-name other-buffer-name)
        (when cmake-build-export-compile-commands
          (cmake-build--create-compile-commands-symlink))))))

(defun cmake-build-clean ()
  "Clean the current project."
  (interactive)
  (cmake-build--save-project-root ()
    (let* ((default-directory (cmake-build--get-build-dir))
           (buffer-name (cmake-build--build-buffer-name))
           (other-buffer-name (cmake-build--run-buffer-name)))
      (cmake-build--compile buffer-name "cmake --build . --target clean"
                            :other-buffer-name other-buffer-name))))

(defun cmake-build--get-available-targets ()
  "Return list of available targets for current project."
  (cmake-build--save-project-root ()
    (let* ((default-directory (cmake-build--get-build-dir))
           ;; cdr to skip the first line which is a cmake comment
           (raw-targets-list
            (cdr (split-string (shell-command-to-string "cmake --build . --target help")
                               "\n"))))
      ;; the actual targets are after "... " in each string
      (mapcar 'cadr (mapcar #'(lambda (x)
                                (split-string x " "))
                            raw-targets-list)))))

(defun cmake-build-other-target (target-name)
  "Build a target (TARGET-NAME) other than the default."
  (interactive
   (list
    (completing-read "Target: " (cmake-build--get-available-targets))))
  (cmake-build--save-project-root ()
    (let* ((default-directory (cmake-build--get-build-dir))
           (buffer-name (cmake-build--build-buffer-name))
           (other-buffer-name (cmake-build--run-buffer-name)))
      (cmake-build--compile buffer-name
                            (concat "cmake --build . " cmake-build-options " --target " target-name)
                            :other-buffer-name other-buffer-name))))

(defun cmake-build-delete-current-windows ()
  "Delete compile/run windows for current run configuration."
  (interactive)
  (cl-flet ((f (name)
              (when-let ((b (get-buffer name)))
                (mapcar #'delete-window (get-buffer-window-list b nil t)))))
    (f (cmake-build--build-buffer-name))
    (f (cmake-build--run-buffer-name))))

;;;; Menu stuff

(defun cmake-build--menu-profiles ()
  "Return a menu for selecting a cmake profile."
  `((:set-profile menu-item ,(concat "Profile: " (symbol-name cmake-build-profile))
                  (keymap nil
                          ,@(mapcar (lambda (x)
                                      (list (car x) 'menu-item (symbol-name (car x)) t))
                                    (cmake-build--get-cmake-profiles))))))

(defun cmake-build--menu-configs ()
  "Return a menu for selecting a cmake config."
  (let ((config (cmake-build--get-build-config (cmake-build-get-run-config-name)))
        (name (symbol-name (cmake-build-get-run-config-name))))
    `((:set-config menu-item ,(concat "Config: "
                                      (if config name "<none selected>"))
                   (keymap nil
                           ,@(mapcar (lambda (x)
                                       (list (car x) 'menu-item (symbol-name (car x)) t))
                                     (cmake-build--get-configs)))))))

(defun cmake-build--menu-other-targets ()
  "Return a menu for selecting a cmake target."
  `((:build-other-target menu-item "Other Targets"
                         (keymap nil
                                 ,@(mapcar (lambda (x)
                                             (list x 'menu-item x t))
                                           (cmake-build--get-other-targets))))))


(defun cmake-build--menu-settings ()
  "Return a menu for setting cmake-build settings."
  `((:info menu-item "Project Info" t)
    ,@(when (cmake-build--get-cmake-profiles)
        (cmake-build--menu-profiles))
    ,@(when (cmake-build--get-configs)
        (cmake-build--menu-configs))
    (nil menu-item "Tools"
         (keymap nil
                 (:cmake menu-item "Re-run cmake" t)
                 (:clean menu-item "Clean build" t)
                 (:nuke menu-item "Delete cache/Re-run cmake" t)
                 (:set-buffer-local menu-item "Set buffer-local run config" t)
                 (:set-options menu-item "Set cmake options" t)
                 (:set-root menu-item "Set project SOURCE root" t)
                 (:set-build-root menu-item "Set project BUILD root" t)))))


(defun cmake-build--menu (&optional config)
  "Return menu for cmake-build adapted to CONFIG."
  (let ((config (or config (car (cmake-build--get-build-config)))))
    `(keymap "CMake Build"
             (:debug menu-item ,(concat "Debug " config) t)
             (:build menu-item ,(concat "Build " config) t)
             (:run menu-item ,(concat "Run " config) t)
             ,@(cmake-build--menu-configs)
             ,@(when (cmake-build--get-other-targets)
                 (cmake-build--menu-other-targets))
             ,@(cmake-build--menu-settings))))

(defun cmake-build--popup-menu (config)
  "Popup a menu for cmake-build adapted to CONFIG."
  (x-popup-menu
   (list '(10 10) (selected-window))
   (cmake-build--menu config)))

(defun cmake-build--popup-settings-menu ()
  "Popup a menu for cmake-build settings."
  (x-popup-menu
   (list '(10 10) (selected-window))
   `(keymap "CMake Build: Settings" ,@(cmake-build--menu-settings))))

(defun cmake-build--menu-action-dispatch (action)
  "Dispatch ACTION to the appropriate function."
  (cl-case (car action)
    (:info (message "Project root: %s" (cmake-build--project-root)))
    (:debug (cmake-build-debug))
    (:build (cmake-build-current))
    (:run (cmake-build-run))
    (:set-config (cmake-build-set-config (cadr action)))
    (:set-profile (cmake-build-set-cmake-profile (cadr action)))
    (:cmake (cmake-build-run-cmake))
    (:clean (cmake-build-clean))
    (:build-other-target (cmake-build-other-target (cadr action)))
    (:nuke (cmake-build-clear-cache-and-configure))
    (:set-options (call-interactively #'cmake-build-set-options))
    (:set-buffer-local (cmake-build-set-buffer-local-config))
    (:set-root (call-interactively #'cmake-build-set-project-root))
    (:set-build-root (call-interactively #'cmake-build-set-project-build-root))))

(defun cmake-build-menu ()
  "Popup a menu for cmake-build."
  (interactive)
  (let ((config (car (cmake-build--get-build-config))))
    (cmake-build--menu-action-dispatch
     (if config
         (cmake-build--popup-menu config)
       (cmake-build--popup-settings-menu)))))

(cmake-build--read-options)
(add-hook 'kill-emacs-hook #'cmake-build--write-options)

(provide 'cmake-build)
;;; cmake-build.el ends here
