(defpackage :lem-run-shell-mode
  (:use :cl :lem)
  (:export :run-shell))

(in-package :lem-run-shell-mode)

(defvar *process* nil)
(defvar *buffer-name* "*shell*")
(defvar *shell-command* '("bash" "--posix"))
(defvar *command* nil)
(defvar *echo-buffer* nil)
(defvar *completion-p* nil)
(defvar *completion-buffer* nil)
(defvar *completion-start-point* nil)
(defvar *completion-end-point* nil)
(defvar *completion-end-point* nil)
(defvar *completion-timer* nil)
(defvar *completion-timeout* 200)

(define-major-mode run-shell-mode ()
    (:name "Run shell"
     :keymap *run-shell-mode-keymap*)
  (reset-listener-variables (current-buffer))
  (lem.listener-mode:listener-mode t))

(define-key *run-shell-mode-keymap* "Tab" 'shell-complete)

(defun reset-listener-variables (buffer)
  (setf (variable-value 'lem.listener-mode:listener-set-prompt-function :buffer buffer)
        #'identity
        (variable-value 'lem.listener-mode:listener-check-input-function :buffer buffer)
        (constantly t)
        (variable-value 'lem.listener-mode:listener-execute-function :buffer buffer)
        'execute-input))

(defun alive-process-p ()
  (and *process*
       (lem-process:process-alive-p *process*)))

(defun repl-buffer-exists-p ()
  (lem:get-buffer *buffer-name*))

(defun get-repl-buffer ()
  (let ((buffer (make-buffer *buffer-name*)))
    (change-buffer-mode buffer 'run-shell-mode)
    buffer))

(defvar *debug* nil)
(defvar *assert* nil)

(defun insert-result (string)
  (let* ((already-exists (repl-buffer-exists-p))
         (buffer (get-repl-buffer))
         (p (buffer-point buffer)))
    (buffer-end p)
    (insert-string p string)
    (lem.listener-mode:listener-reset-prompt buffer nil)
    (unless already-exists
      (setf (current-window) (display-buffer buffer)))
    (alexandria:when-let (window (first (get-buffer-windows buffer)))
      (with-current-window window
        (buffer-end p)
        (window-see window)))
    (redraw-display)))

(defun execute-input (point string)
  (declare (ignore point))
  (lem-process:process-send-input *process*
                                  (concatenate 'string string (string #\newline)))
  (setq *command* string))

(defun start-shell-completion ()
  (let ((items (reverse (cdr *completion-buffer*))))
    (if items
        (lem.completion-mode:run-completion
         #'(lambda (point)
             (declare (ignore point))
             (mapcar #'(lambda (completion)
                         (lem.completion-mode:make-completion-item
                          :label completion
                          :detail ""
                          :start *completion-start-point*
                          :end *completion-end-point*))
                     items)))))
  (setq *completion-p* nil
        *completion-buffer* nil
        *completion-timer* nil))

(defun query-completion (start end)
  (let ((query-command (format nil "compgen -A command ~a" (points-to-string start end))))
    (lem-process:process-send-input *process* 
                                    (concatenate 'string query-command (string #\newline)))
    (setq *command* query-command
          *completion-p* t
          *completion-start-point* start
          *completion-end-point* end)))

(defun complete/insert (string)
  (if *completion-p*
      (progn
        (and *completion-timer*
             (stop-timer *completion-timer*))
        (alexandria:if-let ((lst (ppcre:split "\\n" string)))
          (setq *completion-buffer*
                (append lst *completion-buffer*)))
        (setq *completion-timer*
              (start-timer *completion-timeout* nil #'start-shell-completion)))
      (insert-result string)))

(defun output-callback (string)
  (let ((output (ppcre:regex-replace-all "\\r\\n" string
                                         (string #\newline))))
    (push output *debug*)
    (cond ((and *command* (not (ppcre:scan "\\n" output))) ; buffered
           (push output *echo-buffer*))
          ((null *command*)  ; command result
           (complete/insert output))
          (t           ; the first is command echo
           (ppcre:register-groups-bind (echo rest)
               ("(.*)\\n((?s).*)" output)
             (if *assert*
                 (let ((echo-string (format nil "~{~a~}"
                                            (reverse (cons echo *echo-buffer*)))))
                   (message (format nil "~a,~a:~a"
                                    (string= *command* echo-string)
                                    *command* echo-string))))
             (and *completion-p*
                  (start-timer *completion-timeout* nil #'start-shell-completion))
             (setq *echo-buffer* nil
                   *command* nil)
             (complete/insert rest))))))

#|
    (let ((output-string (ppcre:regex-replace-all  "\\r\\n" string (string #\newline))))
      (multiple-value-bind (drop-echo has-echo)
          (if command
              (ppcre:regex-replace (format nil "^~a~%" command) output-string "")
              (values output-string nil))
        ;; debug
        (push (list command string drop-echo) *debug*)
        (if has-echo
            (setq command nil))
        (if completion-p
            (let ((items (butlast (ppcre:split "\\n" drop-echo))))
              (when items
                (setq completion-p nil)
                (lem.completion-mode:run-completion
                 #'(lambda (point)
                     (declare (ignore point))
                     (mapcar #'(lambda (completion)
                                 (lem.completion-mode:make-completion-item :label completion :detail ""
                                                                           :start start-point :end end-point))
                             items)))))
            (insert-result drop-echo))))))
|#

(defun run-shell-internal ()
  (unless (alive-process-p)
    (setf *process*
          (lem-process:run-process *shell-command*
                                   :name "run-shell"
                                   :output-callback 'output-callback)))
  (lem.listener-mode:listener-start *buffer-name* 'run-shell-mode))

(define-command run-shell () ()
  (run-shell-internal))

(define-command shell-complete  () ()
  (query-completion (lem.listener-mode::listener-start-point (current-buffer))
                    (current-point)))
;;; debug
(define-command show-shell-complete  () ()
  (let* ((start (lem.listener-mode::listener-start-point (current-buffer)))
         (str (points-to-string start (current-point))))
    (message (format nil "compgen -A command ~a" str))))

