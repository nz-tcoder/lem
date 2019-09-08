(defpackage :lem-run-shell-mode
  (:use :cl :lem)
  (:export :run-shell
           :*shell-name*))

(in-package :lem-run-shell-mode)

(defvar *process* nil)
(defvar *buffer-name* "*shell*")
(defvar *shell-name* "bash")
(defvar *enable-complete* nil)
(defvar *shell-command* nil)
(defvar *command* nil)
(defvar *echo-buffer* nil)
(defvar *completion-p* nil)
(defvar *completion-buffer* nil)
(defvar *completion-start-point* nil)
(defvar *completion-end-point* nil)
(defvar *completion-end-point* nil)
(defvar *completion-timer* nil)
(defvar *completion-timeout* 200)
(defvar *default-bash-init-file-content* "PS1='$ '")

(defvar *assert* nil)

(defvar *run-shell-syntax-table*
  (make-syntax-table
   :space-chars '(#\space #\tab #\newline)
   :symbol-chars '(#\+ #\- #\/ #\= #\. #\_ #\% #\:)
   :paren-pairs '((#\[ . #\])
                  (#\{ . #\}))
   :string-quote-chars '(#\" #\')
   :line-comment-string "#"))

(define-major-mode run-shell-mode ()
    (:name "Run shell"
     :keymap *run-shell-mode-keymap*
     :syntax-table *run-shell-syntax-table* )
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
  (let ((items (cdr *completion-buffer*))) ; drop prompt
    (if items
        (lem.completion-mode:run-completion
         (lem.completion-mode:make-completion-spec
          #'(lambda (point)
              (declare (ignore point))
              (mapcar #'(lambda (completion)
                          (lem.completion-mode:make-completion-item
                           :label completion
                           :detail ""
                           :start *completion-start-point*
                           :end *completion-end-point*))
                      items))
          :prefix-search t)))
    (setq *completion-p* nil
          *completion-buffer* nil
          *completion-timer* nil)))

(defun make-query-command (string)
  (if (ppcre:scan "\\s" string)
      (values 'file (format nil "compgen -A file ~@[~a~]"
                           (unless (ppcre:scan "\\s$" string)
                             (car (last (ppcre:split "\\s" string))))))
      (values 'command (format nil "compgen -A command ~a" string))))

(defun skip-to-space (point)
  (if (eql (character-at point) #\space)
      point
      (with-point ((p point))
        (skip-chars-backward p #'(lambda (x) (not (eql x #\space))))
        p)))

(defun query-completion (start end)
  (if (point/= start end)
      (multiple-value-bind (type query-command)
          (make-query-command (points-to-string start end))
        (setq *command* query-command
              *completion-p* t
              *completion-end-point* end
              *completion-start-point*
              (if (eq type 'command)
                  start
                  (skip-to-space end)))
        (lem-process:process-send-input *process*
                                        (concatenate 'string query-command
                                                     (string #\newline))))))

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
    (cond ((and *command* (not (ppcre:scan "\\n" output))) ; buffered
           (push output *echo-buffer*))
          ((null *command*)                                ; command result
           (complete/insert output))
          (t                                               ; the first is echo
           (ppcre:register-groups-bind (echo rest)
               ("(.*)\\n((?s).*)" output)
             (if *assert*
                 (let ((echo-string (format nil "~{~a~}"
                                            (reverse (cons echo *echo-buffer*)))))
                   (message "~a,~a:~a"
                            (string= *command* echo-string)
                            *command* echo-string)))
             (setq *echo-buffer* nil
                   *command* nil)
             (complete/insert rest))))))

(defun setup-bash-command ()
  (let* ((init-dir (merge-pathnames ".lem/" (user-homedir-pathname)))
         (init-file (merge-pathnames "run-bashrc" init-dir)))
    (if (null (cl-fad:file-exists-p init-dir))
        (list *shell-name* "--norc")
        (progn
          (unless (cl-fad:file-exists-p init-file)
            (with-open-file (st init-file :direction :output)
              (write-line *default-bash-init-file-content* st)))
          (list *shell-name* "--rcfile" (namestring init-file))))))

(defun run-shell-internal ()
  (unless (alive-process-p)
    (or *shell-command*
        (setf *shell-command*
              (cond ((string= *shell-name* "bash")
                     (setf *enable-complete* t)
                     (setup-bash-command))
                    (t (list *shell-name*)))))
    (setf *process*
          (lem-process:run-process *shell-command*
                                   :name "run-shell"
                                   :output-callback 'output-callback)))
  (lem.listener-mode:listener-start *buffer-name* 'run-shell-mode))

(define-command run-shell () ()
  (run-shell-internal))

(define-command shell-complete  () ()
  (and *enable-complete*
       (query-completion (lem.listener-mode::listener-start-point (current-buffer))
                         (current-point))))

(define-command kill-shell-process  () ()
  (lem-process:delete-process *process*))
