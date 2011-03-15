(in-package :cl-imagemap)

(defmacro html-to-stout (&body body)
  "Outputs HTML to standard out."
  `(with-html-output (*standard-output* nil :indent t) ,@body))

(defmacro gets (object &rest symbols)
  `(list ,@(loop for s in symbols collect `(getf ,object ,s))))

(defun regex-first-group (regexp target-string)
  (multiple-value-call 
      (lambda (match group) (declare (ignore match)) (aref group 0)) 
    (scan-to-strings regexp target-string)))

(defun cmd-args ()
  (or #+SBCL *posix-argv*  
      #+LISPWORKS system:*line-arguments-list*
      #+CMU extensions:*command-line-words*
      nil))