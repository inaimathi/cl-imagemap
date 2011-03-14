(in-package :cl-imagemap)

(defun test (file)
  (with-open-file (stream file)
    (let ((tag nil))
      (loop do (setf tag (read-tag stream))
	   collect tag until (not tag)))))

(defun read-tag (stream)
  (when (skip-past #\< stream)
    (let ((tag-name (chars->symbol (read-word stream))))
      (when (member tag-name '(svg rect path polygon polyline circle))
	(cons :tag-name
	      (cons tag-name
		    (loop collect (read-property stream)
		       until (next-char-in? "/>" stream))))))))

(defun read-property (stream)
  (peek-char t stream)
  (let ((key (chars->symbol (read-word stream) :keyword)))
    (ignore-chars "=\"'" stream)
    (let ((val (list key
		     (cond ((member key '(d points)) (read-to "\"'" stream))
			   ((member key '(x y width height points cx cy r)) (read-number stream))
			   (t (coerce (read-to "\"'" stream) 'string))))))
      (ignore-chars "\"'" stream)
      val)))

(defun chars->symbol (list-of-chars &optional package) 
  (let ((s (string-upcase (coerce list-of-chars 'string))))
    (if package
	(intern s package)
	(intern s))))

(defun read-word (stream) (read-to " ,.</>{}[]=\\\"" stream))

(defun read-number (stream) (read-while "123456789"))
(defun read-float (stream) 
  (read-number stream)
  (read-*

;; (defun read-tag (&optional stream) (read-section #\< #\> stream))

(defun next-char-in? (char-seq &optional stream)
  (let ((c (char-list chars))
	(next-char (peek-char nil stream nil :eof)))
    (values next-char (member next-char c :test #'eql))))

(defun read-section (start end &optional stream)
  (when (skip-to start stream)
    (coerce (read-to end stream) 'string)))

(defun skip-past (char &optional stream)
  (skip-to char stream)
  (read-char stream))

(defun skip-to (char &optional stream)
  (peek-char char stream nil :eof))

(defun read-to (char-seq &optional stream)
    (let ((c (char-list char-seq)))
    (cond ((eof? stream) :eof)
	  ((next-char-in? c stream) nil)
	  (t (loop 
		collect (read-char stream) 
		until (or (eof? stream) (next-char-in? c stream)))))))

(defun read-while (char-seq &optional stream)
  (let ((c (char-list char-seq)))
    (cond ((eof? stream) :eof)
	  ((not (next-char-in? c stream)) nil)
	  (t (loop 
		collect (read-char stream) 
		until (or (eof? stream) (not (next-char-in? c stream))))))))
  
(defun ignore-chars (chars &optional stream) (read-while chars stream))

(defun eof? (&optional stream)
  (eq (peek-char nil stream nil :eof) :eof))

(defun end? (stream char-seq) 
  (or (eof? stream) (not (next-char-in? c stream))))

(defun char-list (args)
  (cond ((listp args) args)
	((stringp args) (coerce args 'list))
	((characterp args) (list args))
	(t (error "Given non-string, non-list, non-char: ~a" args))))