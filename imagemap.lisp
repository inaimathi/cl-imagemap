(dolist (module '(cl-ppcre cl-who)) (ql:quickload module))

(defpackage :cl-imagemap (:use :cl :cl-ppcre :cl-who))
(in-package :cl-imagemap)

(load "util.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; output
(defun svg-tree->html (tree)
  (mapcar #'svg-tag->area tree))

(defun svg-tag->area (svg-tag)
  (let ((points (case (getf svg-tag :tag)
		  ('polygon (svg-points->area-points (getf svg-tag :points)))
		  ('polyline (svg-points->area-points (getf svg-tag :points)))
		  ('path (svg-points->area-points (getf svg-tag :d)))
		  ('rect (destructuring-bind (x y w h) (gets svg-tag :x :y :width :height)
			   (format nil "~{~a~^,~}" (list x y (+ x w) (+  y h)))))
		  ('circle (destructuring-bind (x y radius) (gets svg-tag :cx :cy :r)
			     (format nil "~{~a~^,~}" (list x y radius))))
		  (otherwise (error "How the fuck did that happen? Tag type: ~a" (getf svg-tag :tag))))))
    (html-to-stout
      (:area :shape (getf svg-tag :tag) :class (getf svg-tag :fill) :coords points  :href "#"))))

(defun svg-points->area-points (svg-points)
  (let ((p (mapcar (lambda (pair) (format nil "~a,~a" (car pair) (cdr pair))) svg-points)))
    (if (string= (car p) (car (last p)))
	(format nil "~{~a~^,~}" p)
	(format nil "~{~a,~}~a" p (car p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; readers/parsers
(defun file->svg-tags (file-name)
  (with-open-file (stream file-name) 
    (stream->svg-tags stream)))

(defun stream->svg-tags (str)
  (let ((tag nil))
    (loop do (setf tag (read-tag str))
	 when (scan "<(svg|rect|path|polygon|polyline|circle) " tag) collect (parse-tag tag)
	 until (not tag))))

(defun read-tag (&optional stream)
  (read-section #\< #\> stream))

(defun parse-tag (tag-string)
  (let ((props (list :TAG (intern (string-upcase (regex-first-group "<(.*?) " tag-string))))))
    (do-matches-as-strings (prop "\\w+?=[\"'].*?[\"']" tag-string nil) 
      (destructuring-bind (key val) (split "=[\"']?" prop)
	(let ((parse-fn (cond ((member key '("d" "points") :test #'string=) #'parse-points)
			      ((member key '("x" "y" "width" "height" "points" "cx" "cy" "r") :test #'string=) #'parse-num)
			      (t #'parse-str))))
	  (setf props (cons (intern (string-upcase key) :keyword) (cons (funcall parse-fn val) props))))))
    props))

(defun parse-points (point-string)
  (let ((props nil))
    (do-matches-as-strings (prop "\\-?\\d+\\.?\\d*" point-string nil)
      (setf props (cons (round (read-from-string prop)) props)))
    (loop for (x y) on (nreverse props) by #'cddr collect (cons x y))))

(defun parse-num (num-string) (parse-integer num-string :junk-allowed t))

(defun parse-str (str) (subseq str 0 (- (length str) 1)))

(defun read-section (start end &optional stream)
  (let* ((p (peek-char start stream nil 'eof))
	 (char nil))
    (unless (eql 'eof p)
      (coerce (loop do (setf char (read-char stream nil 'eof))
		 unless (member char (list 'eof #\Newline)) collect char
		 until (member char (list 'eof end)))
	      'string))))