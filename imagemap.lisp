(dolist (module '(cl-ppcre cl-fad cl-who)) (ql:quickload module))

(defpackage :cl-imagemap 
  (:use :cl :cl-ppcre :cl-fad :cl-who))
(in-package :cl-imagemap)

;;relevant tags: rect|path|polygon|polyline|circle
;;polygon, polyline are basically the same (a string of x,y pairs in the `points` attribute)
;;rect is a bit more complicated. four relevant attributes: x, y, width, height
;;circle has three relevant atributes: cx, cy and r (x,y of the center and the radius)
;;path is basically a stripped down implementation of PS in a tag. Frankly, just skip it for now. Implement later if you have the nerve.

(defun test (file)
  (with-open-file (stream file)
    (stream->svg-tags stream)))

;; "map_ov-550.rtf"
;; "map-na.svg"

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
    (nreverse (loop for (x y) on props by #'cddr collect `(,x . ,y)))))

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

(defmacro val-get (key a-list)
  `(cdr (assoc ,key ,a-list :test #'equal)))

(defun regex-first-group (regexp target-string)
  (multiple-value-call 
      (lambda (match group) 
	(declare (ignore match))
	(aref group 0)) 
    (scan-to-strings regexp target-string)))

;; "<rect x=\"12\" y=\"12\" width=\"200\" height=\"100\" />"
;; "<path id=\"path2308\" d=\"M 0,1090.4999 L 0,-6.3060668e-014 L 1142.5,-6.3060668e-014 L 2285,-6.3060668e-014 L 2285,1090.4999 L 2285,2180.9999 L 1142.5,2180.9999 L 0,2180.9999 L 0,1090.4999 z \" style=\"fill:#9ec7f3;fill-opacity:1\" />"