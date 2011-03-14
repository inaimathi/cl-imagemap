Really, I want to be able to write

(defun parse-tag (stream)
  (skip-past #\<)
  (list :tag-name (read-word)
	(read-properties)))

and have it return a tag.