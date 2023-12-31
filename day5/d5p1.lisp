(ql:quickload :cl-ppcre)

;; Macro for nicer syntax
(defmacro ->> (value &optional form &rest more)
  (cond
    ((null form) value)
    ((null more) (if (listp form)
		     `(,@form ,value)
		     (list form value)))
    (t `(->> (->> ,value ,form) ,@more))))

;; AoC 2023, Day 5
(defun in-range (x a b)
  (and (>= x a) (<= x b)))

(defun conversion (src specs)
  (loop for (fd fs r) in specs
	when (in-range src fs (+ fs (- r 1)))
	  return (+ src (- fd fs))
	finally (return src)))

(defun parse-input (file-path)
  (with-open-file (stream file-path)
    (do ((line (read-line stream nil)
	       (read-line stream nil))
	 (seeds '())
	 (map-data (make-hash-table))
	 (read-map nil))
	((null line) (list seeds map-data))

      (cond
	((uiop:string-prefix-p "seeds:" line)
	 (setf seeds (mapcar 'read-from-string (ppcre:split " " (subseq line 7)))))

	((uiop:emptyp line)
	 (setf read-map nil))
	
	((uiop:string-suffix-p line "map:")
	 (setf read-map (subseq line 0 (- (length line) 5))))

	(read-map
	 (push (mapcar 'read-from-string (ppcre:split " " line))
	       (gethash (read-from-string read-map) map-data)))))))

;; Part 1
(defun part1 (input-path)
  (destructuring-bind (seeds conversions) (parse-input input-path)
    (flet ((convert (type value)
	     (conversion value (gethash type conversions))))
      (loop for seed in seeds
	    minimizing (->> seed
			    (convert 'seed-to-soil)
			    (convert 'soil-to-fertilizer)
			    (convert 'fertilizer-to-water)
			    (convert 'water-to-light)
			    (convert 'light-to-temperature)
			    (convert 'temperature-to-humidity)
			    (convert 'humidity-to-location))))))

(part1 "./input.txt")
;; 389056265
