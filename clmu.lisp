(asdf:load-system :dexador)
(asdf:load-system :lquery)

;; Add manga here
;;(defvar *manga*
;;  '(
;;    (:title "Manga 1"
;;     :url "https://www.mangaupdates.com/releases.html?search=[xxxx]&stype=series")
;;    (:title "Manga 2"
;;     :url "https://www.mangaupdates.com/releases.html?search=[xxxx]&stype=series")
;;    ))

(defvar *manga*
  '((:title "Golden Kamui"
     :url "https://www.mangaupdates.com/releases.html?search=113608&stype=series")
    (:title "Chainsaw Man"
     :url "https://www.mangaupdates.com/releases.html?search=151847&stype=series")
    (:title "Berserk"
     :url "https://www.mangaupdates.com/releases.html?search=88&stype=series")
    (:title "Sono Bisque Doll"
     :url "https://www.mangaupdates.com/releases.html?search=146700&stype=series")
    ))

(defun spaces (n acc)
  (if (= n 0) acc (spaces (- n 1) (concatenate 'string " " acc))))

(defun release (title date chapter group)
  (list :title title
	:date date
	:chapter chapter
	:group group))

(defun prompt-read (prompt)
  (format *query-io* "~a " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-manga ()
  (manga
   (prompt-read "Title:")
   (prompt-read "Url:")))

(defun load-url (url)
  (let ((request (dex:get url)))
    (lquery:$ (initialize request))))

(defun parse-info (page query n)
  (let ((info (lquery:$ page query (text))))
    (elt info n)))

(defun parse-release (m)
  (let ((page (load-url (getf m :url))))
    (release
     (getf m :title)
     (parse-info page "#main_content .col-2" 1)
     (parse-info page "#main_content .col-1" 3)
     (parse-info page "#main_content .col-4" 3))))

(defun get-len (rl id)
  (apply #'max (mapcar (lambda (x) (length (getf x id))) rl)))

(defun format-id (r n id)
  (let ((str (getf r id)))
    (if (eq id :title)
	    (concatenate 'string str (spaces (- n (length str)) ""))
    	    (concatenate 'string (spaces (- n (length str)) "") str))))

(defun print-release (r tl cl)
  (format t "~a | ~a | ~a | ~a~%"
	  (getf r :date)
	  (format-id r tl :title)
	  (format-id r cl :chapter)
	  (getf r :group)))

(defun compare-date (d1 d2)
  (let ((y1 (parse-integer (subseq d1 6 8)))
	(y2 (parse-integer (subseq d2 6 8)))
	(m1 (parse-integer (subseq d1 0 2)))
	(m2 (parse-integer (subseq d2 0 2)))
	(d1 (parse-integer (subseq d1 3 5)))
	(d2 (parse-integer (subseq d2 3 5))))    
    (if (not (equal y1 y2)) (> y1 y2)
	(if (not (equal m1 m2)) (> m1 m2) (> d1 d2)))))

(defun get-releases ()
  (sort
   (mapcar #'parse-release *manga*)
   (lambda (r1 r2) (compare-date (getf r1 :date)
				 (getf r2 :date)))))
(defun main ()
  (let ((rels (get-releases)))
    (mapcar (lambda (r)
	      (print-release
	       r (get-len rels :title) (get-len rels :chapter)))
	    rels)))
