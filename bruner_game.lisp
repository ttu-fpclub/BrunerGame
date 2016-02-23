(defparameter *nodes* '((BRUN404 (You are in a small room filled with functional programmers. See how "I" did not make a joke about room not found.))
                        (BRUN207 (You are in a very large room with multiple LCD displays and two projectors. You see a young man typing away at a keyboard.))
                        (BRUN213 (You see a man wearing glasses staring back at you. There are books about genetic algorithms and AI on his desk.))
			(BRUN214 (You see a lady studying a book on Scala.))
			(BRUN206 (You see a young man wearing a shirt that reads - "I love Assembly". You also see a book labeled "Asssembly is Fun".))
))
(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

(defparameter *edges* '((BRUN404 (BRUN206 east portal))
                        (BRUN206 (BRUN207 east portal)
				 (BRUN404 west portal))
                        (BRUN207 (BRUN206 west portal)
			         (BRUN213 east portal))
			(BRUN213 (BRUN207 west portal)
			         (BRUN214 east portal))
			(BRUN214 (BRUN213 west portal)) 
))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(keyboard assembly-book scala-book genetic-algorithms-book AI-book))

(defparameter *object-locations* '((keyboard BRUN207)
                                   (assembly-book BRUN206)
                                   (scala-book BRUN214)
                                   (genetic-algorithms-book BRUN213)
			           (AI-book BRUN213)))

(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'BRUN404)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	  (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun have (object) 
    (member object (cdr (inventory))))

(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))
