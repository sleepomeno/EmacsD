(defmacro inc (var)
  (list 'setq var (list '1+ var)))

(defmacro for (var from init to final do &rest body)
  "Execute a simple for loop: (for i from 1 to 10 do (print i))."
  `(let ((,var ,init)
         (max ,final))
     (while (<= ,var max)
       ,@body
       (inc ,var))))


(macroexpand '(for i from 1 to 3 do
                   (setq square (* i i))
                   (princ (format "\n%d %d" i square))))

(let ((max 6))
  (for i from 1 to 3 do
       (setq square (* i max))
       (princ (format "\n%d %d" i square))))

(defun bla () (foo 3))
(defun foo (integer)
  (let ((bla 4))
    (* bla integer)))


(bla)

