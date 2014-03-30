;;;; Tests for craps.lisp

;;TODO
;; - wouldnt it be awesome to have a function that tries all combinations of two dices
;;   and another one that will count the trues for that combinations, to use that in the tests?

(load "craps.lisp") ; load the file to be tested

(defun test (fn value expected)
  "Awesomic assert function for tests"
  (cond ((funcall fn value expected) 'OK)
        (t (list value 'SHOULD 'BE expected))))

(defun test-eq (value expected)
  "Assert equal"
  (print (test #'equalp value expected)))

(defun test-predicate (fn result)
  "Assert fn"
  (test-eq (funcall fn result) t))

;; Actual tests

(defun run ()
  ;; throw-die
  (test-predicate #'numberp (throw-die))
  (test-predicate #'(lambda (n) (< n 7)) (throw-die))
  (test-predicate #'(lambda (n) (> n 0)) (throw-die))

  ;; throw-dice
  (test-predicate #'listp (throw-dice))
  (test-eq (length (throw-dice)) 2)

  ;; snake-eyes-p
  (test-eq (snake-eyes-p '(1 1)) t)
  (test-eq (snake-eyes-p '(1 2)) nil)
  (test-eq (snake-eyes-p '(6 6)) nil)

  ;; boxcars-p
  (test-eq (boxcars-p '(1 1)) nil)
  (test-eq (boxcars-p '(1 2)) nil)
  (test-eq (boxcars-p '(6 6)) t)

  ;; instant-win-p
  (test-eq (instant-win-p '(6 1)) t)
  (test-eq (instant-win-p '(5 6)) t)
  (test-eq (instant-win-p '(6 6)) nil)
  (test-eq (instant-win-p '(1 1)) nil)

  ;; instant-loss-p
  (test-eq (instant-loss-p '(1 1)) t)
  (test-eq (instant-loss-p '(2 1)) t)
  (test-eq (instant-loss-p '(6 6)) t)
  (test-eq (instant-loss-p '(5 5)) nil)
  (test-eq (instant-loss-p '(6 3)) nil)
  (test-eq (instant-loss-p '(3 1)) nil)

  ;; say-throw
  (test-eq (say-throw '(1 1)) 'SNAKE-EYES)
  (test-eq (say-throw '(6 6)) 'BOXCARS)
  (test-eq (say-throw '(1 2)) 3)
  (test-eq (say-throw '(1 6)) 7)

  ;; say-status
  (test-eq (say-status '(1 6)) '(YOU WIN))
  (test-eq (say-status '(1 1)) '(YOU LOSE))
  (test-eq (say-status '(4 4)) '(YOUR POINT IS 8))

  ;; try-for-point
  (test-predicate #'symbolp (try-for-point 4))

  'FINISHED
  )
