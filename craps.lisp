;;;; The game of craps 

(defun throw-die ()
  "Simulates the throw of a die"
  (+ 1 (random 6)))

; TODO - refactor for N dices
(defun throw-dice ()
  "Simulates the throw of two dice. The result is a list of two results: (1 5)"
  (list (throw-die) (throw-die)))

(defun snake-eyes-p (result)
  "'Snake eyes' is throwing two ones"
  (equalp result '(1 1)))

(defun boxcars-p (result)
  "'Boxcars' is throwing two sixes"
  (equalp result '(6 6)))

;; TODO - refactor with let
;; TODO - extract method to sum dices
(defun instant-win-p (result)
  "If the first throw is a 7 or 11, you win"
  (or (equalp 7 (apply #'+ result))
      (equalp 11 (apply #'+ result))))

(defun instant-loss-p (result)
  "If the first throw is a 2, 3 or 12, you lose"
  (or (equalp 2 (apply #'+ result))
      (equalp 3 (apply #'+ result))
      (equalp 12 (apply #'+ result))))

(defun say-throw (result)
  "Returns the result of the throw or the name of the play"
  (cond ((snake-eyes-p result) 'SNAKE-EYES)
        ((boxcars-p result) 'BOXCARS)
        (t (apply #'+ result))))

(defun say-status (result)
  "Returns the status of the throw"
  (cond ((instant-win-p result) '(YOU WIN))
        ((instant-loss-p result) '(YOU LOSE))
        (t (append '(YOUR POINT IS) (list (apply #'+ result))))))

;; TODO - Add nice message
(defun try-for-point (point)
  "Continue playing once a point has been stablished. 7 loses, the point wins and the rest goes on playing"
  (let* ((result (throw-dice))
         (sum (apply #'+ result)))
    (cond ((equalp point sum) 'win)
          ((equalp point 7) 'lose)
          (t 'keep-trying))))

(defun craps ()
  "Starts the game of craps"
  (let ((result (throw-dice)))
    (list 'THROW (car result) 'AND (cadr result) '-- (say-throw result) '-- (say-status result))))
