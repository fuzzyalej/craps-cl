;;;; The game of craps 

(defun throw-die ()
  "Simulates the throw of a die"
  (+ 1 (random 6)))

(defun throw-dice (&optional (dices 2))
  "Simulates the throw of two dice. The result is a list of two results: (1 5)"
  (append
    (loop
      for i from 1 upto dices
      collect (throw-die))))

(defun snake-eyes-p (result)
  "'Snake eyes' is throwing two ones"
  (equalp result '(1 1)))

(defun boxcars-p (result)
  "'Boxcars' is throwing two sixes"
  (equalp result '(6 6)))

(defun sum-of-throw (result)
  (apply #'+ result))

(defun instant-win-p (result)
  "If the first throw is a 7 or 11, you win"
  (let ((total (sum-of-throw result)))
    (or (equalp 7 total)
        (equalp 11 total))))

(defun instant-loss-p (result)
  "If the first throw is a 2, 3 or 12, you lose"
  (let ((total (sum-of-throw result)))
    (or (equalp 2 total)
        (equalp 3 total)
        (equalp 12 total))))

(defun say-throw (result)
  "Returns the result of the throw or the name of the play"
  (cond ((snake-eyes-p result) 'SNAKE-EYES)
        ((boxcars-p result) 'BOXCARS)
        (t (sum-of-throw result))))

(defun say-status (result)
  "Returns the status of the throw"
  (cond ((instant-win-p result) '(YOU WIN))
        ((instant-loss-p result) '(YOU LOSE))
        (t (append '(YOUR POINT IS) (list (sum-of-throw result))))))

(defun try-for-point (point)
  "Continue playing once a point has been stablished. 7 loses, the point wins and the rest goes on playing"
  (let* ((result (throw-dice))
         (sum (sum-of-throw result)))
    (cond ((equalp point sum) '(YOU SUPER WIN THE GAME))
          ((equalp point 7) '(YOU LOSE THE GAME))
          (t '(YOU HAVE TO KEEP TRYING!)))))

(defun craps ()
  "Starts the game of craps"
  (let ((result (throw-dice)))
    (list 'THROW (car result) 'AND (cadr result) '-- (say-throw result) '-- (say-status result))))
