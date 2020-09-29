(defun IA-relative-shift (start-point relative-shift)
  "Apply a shift to the starting point to get a second coordinate"
  (list
   (+ (car start-point) (car relative-shift))
   (+ (cadr start-point) (cadr relative-shift)))
  )

(defun IA-apply-cast-mask (rule number1 number2)
  "Cast number1 and number2 into a list according to a rule"
  (cond
   ((= rule "++")
    (list number1 number2))
   ((= rule "+-")
    (list number1 (- number2)))
   ((= rule "-+")
    (list (- number1) number2))
   ((= rule "--")
    (list (- number1) (- number2)))))

(defun IA-apply-cast-mask-equal (rule number)
  "Runs IA-apply-cast-mask but with samy number"
  (IA-apply-cast-mask rule number number))

(defun IA-eval-cast-mask (verbose-form)
  "Evaluate the rule to cast coordinates with: (x,y) -> (±x,±y)"
  (cond
   ((= direction "south-to-east")
    "++")
   ((= direction "east-to-south")
    "--")
   ((= direction "east-to-north")
    "-+")
   ((= direction "north-to-east")
    "+-")
   ((= direction "north-to-west")
    "--")
   ((= direction "west-to-north")
    "++")
   ((= direction "west-to-south")
    "+-")
   ((= direction "south-to-west")
    "-+")))

(defun IA-eval-angle (verbose-form)
  "Evaluate the rule to cast coordinates with"
  (cond
   ((= direction "south-to-east")
    -90)
   ((= direction "east-to-south")
    90)
   ((= direction "east-to-north")
    -90)
   ((= direction "north-to-east")
    90)
   ((= direction "north-to-west")
    -90)
   ((= direction "west-to-north")
    90)
   ((= direction "west-to-south")
    -90)
   ((= direction "south-to-west")
    90)))

(defun IA-my-corner (arc-start corner-size direction / arc-end)
  "Create cornet starting from arc-start, in the indicated direction and with specified corner size

-> Return coordinate of endpoint"

  (setq cast-rule (IA-eval-cast-mask direction))
  (setq arc-end (IA-relative-shift arc-start (IA-apply-cast-mask-equal cast-rule corner-size)))
  (command "_.arc"  arc-start
           "e" arc-end
           "_angle" (IA-eval-angle direction))
  arc-end)

(defun my-line (line-start length direction / line-end)
  "Draw a line in x or y direction and return latest coordinate"
  (setq line-end
        (cond
         ((= direction "+y")
          (list (car line-start) (+ (cadr line-start) length)))
         ((= direction "-y")
          (list (car line-start) (- (cadr line-start) length)))
         ((= direction "-x")
          (list (- (car line-start) length) (cadr line-start)))
         (t
          (list (+ (car line-start) length) (cadr line-start)))
         )
        )
  (command "_line" line-start line-end "")
  line-end
  )


(defun IA-unpack-instruction (instruction cross-center / command-to-run)
  "Unpacks a list of declared intructions. Supported are:

(IA-my-corner 'direction') and a global variable *corner-radius* is used
(my-line length 'direction')
"
  (setq command-to-run (car instruction))
  (cond
   ((= command-to-run 'IA-my-corner)
    (progn
      (print "Drawing Corner")
      (apply command-to-run (list
                             cross-center
                             *corner-radius*
                             (cadr instruction)))))
   ((= command-to-run 'my-line)
    (progn
      (print "Drawing LINE")
      (apply command-to-run (list
                             cross-center
                             (eval (cadr instruction))
                             (caddr instruction))))))
  )

(defun IA-cross (cross-center cross-arm cross-width / entity-list ss cepe ss)
  "Draws a closed cross given a center, arm, and cross width
Note that the *corner-radius* is deducted from both"

  ;; 1 -  set corner-radius if not set
  (if (not *corner-radius*)
      (progn
        (print "\n Setting corner radius to 1")
        (setq *corner-radius* 1)
        ))
  (setq
   cross-arm (- cross-arm (* 2 *corner-radius*))
   cross-width (- cross-width (* 2 *corner-radius*))
   cross-center (IA-relative-shift cross-center (list
                                                 (- (/ cross-width 2))
                                                 (- (+ (/ cross-width 2) cross-arm (* 3 *corner-radius*)))
                                                 ))
                                        ; temp parameters for global variables
   cepe (mapcar 'getvar '(cmdecho peditaccept))
                                        ; empty selection set
   ss (ssadd))
  (progn
                                        ; set temp parameters
    (mapcar 'setvar '(cmdecho peditaccept) '(0 1))
                                        ; move from cross center
    (setq instruction-list (list
                            '(IA-my-corner "east-to-north")
                            '(my-line cross-arm "+y")
                            '(IA-my-corner "south-to-west")
                            '(my-line cross-arm "-x")
                            '(IA-my-corner "east-to-north")
                            '(my-line cross-width "+y")
                            '(IA-my-corner "south-to-east")
                            '(my-line cross-arm "+x")
                            '(IA-my-corner "west-to-north")
                            '(my-line cross-arm "+y")
                            '(IA-my-corner "south-to-east")
                            '(my-line cross-width "+x")
                            '(IA-my-corner "west-to-south")
                            '(my-line cross-arm "-y")
                            '(IA-my-corner "north-to-east")
                            '(my-line cross-arm "+x")
                            '(IA-my-corner "west-to-south")
                            '(my-line cross-width "-y")
                            '(IA-my-corner "north-to-west")
                            '(my-line cross-arm "-x")
                            '(IA-my-corner "east-to-south")
                            '(my-line cross-arm "-y")
                            '(IA-my-corner "north-to-west")
                            '(my-line cross-width "-x")
                            ))
    (foreach instruction instruction-list
             ;; Draw line and update last point
             (setq cross-center (IA-unpack-instruction instruction cross-center))
             ;; Add last draw entity to selection set (ss)
             (ssadd (cdr (assoc -1 (entget (entlast)))) ss))
    ;; Join lines in selection set. Black "" are equivalent to pressing enter
    (command "_.pedit" "_multiple" ss "" "_join" "" "")
                                        ; reset variables
    (mapcar 'setvar '(cmdecho peditaccept) cepe)
    )
  (princ)
  )

(defun c:IA-transmon-cross (/ cross-lenth cross-width)
  (if (and
       (setq cross-center (getpoint "\n--> Pick cross center: ")
             cross-arm 50
             cross-width 10
             cross-gap 2
             *corner-radius* 1
             ))
      (progn
        (IA-cross cross-center cross-arm cross-width)
        (IA-cross cross-center cross-arm (- cross-width (* 2 cross-gap)))
        )
    ))
