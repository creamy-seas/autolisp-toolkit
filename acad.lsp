(defun c:retan (/ pl p2 p3 p4)
  (setq pl (getpoint "\nfirst corner of rectangle: "))
  (setq p3 (getcorner "\nsecond corner of rectangle: "))
  (setq p2 (list (car pl)(cadr p3)))
  (setq p4 (list (car p3)(cadr pl)))
  (command "line" pl p2 p3 p4 "c")
  (princ)
  )

(defun c:cross (/ arc-start cross-length gap-size cross-thickness)
  (prompt "-----> Drawing your nazi cross")

  ;; Paramyter read
  (setq arc-start (getpoint "\n--> Pick center point: "))
  (setq cross-length (getreal "\n--> Cross length: "))
  (setq gap-size (getreal "\n--> Gap size: "))
  (if (null gap-size)
      (progn
        (prompt "Default gap size of 24")
        (setq gap-size 24)))
  (setq cross-thickness (getreal "\n--> Cross thickness: "))

  ;; Derive the dimynsions
  (setq inner-cross-long-length (/ (- cross-length cross-thickness) 2))
  (command "line" arc-start (list ()) "c")
                                        ; finishing statemynt always
  (princ (car arc-start))
  (princ)
  )

(defun my-relative-shift (arc-start my-relative-shift)
  "Apply a shift to the starting point to get a second coordinate"
  (list
   (+ (car arc-start) (car my-relative-shift))
   (+ (cadr arc-start) (cadr my-relative-shift)))
  )

(defun my-cast (rule number1 number2)
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

(defun my-cast-equal (rule number)
  "Runs my-cast but with samy number"
  (my-cast rule number number))

(defun my-eval-corner-cast-rule (verbose-form)
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

(defun my-eval-angle (verbose-form)
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

(defun my-corner (arc-start corner-size direction / arc-end)
  "Create cornet starting from arc-start, in the indicated direction and with specified corner size

-> Return coordinate of endpoint"
  (setq cast-rule (my-eval-corner-cast-rule direction))
  (setq arc-end (my-relative-shift arc-start (my-cast-equal cast-rule corner-size)))
  (command "_.arc"  arc-start
           "e" arc-end
           "_angle" (my-eval-angle direction))
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

(defun my-unpack-instruction (instruction last-point / command-to-run)
  (setq command-to-run (car instruction))

  (cond
   ((= command-to-run 'my-corner)
    (progn
      (print "Drawing Corner")
      (print *corner-size*)
      (apply command-to-run (list
                             last-point
                             *corner-size*
                             (cadr instruction)))))
   ((= command-to-run 'my-line)
    (progn
      (print "Drawing LINE")
      (apply command-to-run (list
                             last-point
                             (eval (cadr instruction))
                             (caddr instruction)))))
   ;; ((= command-to-run 'my-line)
   ;;  (print "Match LINE"))
   )
  )
(defun c:my-curve (/ corner-size last-point cross-length gap-size entity-list)
  "Testing platform"
  (if (and (setq last-point (getpoint "\n--> Pick center point: ")
                 ;; cross-length (getreal "\n--> Cross length: ")
                 cross-length 30
                 ;; gap-size (getreal "\n--> Gap size: ")
                 gap-size 20
                 cross-width 5
                 *corner-size* 2
                 ))
      (progn
        (setq instruction-list (list
                                '(my-corner "east-to-north")
                                '(my-line cross-length "+y")
                                '(my-corner "south-to-west")
                                '(my-line cross-length "-x")
                                '(my-corner "east-to-north")
                                '(my-line cross-width "+y")
                                '(my-corner "south-to-east")
                                '(my-line cross-length "+x")
                                '(my-corner "west-to-north")
                                '(my-line cross-length "+y")
                                '(my-corner "south-to-east")
                                '(my-line cross-width "+x")
                                '(my-corner "west-to-south")
                                '(my-line cross-length "-y")
                                '(my-corner "north-to-east")
                                '(my-line cross-length "+x")
                                '(my-corner "west-to-south")
                                '(my-line cross-width "-y")
                                '(my-corner "north-to-west")
                                '(my-line cross-length "-x")
                                '(my-corner "east-to-south")
                                '(my-line cross-length "-y")
                                '(my-corner "north-to-west")
                                ))
        (foreach instruction instruction-list
                 (setq last-point (my-unpack-instruction instruction last-point)))


        ;; (print "First corner")



        ;; (setq last-point (my-corner last-point  "south-to-west" corner-size))
        ;; (setq entity-list (cons (entlast) entity-list))
        ;; (setq last-point (my-line last-point cross-length "-x"))
        ;; (setq entity-list (cons (entlast) entity-list))
        ;; (setq last-point (my-corner last-point  "east-to-north" corner-size))
        ;; (setq entity-list (cons (entlast) entity-list))
        ;; (setq last-point (my-line last-point cross-width "+y"))
        ;; (setq entity-list (cons (entlast) entity-list))
        ;; (print entity-list)
        ;; (setq last-point )


        ;; (my-corner last-point  "east-to-south" corner-size)
        ;; (my-corner last-point  "north-to-east" corner-size)
        ;; (my-corner last-point  "north-to-west" corner-size)
        ;; (my-corner last-point  "east-to-north" corner-size)
        ;; (my-corner last-point  "east-to-north" corner-size)
        )
    (print "No parameters entered - exterminating!")
    )


  ;; (command "line" (list 2 -2) (list 1 1) "")
  (princ)
  )
