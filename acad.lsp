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

(defun IA-my-line (line-start length direction / line-end)
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


(defun IA-unpack-instruction (instruction last-point / command-to-run)
  "Unpacks a list of declared intructions. Supported are:

(IA-my-corner 'direction') and a global variable *corner-radius* is used
(IA-my-line length 'direction')
"
  (setq command-to-run (car instruction))
  (cond
   ((= command-to-run 'IA-my-corner)
    (progn
      ;; (print "Drawing Corner")
      (apply command-to-run (list
                             last-point
                             *corner-radius*
                             (cadr instruction)))))
   ((= command-to-run 'IA-my-line)
    (progn
      ;; (print "Drawing LINE")
      (apply command-to-run (list
                             last-point
                             (eval (cadr instruction))
                             (caddr instruction))))))
  )

(defun IA-cross (cross-center cross-parameters indent-parameters /
                              cross-arm cross-width top-cross-arm indent-width indent-depth last-point ss )
  "Draws a closed cross with indent on top (used for patches).
A cross-top-arm-correction can be applied to shorten length of top arm.

- cross-center
- cross-parameters (cross-arm-length cross-width cross-top-arm-correction)
- indent-parameters (width depth separation)
- *corner-radius* (global variable) determines smoothing of the cross"
  (print cross-parameters)
  (if (not *corner-radius*)
      (progn
        (print "\n Setting corner radius to 1")
        (setq *corner-radius* 1)
        ))

  (setq
   cross-arm (- (car cross-parameters) (* 2 *corner-radius*))
   cross-width (- (cadr cross-parameters) (* 2 *corner-radius*))
   top-cross-arm (+ cross-arm (caddr cross-parameters))
   indent-width (car indent-parameters)
   indent-depth (cadr indent-parameters)
   last-point (IA-relative-shift cross-center (list
                                               (- (/ cross-width 2))
                                               (- (+ (/ cross-width 2) cross-arm (* 3 *corner-radius*)))
                                               ))
   ss (ssadd))
  (progn
    (setq instruction-list (list
                            '(IA-my-corner "east-to-north")
                            '(IA-my-line cross-arm "+y")
                            '(IA-my-corner "south-to-west")
                            '(IA-my-line cross-arm "-x")
                            '(IA-my-corner "east-to-north")
                            '(IA-my-line cross-width "+y")
                            '(IA-my-corner "south-to-east")
                            '(IA-my-line cross-arm "+x")
                            '(IA-my-corner "west-to-north")
                            '(IA-my-line top-cross-arm "+y")
                            '(IA-my-corner "south-to-east")
                            ;; Create indent
                            '(IA-my-line (/ (- cross-width indent-width) 2) "+x")
                            '(IA-my-line indent-depth "-y")
                            '(IA-my-line indent-width "+x")
                            '(IA-my-line indent-depth "+y")
                            '(IA-my-line (/ (- cross-width indent-width) 2) "+x")
                            '(IA-my-corner "west-to-south")
                            '(IA-my-line top-cross-arm "-y")
                            '(IA-my-corner "north-to-east")
                            '(IA-my-line cross-arm "+x")
                            ;; Finish cross
                            '(IA-my-corner "west-to-south")
                            '(IA-my-line cross-width "-y")
                            '(IA-my-corner "north-to-west")
                            '(IA-my-line cross-arm "-x")
                            '(IA-my-corner "east-to-south")
                            '(IA-my-line cross-arm "-y")
                            '(IA-my-corner "north-to-west")
                            '(IA-my-line cross-width "-x")
                            ))
    (foreach instruction instruction-list
             ;; Draw line and update last point
             (setq last-point (IA-unpack-instruction instruction last-point))
             ;; Add last draw entity to selection set (ss)
             (ssadd (cdr (assoc -1 (entget (entlast)))) ss))
    ;; Join lines in selection set. Black "" are equivalent to pressing enter
    (command "_.pedit" "_multiple" ss "" "_join" "" ""))
  (princ)
  )

(defun IA-centered-rectangle (rectangle-center dimension-x dimension-y / starting-point)
  "Create a rectangle"
  (setq
   p1   (list
         (- (car rectangle-center) (/ dimension-x 2))
         (- (cadr rectangle-center) (/ dimension-y 2)))
   p2 (list
       (+ (car p1) dimension-x)
       (+ (cadr p1) dimension-y))
   )
  (command "_.rectang" p1 p2)
  (princ)
  )

(defun c:IA-transmon-cross (/ cross-center cross-lenth cross-width cross-gap cross-arm ss
                              outer-cross-width inner-patch-center inner-patch-width outer-patch-center outer-patch-width outer-patch-depth
                              )
  (if (and
       (setq
        *corner-radius* 3
        cross-center (getpoint "\n--> Pick cross center: ")
        cross-length (getreal "\n--> Cross length: ")
        cross-width (getreal "\n--> Cross width: ")
        cross-gap (getreal "\n--> Cross gap: ")
        outer-cross-indent '(12 8)
        inner-cross-indent '(10 4)
        jj-gap (+ 8 (cadr outer-cross-indent))
        patch-overlap 1
        inner-patch-depth 16
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Derived parameters
        cross-arm (/ (- cross-length cross-width) 2)
        outer-cross-width (+ cross-width (* 2 cross-gap))
        inner-patch-center (list (car cross-center)
                                 (+ (cadr cross-center) (/ cross-width 2) cross-arm patch-overlap (- (/ inner-patch-depth 2))))
        inner-patch-width (+ cross-width (* 2 patch-overlap))
        outer-patch-center (list (car cross-center)
                                 (+ (cadr cross-center) (/ cross-width 2) cross-arm cross-gap (- jj-gap cross-gap) (- (/ (cadr outer-cross-indent) 2))))
        outer-patch-width (+ (car outer-cross-indent) (* 2 patch-overlap))
        outer-patch-depth (+ (cadr outer-cross-indent) (* 2 patch-overlap))
        ))
      (progn
        ;; Store temporary parameters
        (setq cepe (mapcar 'getvar '(cmdecho peditaccept)))
        (mapcar 'setvar '(cmdecho peditaccept) '(0 1))

        ;; Create patches
        (IA-centered-rectangle inner-patch-center inner-patch-width inner-patch-depth)
        (IA-centered-rectangle outer-patch-center outer-patch-width outer-patch-depth)

        ;; Draw crosses
        (setq ss (ssadd))
        (IA-cross cross-center
                  (list cross-arm outer-cross-width (- jj-gap cross-gap))
                  outer-cross-indent)
        (ssadd (cdr (assoc -1 (entget (entlast)))) ss)
        (IA-cross cross-center
                  (list cross-arm cross-width 0)
                  inner-cross-indent)
        (ssadd (cdr (assoc -1 (entget (entlast)))) ss)
        (command "_.group" "_create" "*" "" ss "")

        ;; Reset variables
        (mapcar 'setvar '(cmdecho peditaccept) cepe)
        (print "Completed cross!")
        )
    ("No parameters entered - skipping!"))
  (princ))
