;Alexander Grzesiak

;; load-file is a simple function that loads the definitions from a file
;; It recursives calls itself, reading one line at a time; on the recursive return, it cons'es
;; all of the lines together in a giant list, which it returns to caller.

;; The "port" parameter is a read port that you pass it.  

(define load-file
   (lambda ( port )
      (let ((nextrec (read port)))
         (cond
            ((eof-object? nextrec) '()) ;; If I've read off the end, return empty list
            (else
              (let* ((nascent-db (load-file port))) ;; Recursive call to finish reading file
                 ;; Now add the line read at this level to growing list
                 (cons nextrec nascent-db )))))))

(define classes '())

(define load-classes
  (lambda (filename)
    (let allClassesLoop ((allClasses (load-file (open-input-file filename)))
                         (formattedClass '()))
      (cond
        ;if the current class does not have a parent
        ((and (not (empty? allClasses)) (null? (cdr (caddr (car allClasses)))))
         (begin
           (set! formattedClass (list (cadr (car allClasses))));name
           (set! formattedClass (append formattedClass (list (cdr (caddr (car allClasses))))));parent class name
           (set! formattedClass (append formattedClass (list (cdr (cadddr (car allClasses))))));const args
           (set! formattedClass (append formattedClass (list (cdr (cadr (cdddr (car allClasses)))))));ivars
           (set! formattedClass (append formattedClass (list (cdr (caddr (cdddr (car allClasses)))))));methods
           (set! classes (append classes (list formattedClass)))
           (display "Loaded class: ")
           (display (car formattedClass))
           (newline)
           (allClassesLoop (cdr allClasses) '())))
        ;if the current class has a parent
        ((and (not (empty? allClasses)) (not (null? (cdr (caddr (car allClasses))))))
         (begin
           (set! formattedClass (list (cadr (car allClasses))));name
           (set! formattedClass (append formattedClass (list (cdr (caddr (car allClasses))))));parent class name
           (set! formattedClass (append formattedClass (list (cdr (cadddr (car allClasses))))));const args
           (set! formattedClass (append formattedClass (list (append (cdr (cadr (cdddr (car allClasses))))
                                                                     (cadddr (getClass classes (cadr (caddr (car allClasses)))))))))           
           (set! formattedClass (append formattedClass (list (append (combineMethods (cadr (cdddr (getClass classes (cadr (caddr (car allClasses))))))
                                                                                     (cdr (caddr (cdddr (car allClasses)))))
                                                                     (cdr (caddr (cdddr (car allClasses))))))));methods           
           (set! classes (append classes (list formattedClass)))
           (display "Loaded class: ")
           (display (car formattedClass))
           (newline)
           (allClassesLoop (cdr allClasses) '())))))))

;For methods:
;returns a list of methods that only appear in the parent method.
(define combineMethods
  (lambda (parentMethods childMethods)
    (if (null? parentMethods) '()
        (if (not (null? (combineMethods-helper (car parentMethods) childMethods)))
            (cons (combineMethods-helper (car parentMethods) childMethods) (combineMethods (cdr parentMethods) childMethods))
            (combineMethods (cdr parentMethods) childMethods)))))
;loooks in the child methods for a method with the same name and number of perameters as the parentMethod
;if there is no child method with the same signiture it returns the parent method
;if there is a child method it returns #t
(define combineMethods-helper
  (lambda (parentMethod childMethods)
    (cond ((null? childMethods) parentMethod)
          ((and (equal? (car parentMethod) (caar childMethods))
                (= (length (cadr parentMethod)) (length (cadr (car childMethods)))))
           '())
          ((not (and (equal? (car parentMethod) (caar childMethods))
                (= (length (cadr parentMethod)) (length (cadr (car childMethods))))))
           (combineMethods-helper parentMethod (cdr childMethods))))))

(define getClass
  (lambda (allClasses targetClassName)
    (cond ((null? allClasses) '())
          ((equal? (caar allClasses) targetClassName) (car allClasses))
          (#t (getClass (cdr allClasses) targetClassName)))))

;Goes through the cunstructor arguements and returns the index place of that variable
;Ex: When defining the ivars and the value of one of those ivars corresponds to a
;constructor arguement it go through the constructor arguements and return the index
;of that constructor arguement so the actual value can be found.
;takes in the variable you're trying to find and the list of constructor arguements blueprint
(define getConstArgNum
  (lambda (variable constArgs)
    (let loop ((var variable) (args constArgs) (count 1))
      (if (empty? args)
          (display "bad variable or arg list")
          (begin
            (if (equal? var (car args))
                count
                (loop var (cdr args) (+ count 1))))))))

;Goes through the constructor arguements and returns the actual value in the constructor
;arguement at the corresponding index
;takes in the index you are trying to find and the actual constructor args
(define getConstArgValue
  (lambda (index args)
    (if (= index 1)
        (car args)
        (getConstArgValue (- index 1) (cdr args)))))

;takes in the actual constructor arguements and the class info and spits out a formatted
;version of the ivars to be used after the let
(define format-ivars
  (lambda (constArgs class)
    (if (not (equal? (length constArgs) (length (caddr class))))
        '()
        (let ((unformattedConstArgs (caddr class))) ;blueprint of the constArgs
          (let loop ((unformattedIvars (cadddr class)));actual const args and blueprint for ivars
            (cond ((null? unformattedIvars) '())
                  ;if the value of the ivar is a literal number
                  ((or (number? (cadr (car unformattedIvars))) (not (symbol? (cadr (car unformattedIvars)))))
                   (cons (car unformattedIvars)
                         (loop (cdr unformattedIvars))))
                  ;if the ivar is a variable corresponding to a constructor arguement
                  ((symbol? (cadr (car unformattedIvars)))
                   (cons (list (caar unformattedIvars)
                               `',(getConstArgValue (getConstArgNum (cadr (car unformattedIvars)) unformattedConstArgs) constArgs))
                         (loop (cdr unformattedIvars))))))))))

;formatting ((equal? message (methodName)) format-methodsLET)
;actually formats the cond clause but we are assuming the cond is provided by the new fn
(define format-methods
  (lambda (class)
    (let ((methods (cadr (cdddr class))))
      (let loop ((remainingMethods methods))
        (cond ((null? remainingMethods)
               (list (list 'else '(begin (display "Error: No method with signature:") (display message)))))
              ;no function perameters
              ((null? (cadr (car remainingMethods)))
               (cons (list `(and (equal? message ',(caar remainingMethods)) (= (length args) 0)) (caddr (car remainingMethods)))
                     (loop (cdr remainingMethods))))
              ;has function perameters
              ;use a let statement
              ((not (null? (cadr (car remainingMethods))))
                (cons (list `(and (equal? message ',(caar remainingMethods)) (= (length args) ,(length (cadr (car remainingMethods)))))
                            (format-methodsLET (cadr (car remainingMethods)) (caddr (car remainingMethods))))
                      (loop (cdr remainingMethods)))))))))

;comes up with the let statement that is for the perameters
;takes in what the perameters are called throughout the code as well as the arguements that were passed to the method
;returns the let statement in the form (let ((peram1 arg1) (peram2 arg2)) (code))
(define format-methodsLET
 (lambda (perams code) 
    (list 'let `,(methodsLet-helper perams) `,code)))
;returns the perameters in the form ((peram1 arg1) (peram2 arg2) ... (peramN argN))
(define methodsLet-helper
  (lambda (perams)
    (let ((lengthPerams (length perams)))
      (cond ((= lengthPerams 1) (list (list (car perams) '(car args))))
            ((= lengthPerams 2) (list (list (car perams) '(car args)) (list (cadr perams) '(cadr args))))
            ((= lengthPerams 3) (list (list (car perams) '(car args)) (list (cadr perams) '(cadr args))
                                      (list (caddr perams) '(caddr args))))
            ((= lengthPerams 4) (list (list (car perams) '(car args)) (list (cadr perams) '(cadr args))
                                      (list (caddr perams) '(caddr args)) (list (cadddr perams) '(cadddr args))))))))

(define new
  (lambda (className . constArgs)
    (cond ((null? (getClass classes className)) (begin (display "Error: No class with signature: ") (display className)))
          ((and (not (null? (getClass classes className))) (null? (format-ivars constArgs (getClass classes className))))
           (display "Error: Incorrect number of args"))
          (#t
           (let* ((class (getClass classes className))
                  (code (list 'let*
                              (cons '(*this* '()) (format-ivars constArgs class))
                              (list 'lambda '(message . args) (cons 'cond (append '(((equal? message 'set*this*)
                                                                                     (set! *this* (car args)))
                                                                                    ((equal? message 'get*this*) *this*))
                                                                                  (format-methods class))))))
                  (object (eval code)))
             (object 'set*this* object)
             object
             )))))
