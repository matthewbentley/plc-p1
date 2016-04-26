; Group Members
; - Matthew Bentley
; - David Lance
; - Alex Tryjankowski

(load "classParser.scm")

(define interpret
  (lambda (name class_name)
    (display_val
     (call/cc
      (lambda (return*)
        (outer_evaluate (parser name) class_name (box (get_empty_environment)) return*))))))

(define default_break (lambda (v) (error "Break outside of loop")))
(define default_continue (lambda (v) (error "continue outide of loop")))
(define default_throw (lambda (v) (error "Error thrown")))
(define default_brace (lambda (v) v))

(define outer_evaluate
  (lambda (program class_name benv return*)
    (cond
      ((null? benv) '())
      ((null? program) (error 'nullprogram "No program was given what is even happening?"))
      (else (M_value_funcall (get_main (get_class_from_classes (create_classes program) class_name)) benv default_break default_continue default_throw return*
                             (create_classes program) class_name '())))))

(define get_main caddr)
(define get_opperand3 cadddr)

; benv: boxed env; env: env
(define evaluate
  (lambda (program benv brace break continue throw return* classes current_class instance)
    (cond
      ((null? benv) '())
      ((null? program) (brace benv))
      (else (evaluate (rest_lines program) (M_state (first_line program) benv break continue throw return* classes current_class instance) brace break continue throw return* classes current_class instance)))))

; first_line: gets the first line of the program from the parsed out list
(define first_line car)
; rest_line: gets the lines after the first of the program from the parsed out list
(define rest_lines cdr)

; M_state:
(define M_state
  (lambda (expression benv break continue throw return* classes current_class instance)
    (cond
      ((null? expression) benv) ; make '() a nop
      ((number? expression) benv) ; No change in state from a number
      ((bool? expression) benv) ; No change in state from a bool
      ((not (list? expression)) benv) ; No change in state from accessing a variable
      (else ((state_dispatch (get_op expression)) expression benv break continue throw return* classes current_class instance)))))

(define expressions
  (lambda ()
    '(+ - * / % < > <= >= == != || && !)))

; state_dispatch: returns the proper state function given the keyword from M_state
(define state_dispatch
  (lambda (keyword)
    (cond
      ((eq? keyword 'break) M_state_break)
      ((eq? keyword 'continue) M_state_continue)
      ((eq? keyword 'begin) M_state_begin)
      ((eq? keyword 'var) M_state_var)
      ((eq? keyword '=) M_state_assign)
      ((eq? keyword 'return) return)
      ((eq? keyword 'if) M_state_if)
      ((eq? keyword 'while) M_state_while)
      ((eq? keyword 'try) M_state_try)
      ((eq? keyword 'catch) M_state_catch)
      ((eq? keyword 'finally) M_state_finally)
      ((eq? keyword 'throw) M_state_throw)
      ((eq? keyword 'funcall) M_state_funcall)
      ((eq? keyword 'function) M_state_function)
      ((eq? keyword 'static-function) M_state_function)
      ((eq? keyword 'new) M_state_nop)
      ((eq? keyword 'dot) M_state_nop)
      ((member keyword (expressions)) M_state_exp)
      (else keyword))))
;      (else (error 'keyword "Unknown or unimplemented keyword")))))

(define M_state_nop
  (lambda (expression benv break continue throw return* classes current_class intance)
    benv)
  )

(define M_state_funcall
  (lambda (expression benv break continue throw return* classes current_class instance)
    (begin
      (M_value_funcall expression benv break continue throw return* classes current_class instance)
      benv)))
; M_value:
(define M_value
  (lambda (expression benv break continue throw return* classes current_class instance)
    (cond
      ((null? expression) (error 'null "You cannot get the value of null"))
      ((number? expression) expression)
      ((bool? expression) expression)
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((not (list? expression)) (get_from_env benv expression))
      (else ((value_dispatch (get_op expression)) expression benv break continue throw return* classes current_class instance)))))

; value_dispatch: returns the proper value function given the keyword from M_value
(define value_dispatch
  (lambda (keyword)
    (cond
      ((eq? keyword 'var) M_value_var)
      ((eq? keyword '=) M_value_assign)
      ((eq? keyword 'return) M_value_return)
      ((eq? keyword 'if) (error 'no_value "If cannot be used as a value"))
      ((eq? keyword 'while) (error 'no_value "While cannot be used as a value"))
      ((eq? keyword 'funcall) M_value_funcall)
      ((eq? keyword 'new) M_value_new)
      ((eq? keyword 'static-function) M_value_funcall)
      ((eq? keyword 'dot) M_value_dot)
      ((member keyword (expressions)) M_value_exp)
      (else (error "Unknown or unimplemented keyword")))))

(define state_from_names_values
  (lambda (names values)
    (if (eq? (length names) (length values))
        (cons (cons names (cons (map box values) '())) '())
        (error 'mismatch "Mismatched parameter length"))))

;(define evaluate
;  (lambda (program benv brace break continue throw return* classes current_class instance)

(define make_closure
  (lambda (benv parameter_names code)
    (lambda (parameter_values _benv break continue throw return* classes current_class instance)
      (call/cc
       (lambda (brace)
         (evaluate code (box (add_state_to_environment (state_from_names_values parameter_names parameter_values) (unbox benv)))
                   brace default_break default_continue throw return* classes current_class instance))))))

;------
; finds this in obj:
; -not in obj, calls find_this on parent
; -parent is '(), error
(define get_field
  (lambda (field obj)
    (cond
      ((eq? '() obj) (error "not instantiated variable/function"))
      (else (if (in_benv? field (cadr obj))
          (get_from_env (cadr obj) field)
          (get_field field (car obj)))))))

(define find_obj
  (lambda (obj benv)
      (if (in_benv? obj benv)
          (get_from_env benv obj))
          (error "class not found")))

(define M_value_dot
  (lambda (expression benv break continue throw return* classes current_class instance)
    (if (eq? (get_operand1 expression) 'this)
        (get_field (get_operand2 expression) instance)
        (if (eq? (car (get_operand1 expression)) 'new)
            (get_field (get_operand2 expression) (M_value (get_operand1 expression) benv break continue throw return* classes current_class instance))
            (get_field (get_operand2 expression) (get_from_env benv (get_operand1 expression)))))))
;------


; M_state_function: state call for _defining_ a function. This should add a binding from an atom to a closure to the environment
;  closure: (cons benv (cons parameters (cons code '())))
;  code is a _function_ in the form (call_me '(parameters) benv break continue throw return*) that returns the value returned
(define M_state_function
  (lambda (expression benv break continue throw return* classes current_class instance)
    (add_to_benv benv (get_operand1 expression) (make_closure benv (get_operand2 expression) (get_operand3 expression)))))

(define get_parameter_list cddr)

; M_value_funcall: probably similar to M_state_begin, but returns a value at the end
;  The replacing functions should take side efffects into account (I hope)
(define M_value_funcall
  (lambda (expression benv break continue throw return* classes current_class instance)
    (call/cc
     (lambda (_return)
       (if (eq? (get_operand1 expression) 'main)
           (evaluate (get_operand3 expression) benv default_brace break continue throw return* classes current_class instance)
           (if (list? (get_operand1 expression))
               (if (eq? (get_operand1 (get_operand1 expression)) 'this)
                   ((M_value (get_operand1 expression) benv break continue throw return* classes current_class instance)
                    (map (lambda (m) (M_value m benv break continue throw return* classes current_class instance)) (get_parameter_list expression))
                    benv break continue throw _return classes current_class instance)
                   (if (eq? (car (get_operand1 (get_operand1 expression))) 'new)
                       ((M_value (get_operand1 expression) benv break continue throw return* classes current_class (M_value (get_operand1 expression) benv break continue throw return* classes current_class instance))
                        (map (lambda (m) (M_value m benv break continue throw return* classes current_class (M_value (get_operand1 expression) benv break continue throw return* classes current_class instance))) (get_parameter_list expression))
                        benv break continue throw _return classes current_class (M_value (get_operand1 expression) benv break continue throw return* classes current_class instance))
                       ((M_value (get_operand1 expression) benv break continue throw return* classes current_class (get_from_env benv (get_operand1 (get_operand1 expression))))
                        (map (lambda (m) (M_value m benv break continue throw return* classes current_class (get_from_env benv (get_operand1 (get_operand1 expression))))) (get_parameter_list expression))
                        benv break continue throw _return classes current_class (get_from_env benv (get_operand1 (get_operand1 expression))))))
               ((get_from_env benv (get_operand1 expression)))))
           (map (lambda (m) (M_value m benv break continue throw return* classes current_class instance)) (get_parameter_list expression))
           benv break continue throw _return classes current_class instance))))

; M_state_begin: implemented for (begin ...) calls; (M_state_begin '(begin <expression>) state) -> state
(define M_state_begin
  (lambda (expression benv break continue throw return* classes current_class instance)
    (call/cc
     (lambda (brace)
       (let ((_begin (lambda (b c)
                       (remove_narrow_scope_benv (evaluate (get_body expression) (add_narrow_scope_benv benv) brace b c throw return* classes current_class instance))))) 
         (_begin (lambda (benv) (brace (break benv))) (lambda (benv) (brace (continue benv)))))))))

(define get_body cdr)

; The finally part of a try/catch/finally block.  Very similar to M_state_begin
(define M_state_finally
  (lambda (expression benv break continue throw return* classes current_class instance)
    (call/cc
     (lambda (brace)
       (evaluate (get_operand1 expression) (construct_state_benv (get_empty_scope) benv) brace break continue throw return* classes current_class instance)))))

; The catch part of try/catch/finally. Very similar to M_state_begin
(define M_state_catch
  (lambda (expression benv break continue throw return* classes current_class instance)
    (call/cc
     (lambda (brace)
       (evaluate (get_operand2 expression) (construct_state_benv (get_empty_scope) benv) brace break continue throw return* classes current_class instance)))))
; M_state_brace: implemented for (begin ...) calls; (M_state_brace '(begin <expression>) state) -> state
(define M_state_brace
  (lambda (expression benv)
    (call/cc
     (lambda (brace)
       (evaluate (cdr expression) (construct_state_benv (get_empty_scope) benv) brace)))))

; M_state_var: implemented for (var ...) calls; (M_state_var '(var name) state) | (M_state_var '(var name <expression>) state) -> state
(define M_state_var
  (lambda (declare benv break continue throw return* classes current_class instance)
    (cond
      ((null? (cddr declare)) (add_to_benv benv (get_operand1 declare) '()))
      ((in_state? (get_operand1 declare) (car (unbox benv))) (error 'declare "Cannot declare a var twice"))
      (else (add_to_benv (M_state (get_operand2 declare) benv break continue throw return* classes current_class instance) (get_operand1 declare) (M_value (get_operand2 declare) benv break continue throw return* classes current_class instance))))))

; M_value_var: implemented for (var ...) calls; (M_value_var '(var name)) | (M_value_var '(var name <expression>)) -> value
(define M_value_var
  (lambda (declare benv)
    (if (null? (cddr declare))
        '()
        (M_value (get_operand2 declare) benv))))

; M_state_assign: implemented for (= ...) calls; (M_state_assign '(= name <expression>) state) -> state
(define M_state_assign
  (lambda (assign benv break continue throw return classes current_class instance*)
    (cond
      ((pair? (get_operand1 assign))
       (if (not (in_benv? (M_value_dot (get_operand1 assign) benv break continue throw return classes current_class instance*) benv))
          (assign_in_object assign benv break continue throw return classes current_class instance*)
          (replace_in_benv benv (M_value_dot (get_operand1 assign) benv break continue throw return classes current_class instance*) (M_value (get_operand2 assign) benv break continue throw return* classes current_class instance))))
  
      (else (if (not (in_benv? (get_operand1 assign) benv))
          (assign_in_object assign benv break continue throw return classes current_class instance*)
          (replace_in_benv benv (get_operand1 assign) (M_value (get_operand2 assign) benv break continue throw return* classes current_class instance)))))))

;(define M_state_assign
;  (lambda (assign benv break continue throw return classes current_class instance*)
;    (cond
;      ((atom? (get_operand1 assign)) (assign_atom assign benv break continue throw return classes current_class instance*))
;      ((and (list? 
;

(define get_benv_from_object cadr)
(define get_parent_from_object car)

(define assign_in_object
  (lambda (assign benv break continue throw return classes current_class instance*)
    (cond
      ((null? current_class) (error "Var not found. You should go home and rethink your life"))
      ((pair? (get_operand1 assign))
       (cond
         ((eq? 'this (get_operand1 (get_operand1 assign)))
             (replace_in_benv (get_benv_from_object instance*)
                              (get_operand2 (get_operand1 assign))
                              (M_value (get_operand2 assign) (if (in_benv? (get_operand2 assign) benv) benv (get_benv_from_object instance*)) break continue throw return classes current_class instance*)))
            ;------
         ((in_benv? (get_benv_from_object instance*) (get_from_env benv (get_operand1 (get_operand1 assign))))
          (replace_in_benv (get_benv_from_object instance*)
                           (get_operand2 (get_operand1 assign))
                           (M_value (get_operand2 assign) (if (in_benv? (get_operand2 assign) benv) benv (get_benv_from_object instance*)) break continue throw return classes current_class instance*)))
         (else (assign_in_object assign benv break continue throw return classes (cadar (get_class_from_classes classes current_class)) (get_parent_from_object instance*)))))
  ;----------- NOT A DOT
      ((in_benv? (get_benv_from_object instance*) (get_operand1 assign)) (replace_in_benv (get_benv_from_object instance*) (get_operand1 assign)
                                                                                          (M_value (get_operand2 assign) (get_benv_from_object instance*) break continue throw return* classes current_class instance)))
      (else (assign_in_object assign benv break continue throw return classes (cadar (get_class_from_classes classes current_class)) (get_parent_from_object instance*))))))

; M_value_assign: implemented for (= ...) calls; (M_value_assign '(= name <expression>) state) -> value
(define M_value_assign
  (lambda (assign benv break continue throw return* classes current_class instance)
    (M_value (get_operand2 assign) benv break continue throw return* classes current_class instance)))

(define M_value_new
  (lambda (name benv break continue throw return* classes current_class instance)
    (instantiate* classes (get_operand1 name))))

; M_value_exp: 
(define M_value_exp
  (lambda (expression benv break continue throw return* classes current_class instance)
    (cond
      ((and (eq? (get_op expression) '-) (null? (cddr expression))) (* -1 (M_value (get_operand1 expression) benv break continue throw return* classes current_class instance)))
      ((eq? (get_op expression) '!) (error_not (M_value (get_operand1 expression) benv break continue throw return* classes current_class instance)))
      (else ((get_exp_op (get_op expression)) (M_value (get_operand1 expression) benv break continue throw return* classes current_class instance) (M_value (get_operand2 expression) (M_state (get_operand1 expression) benv break continue throw return* classes current_class instance) break continue throw return* classes current_class instance))))))

; get_exp_op: returns the functions for any expression
(define get_exp_op
  (lambda (o)
    (cond
      ((eq? '+ o) +)
      ((eq? '- o) -)
      ((eq? '* o) *)
      ((eq? '/ o) quotient)
      ((eq? '% o) remainder)
      ((eq? '< o) <)
      ((eq? '<= o) <=)
      ((eq? '> o) >)
      ((eq? '>= o) >=)
      ((eq? '== o) eq?)
      ((eq? '!= o) not_equal)
      ((eq? '&& o) error_and)
      ((eq? '|| o) error_or)
      ((eq? '! o) error_not)
      (else o))))

(define not_equal
  (lambda (a b)
    (not (eq? a b))))

; M_state_exp: the expression itself doesn't change the state, but the values operated on might
(define M_state_exp
  (lambda (expression benv break continue throw return* classes current_class instance)
    (if (or (and (eq? (get_op expression) '-) (null? (cddr expression))) (eq? (get_op expression) '!))
        (M_state (get_operand1 expression) benv break continue throw return* classes current_class instance)
        (M_state (get_operand2 expression) (M_state (get_operand1 expression) benv break continue throw return* classes current_class instance) break continue throw return* classes current_class instance))))

; error_and: basic error catching and function in case of nonbooleans
(define error_and
  (lambda (e1 e2)
    (if (and (bool? e1) (bool? e2))
        (and e1 e2)
        (error 'type "Operand 1 and 2 for and were not booleans"))))

; error_or: basic error catching or function in case of nonbooleans
(define error_or
  (lambda (e1 e2)
    (if (and (bool? e1) (bool? e2))
        (or e1 e2)
        (error 'type "Operand 1 and 2 for or were not booleans"))))

; error_not: basic error catching not function in case of nonbooleans
(define error_not
  (lambda (e)
    (if (bool? e)
        (not e)
        (error 'type "Not must take a bool"))))

; M_value_return: implemented for (return ...); note: return does not need an M_state, as the state doesn't matter after return; (M_value_return '(return <expression>) state) -> value
(define M_value_return
  (lambda (expression benv return*)
    ((return* (M_value (get_operand1 expression) benv)))))

; M_state_if: implemented for (if ...); (M_state_if '(if <condition> <expression>) state) | (M_state_if '(<condition> <expression> <expression>) state) -> state
(define M_state_if
  (lambda (expression benv break continue throw return* classes current_class instance)
    (cond
      ((M_value (get_operand1 expression) benv break continue throw return* classes current_class instance)
       (M_state (get_operand2 expression)
                (M_state (get_operand1 expression) benv break continue throw return* classes current_class instance) break continue throw return* classes current_class instance))
      ((not (null? (cdddr expression)))
       (M_state (get_operand3 expression)
                (M_state (get_operand1 expression) benv break continue throw return* classes current_class instance) break continue throw return* classes current_class instance))
      (else benv))))

; M_state_while: implemented for (while ...); (M_state_while '(while <condition> <expression>) state) -> state
(define M_state_while
  (lambda (expression benv break continue throw return* classes current_class instance)
    (call/cc
     (lambda (_break)
       (letrec ((loop (lambda (expression benv)
                        (if (M_value (get_operand1 expression) (M_state (get_operand1 expression) benv break continue throw return* classes current_class instance) break continue throw return)
                            (loop expression (remove_narrow_scope_benv (M_state (get_operand2 expression)
                                                                                (M_state (get_operand1 expression) benv _break continue throw return* classes current_class instance)
                                                                                (lambda (benv) (_break benv))
                                                                                (lambda (benv) (_break (loop expression benv)))
                                                                                throw return* classes current_class instance)))
                            (M_state (get_operand1 expression) (M_state (get_operand1 expression) benv break continue throw return* classes current_class instance) break continue throw return* classes current_class instance)))))
         (loop expression benv))))))

; (continue ...) -> jumps to next iteration of innermost loop
(define M_state_continue
  (lambda (expression benv break continue throw return* classes current_class instance)
    (continue (remove_narrow_scope_benv benv))))

; (break ...) -> jumps out of innermost loop
(define M_state_break
  (lambda (expression benv break continue throw return* classes current_class instance)
    (break (remove_narrow_scope_benv benv))))

; Makes the catch continuation.  If there is not catch block, use next catch "up".  If there is, calling this will call the call/cc for getting out of try, plus the whatever's in the catch block
(define M_state_try_catch_helper
  (lambda (expression benv break continue throw_old throw_cc return*)
    (if (null? expression)
        (lambda (_benv v) (throw_old _benv v))
        (lambda (_benv v)
          (throw_cc
           (call/cc
            (lambda (brace)
              (evaluate (get_operand2 expression) (construct_state_benv (scope_with_value 'e v) benv) (lambda (v) (brace (remove_narrow_scope_benv v))) break continue throw_old return*))))))))

; (try ...) -> if no throw is encountered, return (finally (try)) [sort of]. if throw is found, return (finally (catch (try_until_throw))) [sort of]
(define M_state_try
  (lambda (expression benv break continue throw return* classes current_class instance)
    (M_state (get_operand3 expression)
             (remove_narrow_scope_benv (call/cc
                                        (lambda (throw_cc)
                                          (M_state (cons 'begin (get_operand1 expression)) benv break continue (M_state_try_catch_helper (get_operand2 expression) benv break continue throw throw_cc return*) return*))))
             break continue throw return* classes current_class instance)))

; Actually do the throw
(define M_state_throw
  (lambda (expression benv break continue throw return* classes current_class instance)
    (letrec ((e (M_value (get_operand1 expression) benv break continue throw return* classes current_class instance)))
      (throw (remove_narrow_scope_benv benv) e))))

; Pretty printing for true and false
(define display_val
  (lambda (a)
    (cond
      ((eq? a #t) 'true)
      ((eq? a #f) 'false)
      (else a))))

; Actually call return on the value of what is being returned
(define return
  (lambda (expression benv break continue throw return* classes current_class instance)
    (return* (M_value (get_operand1 expression) benv break continue throw return* classes current_class instance))))

(define bool?
  (lambda (b)
    (or (eq? b #t) (eq? b #f))))

(define get_op car)
(define get_operand1 cadr)
(define get_operand2 caddr)
(define get_operand3 cadddr)

; ------------------------ CLASS STUFF ------------------------

(define get_main_from_code
  (lambda (code)
    (cond
      ((null? code) '())
      ((list? (get_next_line code))
       (if (eq? (get_keyword (get_next_line code)) 'static-function)
           (if (eq? (get_second_operand (get_next_line code)) 'main)
               (get_next_line code)
               (get_main_from_code (get_rest_lines code)))
           (get_main_from_code (get_rest_lines code))))
      ((null? (get_rest_lines code)) '())
      (else '()))))


(define get_next_line car)
(define get_keyword car)
(define get_second_operand cadr)
(define get_rest_lines cdr)

(define get_all_but_main
  (lambda (l)
    (cond
      ((null? l) '())
      ((eq? (get_vars l) 'static-function) (rest_vars l))
      (else (cons (get_op l) (get_all_but_main (rest_vars l)))))))

(define get_parent_name
  (lambda (extends)
    (if (null? extends) (cons '()'())
        (cons (get_operand1 extends) '()))))

(define create_class
  (lambda (class_code)
    (cons (cons (get_operand1 class_code) (get_parent_name (get_operand2 class_code)))
          (cons (get_all_but_main (get_operand3 class_code))
                (cons (get_main_from_code (get_operand3 class_code)) '())))))

(define get_class_from_classes
  (lambda (classes name)
    (cond
      ((null? classes) (error "Not a class"))
      ((null? name) '())
      ((eq? name (caaar classes)) (car classes))
      (else (get_class_from_classes (cdr classes) name)))))

(define eval_constructor
  (lambda (program benv return* classes name)
    (cond
      ((null? benv) '())
      ((null? program) benv)
      (else (eval_constructor (rest_lines program) (M_state (first_line program) benv default_break default_continue default_throw return* classes name '()) return* classes name)))))

(define create_object_benv
  (lambda (constructor_code classes name)
    (eval_constructor constructor_code (box (get_empty_environment)) (lambda (v) (error "You can't return in a constructor, silly")) classes name)))

(define instantiate*
  (lambda (classes name)
    (cond
      ((null? classes) '())
      ((null? name) '())
      ((null? (cadar (get_class_from_classes classes name))) (cons '() (cons (create_object_benv (get_operand1 (get_class_from_classes classes name)) classes name) '())))
      (else (cons (instantiate* classes (get_parent_from_class (get_class_from_classes classes name))) (cons '() (cons (create_object_benv (get_operand1 (get_class_from_classes classes name)) classes name) '())))))))
; (instantiate classes (cadar (get_class_from_classes classes name)))
; (cons (create_object_benv (get_operand1 (get_class_from_classes classes name))) '())))))

(define get_parent_from_class cadar)

(define create_classes
  (lambda (code)
    (if (null? code)
        '()
        (cons (create_class (car code)) (create_classes (cdr code))))))

; ------------------------ STATE STUFF ------------------------
; - A scope is the current set of {} that the program is in.  -
; - A state is a the cons of all scopes going from narrowest  -
; -   to widest with each car.                                -
; -------------------------------------------------------------
;
; scope: '((var1 var2 var3 var4 ...) (#&value1 #&value2 #&value3 #&value4 ...))
;  where lower numbered var/value pairs are newer
;  and #&value indicates a box containing 'value
; state: '(scope1 scope2 scope3 ...)
;  where lower numbered scopes are newer
; environment: '(state1 state2 state3 ... globalstate)
;  where lower numbered states are newer, and globalstate is always available

(define get_empty_environment
  (lambda ()
    (cons (get_empty_state) '())))

(define add_state_to_environment
  (lambda (state environment)
    (cons state environment)))

(define pop_state_env cdr)

(define remove_first_state_from_environment cdr)

(define remove_first_state_from_benv
  (lambda (benv)
    (begin
      (set-box! benv
                (cdr (unbox benv)))
      benv)))

(define get_empty_state
  (lambda ()
    (cons (get_empty_scope) '())))

; The scope: '((var1, var2, ...) (value1, value2, ...))'
(define get_empty_scope
  (lambda ()
    '(() ())))

(define empty_scope_state '(()()))

; Almost empty scope: it has one value in it (useful for throws)
(define scope_with_value
  (lambda (e v)
    (cons (cons e '()) (cons (cons (box v) '()) '()))))

; remove_narrow_scope: gets all scopes minus most narrow
(define remove_narrow_scope cdr)
(define remove_narrow_state cdr)
;cdar)
(define remove_narrow_scope_benv
  (lambda (benv)
    (begin
      (set-box! benv
                (cons (remove_narrow_scope (car (unbox benv))) (cdr (unbox benv))))
      benv)))

(define remove_narrow_scope_env
  (lambda (env)
    (cons (remove_narrow_scope (car env)) (cdr env))))

(define add_narrow_scope
  (lambda (state)
    (cons empty_scope_state state)))

(define add_narrow_scope_benv
  (lambda (benv)
    (begin
      (set-box! benv
                (cons (cons empty_scope_state (car (unbox benv))) (cdr (unbox benv))))
      benv)))

(define get_current_scope car)
(define get_first_var caaar)
(define get_first_value
  (lambda (v)
    (unbox (caadar v))))

(define maybe_unbox
  (lambda (b)
    (if (box? b) (unbox b) b)))

(define get_vars caar)
(define get_values cadar)
(define rest_vars cdr)
(define rest_values cdr)
(define get_scope_vars car)
(define get_scope_vals
  (lambda (v)
    (map unbox (cadr v))))
(define get_scope_boxed_vals cadr)

(define eq_scope?
  (lambda (scope1 scope2)
    (and (eq_list? (get_scope_vars scope1) (get_scope_vars scope2))
         (eq_list? (get_scope_vals scope1) (get_scope_vals scope2)))))

(define eq_list?
  (lambda (ls1 ls2)
    (cond
      ((and (null? ls1) (null? ls2)) #t)
      ((null? ls1) #f)
      ((null? ls2) #f)
      ((eq? (car ls1) (car ls2)) (eq_list? (cdr ls1) (cdr ls2)))
      (else #f))))

; construct_scope: makes a scope from a vars list and a values list
(define construct_scope
  (lambda (vars values)
    (cons vars (cons values '()))))

; construct_state: makes a state from a new scope and the state with the wider scopes
(define construct_state
  (lambda (scope state)
    (cons scope state)))

(define construct_state_env
  (lambda (scope env)
    (cons (cons scope (car env)) (cdr env))))

(define construct_state_benv
  (lambda (scope benv)
    (begin
      (set-box! benv
                (construct_state_env scope (unbox benv)))
      benv)))

; null_current_scope?: checks if the vars or the values are null of current scope
(define null_current_scope?
  (lambda (state)
    (cond
      ((null? state) #t)
      ((null? (car state)) #t)
      (else (or (null? (car (car state))) (null? (cdr (car state))))))))

; null_state?: checks if all the scopes are null
(define null_state?
  (lambda (state)
    (cond
      ((null? state) #t)
      (else (and (null_current_scope? state) (null_state? (remove_narrow_scope state)))))))

(define null_env?
  (lambda (env)
    (null? env)))

; remove_first_var: essentially "cdrscope"
(define remove_first_var
  (lambda (state)
    (if (null_current_scope? state)
        (construct_state (get_empty_scope) (remove_narrow_scope state))
        (construct_state
         (construct_scope (rest_vars (get_vars state)) (rest_values (get_values state)))
         (remove_narrow_scope state)))))

; add_to_state: adds a var with value to the narrowest scope in the state
(define add_to_state
  (lambda (state var value)
    (construct_state (construct_scope (cons var (get_vars state)) (cons (box value) (get_values state)))
                     (remove_narrow_scope state))))

(define add_to_env
  (lambda (env var value)
    (cons (add_to_state (car env) var value) (cdr env))))

(define add_to_benv
  (lambda (benv var value)
    (begin
      (set-box! benv
                (add_to_env (unbox benv) var value))
      benv)))

; remove_from_state: removes a var from the state starting with the narrrowest scope, does nothing if var is not in the state
(define remove_from_state
  (lambda (state var)
    (cond
      ((null? state) '())
      ((null_state? state) (get_empty_state))
      ((eq_scope? (get_current_scope state)
                  (get_current_scope (remove_from_scope state var))) (construct_state (get_current_scope state)
                                                                                      (remove_from_state (remove_narrow_scope state) var)))
      (else (remove_from_scope state var)))))

; remove_from_scope: removes a var from a given scope if found
(define remove_from_scope
  (lambda (state var)
    (cond
      ((null_current_scope? state) (construct_state (get_empty_scope) (remove_narrow_scope state)))
      ((eq? (get_first_var state) var) (construct_state (construct_scope (rest_vars (get_vars state))
                                                                         (rest_values (get_values state)))
                                                        (remove_narrow_scope state)))
      (else (add_to_state (remove_from_scope (remove_first_var state) var)
                          (get_first_var state) (get_first_value state))))))

; (replace_in_state state var value) -> state; replaces var with value in state
(define replace_in_state
  (lambda (state var value)
    (cond
      ((null? state) '())
      ((null_state? state) (get_empty_state))
      ((eq_scope? (get_current_scope state)
                  (get_current_scope (remove_from_scope state var))) (construct_state (get_current_scope state)
                                                                                      (replace_in_state (remove_narrow_scope state) var value)))
      (else (begin
              (replace_in_scope (get_scope_vars (get_current_scope state)) (get_scope_boxed_vals (get_current_scope state)) var value)
              state)))))
;      (else (add_to_state (remove_from_scope state var) var value)))))

(define replace_in_scope
  (lambda (vars values var value)
    (if (eq? (car vars) var) (set-box! (car values) value)                      
        (replace_in_scope (cdr vars) (cdr values) var value))))

(define replace_in_env
  (lambda (env var value)
    (cond
      ((null? env) '())
      ((null_env? env) (get_empty_environment))
      ((in_state? var (car env)) (cons (replace_in_state (car env) var value) (cdr env)))
      (else (cons (car env) (replace_in_env (cdr env) var value))))))

(define replace_in_benv
  (lambda (benv var value)
    (begin
      (set-box! benv
                (replace_in_env (unbox benv) var value))
      benv)))

; (get_from_env env var) -> value; gets the value of var from the state
(define get_from_env
  (lambda (benv var)
    (call/cc
     (lambda (found)
       (letrec ((env (unbox benv))
                (fun (lambda (env var found)
                       (cond
                         ((null? env) (error 'var "Undeclared var"))
                         ((null_state? (car env)) (fun (remove_narrow_state env) var found))
                         (else (and (get_from_state (car env) var found) (fun (remove_narrow_state env) var found)))))))
         (fun env var found))))))

;           ((and (eq? (get_first_var state) var) (null? (get_first_value state))) (error 'var "Unassigned variable"))
;          ((eq? (get_first_var state) var) (get_first_value state))
;         (else (get_from_env (remove_first_var state) var))))))))

(define get_from_state
  (lambda (state var found)
    (cond
      ((null_state? state) '())
      ((null_current_scope? state) (get_from_state (remove_narrow_scope state) var found))
      ((and (eq? (get_first_var state) var) (null? (get_first_value state))) (error 'var "Unassigned variable"))
      ((eq? (get_first_var state) var) (found (get_first_value state)))
      (else (get_from_state (remove_first_var state) var found)))))

; (in_state? var s) -> bool; checks if var is a declared variable in s
(define in_state?
  (lambda (var s)
    (cond
      ((null_state? s) #f)
      ((null_current_scope? s) (in_state? var (remove_narrow_scope  s)))
      ((eq? (get_first_var s) var) #t)
      (else (in_state? var (remove_first_var s))))))

(define in_env?
  (lambda (var env)
    (cond
      ((null_env? env) #f)
      (else (or (in_state? var (car env)) (in_env? var (cdr env)))))))

(define in_benv?
  (lambda (var benv)
    (in_env? var (unbox benv))))
