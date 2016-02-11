; M_state_var: implemented for (var ...) calls; (M_state_var '(var name) state) | (M_state_var '(var name <epxression>) state) -> state

(define M_state_var
  (lambda (declare s)
      (add_to_state s (var_name declare) (M_value (declare_experssion declare)))))

; declare_experssion: returns the expression of a declare if one exists from the format '(var name <experssion>)
;    helper for M_state_var
(define declare_experssion caddr)
       
; var_name: returns the name of a var from a declare statement        
;    helper for M_state_var        
(define var_name cadr)

; M_state_assign: implemented for (= ...) calls; (M_state_assign '(= name <expression>) state) -> state

; M_value_assign: implemented for (= ...) calls; (M_value_assign '(= name <expression>) state) -> value

; M_boolean_assign: implemented for (= ...) calls; (M_boolean_assign '(= name <expression>) state) -> bvalue (or error if expression is <numeric>)

; M_value_math: implemented for ({+,-,*,/,%} ...) calls; (M_value_math '(<math_op> <numeric> <numeric>) state) -> nvalue

; M_boolean_logic: implemented for ({&&, ||} ...) calls; (M_value_boolean '(<bool_op> <condition> <condition>) state) -> bvalue

; M_boolean_comparison: implemented for ({<,>,<=,>=,==,!=} ...) calls; (M_boolean_comparison '(<comp_op> <numeric> <numeric>) state) -> bvalue

; M_value_return: implemented for (return ...); note: return does not need an M_state, as the state doesn't matter after return; (M_value_return '(return <expression>) state) -> value

; M_state_if: implemented for (if ...); (M_state_if '(if <condition> <expression>) state) | (M_state_if '(<condition> <expression> <expression>) state) -> state

; M_state_while: implemented for (while ...); (M_state_while '(while <condition> <expression>) state) -> state

; M_state_expression: dispatches M_state to the proper M_state_(expression)

; M_value_expression: dispatches M_value to the proper M_value_(expresion) (error if value is not defined for that expression)

; M_boolean_expressin: dispatches M_boolean to the proper M_boolean_(expression) (error if boolean is not defined for that expression)

; The state: '((var1, var2, ...) (value1, value2, ...))

; M_value_return
(define M_value_return
  (lambda (expression s)
    (M_value (get_operand1 expression) s)))

(define value_dispatch
  (lambda (keyword)
    (cond
      ((eq? keyword 'var) M_value_var)
      ((eq? keyword '=) M_value_assign)
      ((eq? keyword 'return) M_value_return)
      ((eq? keyword 'if) (error 'no_value "If cannot be used as a value"))
      ((eq? keyword 'while) (error 'no_value "While cannot be used as a value"))
      ((or (eq? keyword '+) (eq? keyword '-) (eq? keyword '*) (eq? keyword '/) (eq? keyword '%)) M_value_math)
      (else (error 'keyword "Unknown or unimplemented keyword")))))

(define M_value
  (lambda (expression s)
    (cond
      ((null? expression) (error 'null "You cannot get the value of null"))
      ((number? expression) expression)
      (else ((value_dispatch (get_op expression)) expression s)))))

(define state_dispatch
  (lambda (keyword)
    (cond
      ((eq? keyword 'var) M_state_var)
      ((eq? keyword '=) M_state_assign)
      ((eq? keyword 'return) M_state_return)
      ((eq? keyword 'if) M_state_if)
      ((eq? keyword 'while) M_state_while)
      ((or (eq? keyword '+) (eq? keyword '-) (eq? keyword '*) (eq? keyword '/) (eq? keyword '%)) M_state_math)
      (else (error 'keyword "Unknown or unimplemented keyword")))))

(define M_state
  (lambda (expression s)
    (cond
      ((null? expression) (error 'null "You cannot evaluate null"))
      ((number? expresion) s) ; No change in state from a number
      (else ((state_dispatch (get_op expression)) expression s)))))

(define boolean_dispatch
  (lambda (expression s)
    (cond
      ((eq? keyword 'var) M_boolean_var)
      ((eq? keyword '=) M_boolean_assign)
      ((eq? keyword 'return) M_boolean_return)
      ((eq? keyword 'if) (error 'no_value "If does not have a truthiness"))
      ((eq? keyword 'while) (error 'no_value "While does not have a thruthiness"))
      ((or (eq? keyword '||) (eq? keyword '&&)) M_boolean_exp)
      ((or (eq? keyword '<) (eq? keyword '>) (eq? keyword '<=) (eq? keyword '>=) (eq? keyword '==) (eq? keyword '!=)) M_boolean_compare)
      (else (error 'keyword "Unknown or unimplemented keyword")))))

(define M_boolean
  (lambda (expression s)
    (cond
      ((null? expression) (error 'null "You cannot get the value of null"))
      ((bool? expression) expression)
      (else ((boolean_dispatch (get_op expression)) expression s)))))

(define bool?
  (lambda (b)
    (or (eq? b #t) (eq? b #f))))

(define get_op car)
(define get_operand1 cadr)
(define get_operand2 caddr)

; STATE STUFF
(define get_empty_state
  (lambda ()
    '(() ())))

(define get_first_var caar)
(define get_first_value caadr)
(define get_vars car)
(define get_values cadr)

; makes a state from a vars list and a values list
(define construct_state
  (lambda (vars values)
    (cons vars (cons values '()))))

; checks if the vars or the values are null
(define null_state?
  (lambda (state)
    (or (null? (get_vars state)) (null? (get_values state)))))

; essentially "cdrstate"
(define remove_first_var
  (lambda (state)
    (if (null_state? state)
        (get_empty_state)
        (construct_state (cdr (get_vars state)) (cdr (get_values state))))))

; (add_to_state state var value) -> state; adds a var with value to the state
(define add_to_state
  (lambda (state var value)
    (construct_state (cons var (get_vars state)) (cons value (get_values state)))))

; (remove_from_state state var) -> state; removes a var from the state, does nothing if var is not in the state
(define remove_from_state
  (lambda (state var)
    (cond
      ((null_state? state) '(() ()))
      ((eq? (get_first_var state) var) (construct_state (cdr (get_vars state)) (cdr (get_values state))))
      (else (add_to_state (remove_from_state (remove_first_var state) var) (get_first_var state) (get_first_value state))))))

; (replace_in_state state var value) -> state; replaces var with value in state
(define replace_in_state
  (lambda (state var value)
    (add_to_state (remove_from_state state var) var value)))

; (get_from_state var) -> value; gets the value of var from the state
(define get_from_state
  (lambda (state var)
    (cond
      ((null_state? state) '())
      ((eq? (get_first_var state) var) (get_first_value state))
      (else (get_from_state (remove_first_var state) var)))))
