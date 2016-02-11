(load "simpleParser.scm")

; M_state_var: implemented for (var ...) calls; (M_state_var '(var name) state) | (M_state_var '(var name <epxression>) state) -> state

(define M_state_var
  (lambda (declare s)
    (cond
      ((null? (cddr declare)) (add_to_state s (get_operand1 declare) '()))
      ((in_state? (get_operand1 declare) s) (error 'declare "Cannot declare a var twice"))
      (else (add_to_state (M_state (get_operand2 declare) s) (get_operand1 declare) (M_value (get_operand2 declare) s))))))

; M_value_var: implemented for (var ...) calls; (M_value_var '(var name)) | (M_value_var '(var name <epxression>)) -> value

(define M_value_var
  (lambda (declare s)
    (if (null? (cddr declare))
        '()
        (M_value (get_operand2 declare) s))))

; M_state_assign: implemented for (= ...) calls; (M_state_assign '(= name <expression>) state) -> state
(define M_state_assign
  (lambda (assign s)
    (if (not (in_state? (get_operand1 assign) s))
        (error 'var "Variable not declared")
        (replace_in_state (M_state (get_operand2 assign) s) (get_operand1 assign) (M_value (get_operand2 assign) s)))))

; M_value_assign: implemented for (= ...) calls; (M_value_assign '(= name <expression>) state) -> value
(define M_value_assign
  (lambda (assign s)
    (M_value (get_operand2 assign) s)))

; M_value_exp: 
(define M_value_exp
  (lambda (expression s)
      ((get_exp_op (get_op expression)) (M_value (get_operand1 expression) s) (M_value (get_operand2 expression) (M_state (get_operand1 expression) s)))))

; get_exp_op: returns the functions for any experssion
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
      (else o))))

(define not_equal
  (lambda (a b)
    (not (eq? a b))))

; M_state_exp: the expression itself doesn't change the state, but the values operated on might
(define M_state_exp
  (lambda (expression s)
    (M_state (get_operand2 expression) (M_state (get_operand1 expression) s))))

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

; M_value_return: implemented for (return ...); note: return does not need an M_state, as the state doesn't matter after return; (M_value_return '(return <expression>) state) -> value

(define M_value_return
  (lambda (expression s)
    (M_value (get_operand1 expression) s)))

; M_state_if: implemented for (if ...); (M_state_if '(if <condition> <expression>) state) | (M_state_if '(<condition> <expression> <expression>) state) -> state

(define M_state_if
  (lambda (expression s)
    (if (M_value (get_operand1 expression) s)
        (M_state (get_operand2 expression)
                            (M_state (get_operand1 expression) s))
        (M_state (get_operand3 expression)
                            (M_state (get_operand1 expression) s)))))

; M_state_while: implemented for (while ...); (M_state_while '(while <condition> <expression>) state) -> state
(define M_state_while
  (lambda (expression s)
    (if (M_value (get_operand1 expression) s)
        (M_state_while expression (M_state (get_operand2 expression) (M_state (get_operand1 expression) s)))
        (M_state (get_operand1 expression) s))))

; M_state:
(define M_value
  (lambda (expression s)
    (cond
      ((null? expression) (error 'null "You cannot get the value of null"))
      ((number? expression) expression)
      ((bool? expression) expression)
      ((not (list? expression)) (get_from_state s expression))
      (else ((value_dispatch (get_op expression)) expression s)))))

; M_value
(define M_state
  (lambda (expression s)
    (cond
      ((null? expression) (error 'null "You cannot evaluate null"))
      ((number? expression) s) ; No change in state from a number
      ((bool? expression) s) ; No change in state from a bool
      ((not (list? expression)) s) ; No change in state from accessing a variable
      (else ((state_dispatch (get_op expression)) expression s)))))

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
      ((or (eq? keyword '+) (eq? keyword '-) (eq? keyword '*) (eq? keyword '/) (eq? keyword '%)
           (eq? keyword '<) (eq? keyword '>) (eq? keyword '<=) (eq? keyword '>=) (eq? keyword '==) (eq? keyword '!=)
           (eq? keyword '||) (eq? keyword '&&)) M_value_exp)
      (else (error 'keyword "Unknown or unimplemented keyword")))))

(define state_dispatch
  (lambda (keyword)
    (cond
      ((eq? keyword 'var) M_state_var)
      ((eq? keyword '=) M_state_assign)
      ((eq? keyword 'return) M_state_return)
      ((eq? keyword 'if) M_state_if)
      ((eq? keyword 'while) M_state_while)
      ((or (eq? keyword '+) (eq? keyword '-) (eq? keyword '*) (eq? keyword '/) (eq? keyword '%)
           (eq? keyword '<) (eq? keyword '>) (eq? keyword '<=) (eq? keyword '>=) (eq? keyword '==) (eq? keyword '!=)
           (eq? keyword '||) (eq? keyword '&&)) M_state_exp)
      (else (error 'keyword "Unknown or unimplemented keyword")))))

(define bool?
  (lambda (b)
    (or (eq? b #t) (eq? b #f))))

(define get_op car)
(define get_operand1 cadr)
(define get_operand2 caddr)
(define get_operand3 cadddr)


; STATE STUFF
; The state: '((var1, var2, ...) (value1, value2, ...))
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

; (get_from_state state var) -> value; gets the value of var from the state
(define get_from_state
  (lambda (state var)
    (cond
      ((null_state? state) '())
      ((eq? (get_first_var state) var) (get_first_value state))
      (else (get_from_state (remove_first_var state) var)))))

; (in_state? var s) -> bool; checks if var is a declared variable in s
(define in_state?
  (lambda (var s)
    (cond
      ((null_state? s) #f)
      ((eq? (get_first_var s) var) #t)
      (else (in_state? var (remove_first_var s))))))