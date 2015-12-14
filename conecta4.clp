;* IMPORTANTE:
;utilizaremos el hecho (turno j1) o (turno j2) para cambiar los turnos
;utilizaremos el hecho (comprobar-ganador) para verificar si hay algún ganador.


(defglobal ?*fila* = 3)
(defglobal ?*columna* = 3)
(defglobal ?*fichas* = 2)

(deftemplate tablero
  (slot jugador (allowed-values max min))
    (multislot valor
     (type LEXEME)
     (allowed-lexemes x o .)
     (default .)
  )
)



(defrule Estado-Inicial
  ?inicial <- (initial-fact)
 =>
  (printout t "Cuantas columnas quieres?" crlf)
  (bind ?*columna* (read))
  (printout t "Cuantas filas quieres?" crlf)
  (bind ?*fila* (read))
  (printout t "De cuantas fichas conectadas quieres que sea el juego?" crlf)
  (bind ?*fichas* (read))
  
  (bind $?tab (create$))  
  (bind $?filamulti (create$))
  
  (loop-for-count (?j 1 ?*columna*) do
    (bind $?filamulti (insert$ $?filamulti 1 .))
  )
  (bind $?filastrin (implode$ $?filamulti))
    
  (loop-for-count (?i 1 ?*fila*)
    (bind $?tab (insert$ $?tab 1 $?filastrin))
  (bind ?kaka 1)
  )
  (assert (tablero (valor (create$ $?tab))))
  
)

(defrule dibuja-tablero
  ?h<-(mostrar-tablero)
  (tablero (valor $?t))
=>
  (printout t "La situacion del juego es la siguiente:" crlf crlf)
  
  (printout t "         1 ")
  (loop-for-count (?j 2 ?*columna*) do
    (printout t "  "?j" ")
  )
  (printout t crlf)
  (printout t "       |---")
  (loop-for-count (?j 2 ?*columna*) do
    (printout t "+---")
  )
  (printout t "|" crlf)
  (bind ?i 1)
  (bind ?k 1)
  (progn$ (?ald1 $?t)
    (bind $?fila (explode$(nth$ ?i $?t)))
    (printout t "     "?k" |")
    (bind ?k (+ 1 ?k))
    (progn$ (?ald2 $?fila)
      (printout t " "?ald2" |")
    )
    (bind ?i (+ ?i 1))
    (printout t crlf)
    (printout t "       |---")
    (loop-for-count (?j 2 ?*columna*) do
      (printout t "+---")
    )
    (printout t "|" crlf)
  )
  (retract ?h)
  (assert (comprobar-ganador))
  (bind ?mostrar (read))
)

(defrule elige-jugador-para-empezar
  (not (turno ?))
  (tablero (valor $?t))
  =>
  (printout t "Elige quien empieza: (j1/j2)" crlf)
  (assert (turno (read))))

(defrule incorrecta-eleccion-jugador
  ?eleccion <- (turno ~j1&~j2)
  =>
  (retract ?eleccion)
  (printout t "La respuesta no es valida" crlf))

(defrule elige-ficha
  (turno j1|j2) ;|)
  (not (ficha-jugador $?))
  =>
  (printout t "Elige la ficha que usaras: (x/o)" crlf)
  (assert (ficha-jugador (read) j1)))

(defrule incorrecta-eleccion-ficha
  ?eleccion <- (ficha-jugador ~x&~o ?x)
  =>
  (retract ?eleccion)
  (printout t "El tipo de ficha no es valido" crlf))

(defrule correcta-eleccion-j1-x
  ?eleccion <- (ficha-jugador x j1)
  =>
  (assert (ficha-jugador o j2))
  ;(assert (mostrar-tablero))
  )

(defrule correcta-eleccion-j1-o
  ?eleccion <- (ficha-jugador o j1)
  =>
  (assert (ficha-jugador x j2))
  ;(assert (mostrar-tablero))
)