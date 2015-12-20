;* IMPORTANTE:
;utilizaremos el hecho (turno j1) o (turno j2) para cambiar los turnos
;utilizaremos el hecho (comprobar-ganador) para verificar si hay algún ganador.


(defglobal ?*fila* = 3)
(defglobal ?*columna* = 3)
(defglobal ?*fichas* = 2)
(defglobal ?*posibles_conexiones* = (create$))

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
  )
  (assert (tablero (valor (create$ $?tab))))
  
  (loop-for-count (?f 1 ?*fila*) do
    (loop-for-count (?c 1 ?*columna*) do
      (if (<= (- ?f 1) (- ?*fila* ?*fichas*)) then
        (bind ?conexion_vertical_arriba (create$))
        (loop-for-count (?n 0 (- ?*fichas* 1)) do
          (bind ?coordenadas (create$ ?c (+ ?f ?n)))
          (bind $?conexion_vertical_arriba (insert$ $?conexion_vertical_arriba (+ (length$ $?conexion_vertical_arriba) 1) ?coordenadas))
        )
        (bind ?*posibles_conexiones* (insert$ ?*posibles_conexiones* (+ (length$ ?*posibles_conexiones*) 1) (implode$ ?conexion_vertical_arriba)))
      )
      (if (<= (- ?c 1) (- ?*columna* ?*fichas*)) then
        (bind ?conexion_horizontal_derecha (create$))
        (loop-for-count (?n 0 (- ?*fichas* 1)) do
          (bind ?coordenadas (create$ (+ ?c ?n) ?f))
          (bind $?conexion_horizontal_derecha (insert$ $?conexion_horizontal_derecha (+ (length$ $?conexion_horizontal_derecha) 1) ?coordenadas))
        )
        (bind ?*posibles_conexiones* (insert$ ?*posibles_conexiones* (+ (length$ ?*posibles_conexiones*) 1) (implode$ ?conexion_horizontal_derecha)))
      )
      (if (and (<= (- ?f 1) (- ?*fila* ?*fichas*)) (<= (- ?c 1) (- ?*columna* ?*fichas*))) then
        (bind ?conexion_diagonal_arriba_derecha (create$))
        (loop-for-count (?n 0 (- ?*fichas* 1)) do
          (bind ?coordenadas (create$ (+ ?c ?n) (+ ?f ?n)))
          (bind $?conexion_diagonal_arriba_derecha (insert$ $?conexion_diagonal_arriba_derecha (+ (length$ $?conexion_diagonal_arriba_derecha) 1) ?coordenadas))
        )
        (bind ?*posibles_conexiones* (insert$ ?*posibles_conexiones* (+ (length$ ?*posibles_conexiones*) 1) (implode$ ?conexion_diagonal_arriba_derecha)))
      )
      (if (and (<= (- ?f 1) (- ?*fila* ?*fichas*)) (>= ?c ?*fichas*)) then
        (bind ?conexion_diagonal_arriba_izquierda (create$))
        (loop-for-count (?n 0 (- ?*fichas* 1)) do
          (bind ?coordenadas (create$ (- ?c ?n) (+ ?f ?n)))
          (bind $?conexion_diagonal_arriba_izquierda (insert$ $?conexion_diagonal_arriba_izquierda (+ (length$ $?conexion_diagonal_arriba_izquierda) 1) ?coordenadas))
        )
        (bind ?*posibles_conexiones* (insert$ ?*posibles_conexiones* (+ (length$ ?*posibles_conexiones*) 1) (implode$ ?conexion_diagonal_arriba_izquierda)))
      )
    )
  )
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

(deffunction heuristico(?valor ?simbolo) ;valor es un array de filas y las filas son srings.
  (bind ?conexiones-x 0)
  (bind ?conexiones-o 0)
  (progn$ (?con ?*posibles_conexiones*)
    (bind ?hayx FALSE)
    (bind ?hayo FALSE)
    (bind ?hayp FALSE)
    (bind ?conexion (explode$ ?con))
    (loop-for-count (?posicion 1 (/ (length$ ?conexion) 2))
      (if (eq (nth$ (nth$ (- (* ?posicion 2) 1) ?conexion) (explode$ (nth$ (nth$ (* ?posicion 2) ?conexion) ?valor))) x) then
        (bind ?hayx TRUE)
      )
      (if (eq (nth$ (nth$ (- (* ?posicion 2) 1) ?conexion) (explode$ (nth$ (nth$ (* ?posicion 2) ?conexion) ?valor))) o) then
        (bind ?hayo TRUE)
      )
      (if (eq (nth$ (nth$ (- (* ?posicion 2) 1) ?conexion) (explode$ (nth$ (nth$ (* ?posicion 2) ?conexion) ?valor))) .) then
        (bind ?hayp TRUE)
      )
    )
    (if ?hayx then
      (if (not ?hayo) then
        (if ?hayp then
          (bind ?conexiones-x (+ ?conexiones-x 1))
        else
          (if (eq ?simbolo x) then
            (return 1000000)
          else
            (return -1000000)
          )
        )
      )
    else
      (if ?hayo then
        (if ?hayp then
          (bind ?conexiones-o (+ ?conexiones-o 1))
        else
          (if (eq ?simbolo x) then
            (return -1000000)
          else
            (return 1000000)
          )
        )
      else
        (bind ?conexiones-x (+ ?conexiones-x 1))
        (bind ?conexiones-o (+ ?conexiones-o 1))
      )
    )
  )
  (if (eq ?simbolo x) then
    (return (- ?conexiones-x ?conexiones-o))
  else
    (return (- ?conexiones-o ?conexiones-x))
  )
)