#lang racket
; Implementación del TDA fecha

; Representacion
; (entero X entero X entero)
; (list dia mes año) 

; CONSTRUCTOR
; Descripción: Permite crear una fecha
; Dominio: entero X entero X entero
; Recorrido: lista
(define (fecha d m a)
  (if (and (integer? d) (integer? m) (integer? a)
           (> d 0) (> m 0) (< m 13) (not (= a 0))
           (<=  d (getDiasDelMes m a)))
      (list d m a)
      null
  )
)

; PERTENENCIA
; Descripción: Función que permite determinar si un elemento cualquiera es del tipo fecha
; se implementa a partir del constructor evaluando el retorno
; Dominio: elemento de cualquier tipo
; Recorrido: boolean
(define (esFecha f)
  (and (list? f) 
       (= (length f) 3)
       (not (null? (fecha (car f) (cadr f) (caddr f)))))
)

; SELECTORES
; Descripción: Función que retorna el día en una fecha
; Dominio: fecha
; Recorrido: entero
(define (getDia f)
   (if (esFecha f)
       (car f)
       0
   )
 )

; Descripción: Función que retorna el mes en una fecha
; Dominio: fecha
; Recorrido: entero
(define (getMes f)
 (if (esFecha f)
       (cadr f)
       0
   )  
)

; Descripción: Función que retorna el año en una fecha
; Dominio: fecha
; Recorrido: entero
(define (getAgno f)
 (if (esFecha f)
       (caddr f)
       0
   )
 )

; Modificadores
; Descripción: Función que crea una nueva fecha a partir de una fecha de entrada reemplazando el valor correspondiente al día
; Dominio: fecha x entero
; Recorrido: fecha
(define (setDia f nd)
  (if (esFecha f)
      (fecha nd (getMes f) (getAgno f))
      null
   )
 )

; Descripción: Función que crea una nueva fecha a partir de una fecha de entrada reemplazando el valor correspondiente al mes
; Dominio: fecha x entero
; Recorrido: fecha
(define (setMes f nm)
  (if (esFecha f)
      (fecha (getDia f) nm (getAgno f))
      null
   )
 )

; Descripción: Función que crea una nueva fecha a partir de una fecha de entrada reemplazando el valor correspondiente al año
; Dominio: fecha x entero
; Recorrido: fecha
(define (setAgno f na)
  (if (esFecha f)
      (fecha (getDia f) (getMes f) na)
      null
   )
 )

; OTRAS FUNCIONES
; Descripción: función que transforma una fecha en string
; Dominio: fecha
; Recorrido: string
(define (fecha->string f)
  (if (esFecha f)
      (string-append (number->string (getDia f)) " de " (getMonthName (getMes f)) " de " (number->string (getAgno f)))
      ""
   )
)

; Descripción: función que retorna el siguiente año para una fecha
; Dominio: fecha
; Recorrido: entero
(define (nextAgno f)
  (if (esFecha f)
      (setAgno f (+ 1 (getAgno f)))
      null
  )
 )

; LAS SIGUIENTES TRES FUNCIONES SON COMPLEMENTARIAS/AUXILIARES AL TDA. NO FORMAN PARTE DEL TDA FECHA, PERO
; EL TDA FECHA LAS EMPLEA PARA PODER REALIZAR SU TRABAJO. ESTAS FUNCIONES DE IGUAL FORMA PUEDEN
; SER EMPLEADAS INDEPENDIENTEMENTE DE LA EXISTENCIA DEL TDA FECHA, POR LO QUE NO EXISTE ACOPLAMIENTO CON EL TDA.
; POR OTRO LADO, EL TDA SEGUN LA IMPLEMENTACION REALIZADA A CONTINUACION, ESTA ACOPLADO A ESTAS FUNCIONES.

; Descripción: función para determinar si un año es bisiesto
; Dominio: entero
; Recorrido: boolean
(define (bisiesto? a)
  (if (and (integer? a) (not (= a 0)))
      (or (= (remainder a 400) 0)
              (and (= (remainder a 4) 0) (not (= (remainder a 100) 0))))
      #f
  )
)

; Descripción: función para determinar los días de un mes
; Dominio: entero X entero
; Recorrido: entero
(define (getDiasDelMes m a)
  (if (and (integer? m) (integer? a) (not (= a 0))
           (> m 0) (< m 13))
           (if (or (= m 1) (= m 3) (= m 5) (= m 7) (= m 8) (= m 10) (= m 12))
                31
                (if (= m 2)
                    (if (bisiesto? a)
                        29
                        28
                    )
                    30
                )
            )
           0
   )
 )

; Descripción: función que transforma un mes entero a su nombre en string
; Dominio: entero
; Recorrido: string
(define (getMonthName m)
      (cond ((not (and (integer? m) (> m 0) (< m 13))) "")
            ((= m 1) "Enero")
            ((= m 2) "Febrero")
            ((= m 3) "Marzo")
            ((= m 4) "Abril")
            ((= m 5) "Mayo")
            ((= m 6) "Junio")
            ((= m 7) "Julio")
            ((= m 8) "Agosto")
            ((= m 9) "Septiembre")
            ((= m 10) "Octubre")
            ((= m 11) "Noviembre")
            ((= m 12) "Diciembre")
       )
  )


(provide (all-defined-out))

