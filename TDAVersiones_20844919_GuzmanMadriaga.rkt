#lang racket
; TDAVersiones

; CONSTRUCTOR
; Descripción: construye una versión.
; Tipo de recursión: no utiliza recursión.
; Dominio: id, fecha y contenido.
; Recorrido: versión.
(define(createVersion iD content)
  (list iD content))

; PERTENENCIA
(define(isVersion version)
   (if(and (= (length version) 2)
           (integer? (car version))
           (string? (cadr version))
           )
      #t
      #f))


; SELECTORES

; Descripción: entrega la id.
; Tipo de recursión: no utiliza recursión.
; Dominio: version
; Recorrido: id.
(define(getIdVersion version)
  (car version))


; Descripción: entrega la fecha de creación de una versión.
; Tipo de recursión: no utiliza recursión.
; Dominio: version
; Recorrido: fecha.
(define(getContentVersion version)
  (cadr version))

; FUNCIONES COMPLEMENTARIAS

; Desripción: función que convierte accesos a strings.
; Tipo de recursión: no utiliza recursión.
; Dominio: lista de accesos.
; Recorrido: información de accesos convertida a string.
(define (versions->string versions decrypt)
  (map(lambda(version)
        (if (isVersion version)
            (string-append  "ID: " (number->string (getIdVersion version)) "\n" "Contenido: " (decrypt (getContentVersion version)) "\n")
            ""
   ))versions))

;(define a(display (versiones->string (createVersion 5 "wena"))))


(provide (all-defined-out))