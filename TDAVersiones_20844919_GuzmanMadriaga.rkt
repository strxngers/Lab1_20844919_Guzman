#lang racket
; TDAVersiones

; CONSTRUCTOR
; Descripción: construye una versión.
; Tipo de recursión: no utiliza recursión.
; Dominio: id, fecha y contenido.
; Recorrido: versión.
(define(createVersion iD content)
  (list iD content))


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

(provide (all-defined-out))