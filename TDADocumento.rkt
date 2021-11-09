#lang racket

; Para este TDA utilizaremos el TDA Fecha
(require "TDAFecha.rkt")

; TDA Documento

; CONSTRUCTOR
; Descripción: crea un documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: fecha tipo entero, nombre del documento tipo string y contenido del documento tipo string.
; Recorrido: lista con enteros y strings.
(define (createDoc fecha nameDoc content owner)
  (list fecha nameDoc content owner))

; PERTENENCIA
; Descripción: nos retorna true si corresponde a un documento y en caso contrario retorna false.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: bolleano.
(define(isDoc doc)
 (if(and (= (length doc) 4)
           (esFecha (car doc))
           (string? (cadr doc))
           (string? (caddr doc))
           (string? (cadddr doc))
           )
      #t
      #f))

; Pequeño ejemplo para comprobar si funciona.
(define a(createDoc (fecha 03 11 2021) "a1" "este es mi primer documento" "user1"))

; SELECTORES

; Descripción: nos entrega la fecha de creación del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: fecha tipo entero.
(define(getFechaDoc doc)
  (if (isDoc doc)
      (car doc)
      null))

; Descripción: nos entrega el nombre del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: nombre del documento tipo string.
(define(getNameDoc doc)
  (if (isDoc doc)
      (cadr doc)
      null))

; Descripción: nos entrega el contenido del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: contenido del documento tipo string.
(define(getContent doc)
  (if (isDoc doc)
      (caddr doc)
      null))

; Descripción: nos entrega el usuario que creó el documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: usuario que creó el documento tipo string.
(define(getOwner doc)
  (if (isDoc doc)
      (cadddr doc)
      null))


; MODIFICADORES

; Descripción: cambia la fecha de creación del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: fecha tipo entero
(define(setFechaDoc doc fecha)
  (list fecha (getNameDoc doc) (getContent doc) (getOwner doc)))

; Descripción: cambia el nombre del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: nombre del documento tipo string.
(define(setNameDoc doc nameDoc)
  (list (getFechaDoc doc) nameDoc (getContent doc) (getOwner doc)))

; Descripción: cambia el contenido del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: contenido del documento tipo string.
(define(setContent doc content)
  (list (getFechaDoc doc)(getNameDoc doc) content (getOwner doc)))

; Descripción: cambia el usuario que creó el documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: usuario que creó el documento tipo string.
(define(setOwner doc owner)
  (list (getFechaDoc doc)(getNameDoc doc) (getContent doc) owner))


; FUNCIONES COMPLEMENTARIAS

; Descripción:
; Tipo de recursión:
; Dominio:
; Recorrido:

(provide createDoc)
(provide all-defined-out)