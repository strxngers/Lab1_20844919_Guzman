#lang racket

; Para este TDA utilizaremos el TDA Fecha
(require "TDAFecha.rkt")

; TDA Documento

; CONSTRUCTOR
; Descripción: crea un documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: fecha tipo entero, nombre del documento tipo string y contenido del documento tipo string.
; Recorrido: lista con enteros y strings.
(define (createDoc fecha nombreDoc contenido)
  (list fecha nombreDoc contenido))

; PERTENENCIA
; Descripción: nos retorna true si corresponde a un documento y en caso contrario retorna false.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: bolleano.
(define(isDoc doc)
 (if(and (= (length doc)3)
           (esFecha (car doc))
           (string? (cadr doc))
           (string? (caddr doc))
           )
      #t
      #f))

(define a(createDoc (fecha 03 11 2021) "a1" "este es mi primer documento"))

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
(define(getNombreDoc doc)
  (if (isDoc doc)
      (cadr doc)
      null))

; Descripción: nos entrega el contenido del documento,
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: contenido del documento tipo string.
(define(getContenido doc)
  (if (isDoc doc)
      (caddr doc)
      null))

; MODIFICADORES

; Descripción: cambia la fecha de creación del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: fecha tipo entero
(define(setFechaDoc doc fecha)
  (list fecha (getNombreDoc doc) (getContenido doc)))

; Descripción: cambia el nombre del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: nombre del documento tipo string.
(define(setNombreDoc doc nombreDoc)
  (list (getFechaDoc doc) nombreDoc (getContenido doc)))

; Descripción: cambia el contenido del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: contenido del documento tipo string.
(define(setContenido doc contenido)
  (list (getFechaDoc doc)(getNombreDoc doc) contenido))

; FUNCIONES COMPLEMENTARIAS

; Descripción:
; Tipo de recursión:
; Dominio:
; Recorrido:

(provide createDoc)
(provide all-defined-out)