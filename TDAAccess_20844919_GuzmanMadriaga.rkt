#lang racket

; CONSTRUCTOR
; Descripción: construye un acceso (lista con el usuario y los accesos que tiene  al documento).
; Tipo de recursión: no utiliza recursión.
; Dominio: usuario y caracter tipo string.
; Recorrido: lista con strings.
(define(access user char)
  (list user char))

; SELECTORES

; Descripción: entrega el usuario.
; Tipo de recursión: no utiliza recursión.
; Dominio: lista con strings.
; Recorrido: usuario tipo string.
(define(getUserAccess access)
  (car access))

; Descripción: entrega el/los accesos que tiene el usuario a un documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: lista con strings.
; Recorrido: accesos tipo string.
(define(getChar access)
  (cadr access))

; MODIFICADORES

; Descripción: modifica los accesos que tiene un usuario a un documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: lista con strings y caracter.
; Recorrido: lista con strings.
(define(setChar access char)
  (list (getUserAccess access) char))

(provide (all-defined-out))