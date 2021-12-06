#lang racket

; CONSTRUCTOR
; Descripción: construye un acceso (lista con el usuario y los accesos que tiene  al documento).
; Tipo de recursión: no utiliza recursión.
; Dominio: usuario y caracter tipo string.
; Recorrido: lista con strings.
(define(access user char)
  (list user char))

; PERTENENCIA
(define(isAccess access)
   (if(and (= (length access) 2)
           (string? (car access))
           (char? (cadr access))
           )
      #t
      #f))


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


; Desripción: función que convierte accesos a strings.
; Tipo de recursión: no utiliza recursión.
; Dominio: lista de accesos.
; Recorrido: información de accesos convertida a string.
(define (access->string accesses)
  (if (empty? accesses)
      (list "")
      (map(lambda(access)
            (if (isAccess access)
                (string-append "User: " (getUserAccess access) "\n" "Accesos: " (string(getChar access)) "\n")
                ""
                ))accesses)))

(define e(access->string(list (access "Benja" #\r) (access "Paolo" #\w))))

(provide (all-defined-out))