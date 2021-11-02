#lang racket

; main

(require "TDAFecha.rkt")
(require "TDAParadigmaDocs.rkt")
(require "TDAUser.rkt")


; FUNCIÓN REGISTER
; con el TDAUser y TDAParadigmaDocs podemos registrar a un usuario
; Descripción: función que registra un usuario.
; Tipo de recursión: no utiliza recursión.
; Dominio: strings y enteros
; Recorrido: lista con strings y enteros
(define(register paradigmadocs fecha username password)
  (setListaUsers paradigmadocs (createUser fecha username password)))


; EJEMPLOS FUNCIÓN REGISTER
(define emptyGDocs(createParadigmaDocs "gDocs" (fecha 25 10 2021) "encryptFn" "encryptFn"))
(define ejemploR1(register emptyGDocs (fecha 27 10 2021) "strxngers" "equisde"))
(define ejemploR2(register emptyGDocs (fecha 01 11 2021) "tanzerin" "blaue"))
(define gDocs1
  (register (register (register emptyGDocs (fecha 25 10 2021) "user1" "pass1")(fecha 25 10 2021) "user2" "pass2") (fecha 25 10 2021) "user3" "pass3"))
; Ejemplo erroneo
(define gDocs2
  (register (register (register emptyGDocs (fecha 25 10 2021) "user1" "pass1")(fecha 25 10 2021) "user1" "pass2") (fecha 25 10 2021) "user3" "pass3"))

; FUNCIÓN LOGIN


