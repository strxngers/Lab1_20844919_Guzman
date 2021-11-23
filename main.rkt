#lang racket

; main

(require "TDAFecha.rkt")
(require "TDAParadigmaDocs.rkt")
(require "TDAUser.rkt")
(require "TDADocumento.rkt")
(require "TDAAccess.rkt")


; FUNCIÓN REGISTER
; con el TDAUser y TDAParadigmaDocs podemos registrar a un usuario
; Descripción: Función que registra un usuario.
; Tipo de recursión: una de las funciones a las que se le llama en la función tiene recursión natural.
; Dominio: strings y enteros
; Recorrido: lista con strings y enteros
(define(register paradigmadocs fecha username password)
  (setListaUsers paradigmadocs (createUser fecha username password)))


; FUNCIÓN LOGIN
; Descripción: Función que loguea a un usuario.
; Tipo de Recursión: una de las funciones utilizadas en la función utiliza recursión natural.
; Dominio:
; Recorrido:
(define(login paradigmadocs username password operation)
  (if(and(not(empty? (getListaUsers paradigmadocs)))(userPass (getListaUsers paradigmadocs) username password))
     (operation (setListaUsersActivo paradigmadocs username)) 
     (operation paradigmadocs)))


; por mientras jiji -----------------------------------------------------------------------------------------------------------------------
(define pseudologin (lambda (paradigmadocs username password) (setListaUsersActivo paradigmadocs username)))
; -----------------------------------------------------------------------------------------------------------------------------------------


; FUNCIÓN CREATE
; Descripción: Función que permite a un usuario con sesión iniciada en la plataforma crear un nuevo documento. 
; Tipo de Recursión: no utiliza recursión.
; Dominio: paradigmadocs tipo lista con strings y enteros.
; Recorrido: paradigmadocs tipo lista con strings y enteros.
(define create (lambda (paradigmadocs)
                 (lambda (fecha nameDoc content) paradigmadocs
                   (if (empty? (getListaUserActivo paradigmadocs))
                       paradigmadocs
                       (setDoc paradigmadocs (setOwner (createDoc fecha nameDoc ((getEncryptFunction paradigmadocs) content) ) (car(getListaUserActivo paradigmadocs)))) 
                       ))))    


; FUNCIÓN SHARE
; Descripción: Función que permite compartir un documento con otros usuarios especificando el tipo de acceso a éste
;              (lectura, escritura, comentarios).
; Tipo de Recursión: no utiliza recursión.
; Dominio: paradigmadocs, entero y string.
; Recorrido: paradigmadocs tipo lista con strings y enteros.
(define share (lambda (paradigmadocs)
                (lambda (idDoc access)
                  (if (empty? (getListaUserActivo paradigmadocs))
                      paradigmadocs
                      (setAcc paradigmadocs idDoc (car(getListaUserActivo paradigmadocs)) access)))))



; ----------------------------------------------------------- EJEMPLOS FUNCIÓN REGISTER ----------------------------------------------------------------

(define emptyGDocs(createParadigmaDocs "gDocs" (fecha 25 10 2021) encryptFunction decryptFunction))
(define ejemploR1(register emptyGDocs (fecha 27 10 2021) "userR" "passR"))
(define ejemploR2(register emptyGDocs (fecha 01 11 2021) "user2R" "pass2R"))
(define gDocsR1(register (register (register emptyGDocs (fecha 25 10 2021) "user1" "pass1")(fecha 25 10 2021) "user2" "pass2") (fecha 25 10 2021) "user3" "pass3"))
; Ejemplo en el que hay dos usuarios iguales, entonces solo registra los que son distintos.
(define gDocsR2(register (register (register emptyGDocs (fecha 25 10 2021) "user1" "pass1")(fecha 25 10 2021) "user1" "pass2") (fecha 25 10 2021) "user3" "pass3"))

; ----------------------------------------------------------- EJEMPLOS FUNCIÓN LOGIN ------------------------------------------------------------------

(define ejemploL1((login ejemploR1 "userR" "passR" create)(fecha 22 02 2021) "xd" "ñe"))
(define ejemploL2((login gDocsR1 "user1" "pass1" create) (fecha 30 10 2020) "doc1" "este es mi primer documento"))
(define gDocsL1 ((login gDocsR1 "user1" "pass1" create) (fecha 30 08 2021) "doc0" "contenido doc0"))
(define gDocsL2 ((login gDocsR1 "user2" "pass2" create) (fecha 30 08 2021) "doc1" "contenido doc1"))
; Ejemplo en el que el usuario no está registrado, entonces no lo loguea ni funciona el create.
(define gDocsL3 ((login gDocsR2 "user2" "pass2" create) (fecha 30 08 2021) "doc2" "contenido doc2"))

; ----------------------------------------------------------- EJEMPLOS FUNCIÓN CREATE ------------------------------------------------------------------
(define yay((create ejemploR1) (fecha 19 11 2021) "felicidad" "alegría")) ; primer ejemplo de cuando me finalmente me funcionó create 




; ------------------------------------------------------------ EJEMPLOS FUNCIÓN SHARE ------------------------------------------------------------------
;Funciones de prueba
(define gDocs1 (register (register (register emptyGDocs
                               (fecha 25 10 2021)"user1" "pass1")
                     (fecha 25 10 2021) "user2" "pass2")
           (fecha 25 10 2021) "user3" "pass3"))
(define gDocs2 ((login gDocs1 "user1" "pass1" create) (fecha 30 08 2021) "doc0" "contenido doc0"))
(define gDocs3 ((login gDocs2 "user1" "pass1" create) (fecha 30 08 2021) "doc1" "contenido doc1"))
(define gDocs4 ((login gDocs3 "user2" "pass2" create) (fecha 30 08 2021) "doc2" "contenido doc2"))
(define gDocs5 ((login gDocs4 "user3" "pass3" create) (fecha 30 08 2021) "doc3" "contenido doc3"))
(define gDocs6 ((login gDocs5 "user1" "pass1" share) 1 (access "user2" #\r)))


(define gDocs7 ((login gDocs6 "user2" "pass2" share) 0 (access "user1" #\r)))
(define gDocs8 ((login gDocs7 "user3" "pass3" share) 0 (access "user1" #\c)))
(define gDocs9 ((login gDocs8 "user1" "pass1" add) 0 (fecha 30 11 2021) "mas contenido en doc0"))
(define gDocs10 ((login gDocs9 "user3" "pass3" add) 0 (fecha 30 11 2021) "mas contenido en doc3"))
;(define gDocs11 ((login gDocs10 "user1" "pass1" restoreVersion) 0 0))
;(define gDocs12 (login gDocs11 "user2" "pass2" revokeAllAccesses))

