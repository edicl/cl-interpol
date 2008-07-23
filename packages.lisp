;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-interpol/packages.lisp,v 1.4 2004/04/24 00:19:13 edi Exp $

;;; Copyright (c) 2003, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:cl-user)

#-:cormanlisp
(defpackage #:cl-interpol
  (:nicknames #:interpol)
  (:use #:cl)
  (:export #:enable-interpol-syntax
           #:disable-interpol-syntax
           #:quote-meta-chars
           #:*list-delimiter*
           #:*long-unicode-names-p*
           #:*short-unicode-names-p*
           #:*unicode-scripts*
           #:*outer-delimiters*
           #:*inner-delimiters*
	   #:*optional-delimiters-p*))

#+:cormanlisp
(defpackage "CL-INTERPOL"
  (:nicknames "INTERPOL")
  (:use "CL")
  (:export "ENABLE-INTERPOL-SYNTAX"
           "DISABLE-INTERPOL-SYNTAX"
           "QUOTE-META-CHARS"
           "*LIST-DELIMITER*"
           "*LONG-UNICODE-NAMES-P*"
           "*SHORT-UNICODE-NAMES-P*"
           "*UNICODE-SCRIPTS*"
           "*OUTER-DELIMITERS*"
           "*INNER-DELIMITERS*"))

(pushnew :cl-interpol *features*)