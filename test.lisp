;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-INTERPOL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-interpol/test.lisp,v 1.9 2004/04/24 00:19:13 edi Exp $

;;; Copyright (c) 2002-2003, Dr. Edmund Weitz. All rights reserved.

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

(in-package #:cl-interpol)

;; Otherwise it's impossible to see which tests are failing for all
;; the "helpful" warnings.
#+sbcl (declaim (optimize (sb-ext:inhibit-warnings 3)))

(defvar *temp*)

(defparameter *cl-interpol-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(defparameter *test-counter* 0)

(defparameter *failure-counter* 0)

(defun test (form1 form2)
  (unless (string= form1 form2)
    (incf *failure-counter*)
    (format t "~&Test ~A failed: Expected ~S but got ~S~%"
            (1+ *test-counter*) form2 form1))
  (incf *test-counter*)
  (princ #\.)
  (when (zerop (mod *test-counter* 10))
    (princ #\Space)
    (princ *test-counter*)
    (terpri))
  (force-output))

(enable-interpol-syntax)

(test #?"abc" "abc")
(test #?'abc' "abc")
(test #?|abc| "abc")
(test #?/abc/ "abc")
(test #?#abc# "abc")
(test #?{abc} "abc")
(test #?(abc) "abc")
(test #?<abc> "abc")
(test #?[abc] "abc")
(test #?"\t\n\r\f\b\a\e"
                 (coerce (list #\Tab #\Newline #\Return #\Page #\Backspace (code-char 7) (code-char 27))
                         'string))
(test #?"\033\x1b\c[\x{1b}"
                 (make-string 4 :initial-element (code-char 27)))
(test #?"\x" (string (code-char 0)))
(test #?"\x001" (format nil "~A1" (code-char 0)))
(test #?"\0001" (format nil "~A1" (code-char 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *temp* (list *long-unicode-names-p*
                     *short-unicode-names-p*
                     *unicode-scripts*)
        *long-unicode-names-p* t))

(test #?"\N{LATIN CAPITAL LETTER A WITH DIAERESIS}" "Ä")
(test #?"\N{latin capital letter a with diaeresis}" "Ä")
(test #?{\N{LATIN CAPITAL LETTER A WITH DIAERESIS}} "Ä")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *short-unicode-names-p* t))

(test #?"\N{Latin:A with Diaeresis}" "Ä")
(test #?"\N{Latin:a with diaeresis}" "ä")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *unicode-scripts* (list "Latin")))

(test #?"\N{A with Diaeresis}" "Ä")
(test #?"\N{a with diaeresis}" "ä")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *long-unicode-names-p* (first *temp*)
        *short-unicode-names-p* (second *temp*)
        *unicode-scripts* (third *temp*)))

(test #?/\1/ "\\1")
(test #?r"\1" "\\1")
(test #?x/abc / "abc")
(test #?x/abc
                         / "abc")
(test #?rx"abc " "abc")
(test #?/[\1]\1/ (format nil "[~A]\\1" (code-char 1)))
(test #?/[(?#foo)](?#foo)/ "[(?#foo)]")
(test #?/a#bc/ "a#bc")
(test #?x/a#bc/ "a")
(test #?x/\d\A[\d\A]/ "\\d\\A[\\dA]")

(test #?"\Q-" "\\-")
(test #?"\Q-\E-" "\\--")
(test #?"\ufoo" "Foo")
(test #?"\Ufoo" "FOO")
(test #?"\Ufoo\Ebar" "FOObar")
(test #?"\Ufoo\LBAR" "FOObar")

(let ((a "foo"))
  (test #?"$" "$")
  (test #?"@ @" "@ @")
  (test #?"${a}bar" "foobar")
  (test #?/${a}bar/ "foobar")
  (test #?"$[a]bar" "foobar")
  (test #?"$(a)bar" "foobar")
  (test #?"$<a>bar" "foobar")
  (test #?/$<a>bar/ "$<a>bar")
  (test #?"$a @a " "$a @a "))

(let ((a (list 1 2 3)))
  (test #?"${a}" "(1 2 3)")
  (test #?"@{a}" "1 2 3")
  (let ((*list-delimiter* ""))
    (test #?"@{a}" "123")))

(let* ((a "foo")
       (b #\Space)
       (c "bar")
       (d (list a b c))
       (x 40))
  (test #?"$ @" "$ @")
  (test #?"$(a)" "foo")
  (test #?"$<a>$[b]" "foo ")
  (test #?"\U${a}\E \u${a}" "FOO Foo")
  (test (let ((cl-interpol:*list-delimiter* #\*))
          #?"@{d}")
        "foo* *bar")
  (test (let ((cl-interpol:*list-delimiter* ""))
          #?"@{d}")
        "foo bar")
  (test #?"The result is ${(let ((y 2)) (+ x y))}"
        "The result is 42")
  (test #?"${#?'${a} ${c}'} ${x}" "foo bar 40"))

(setq cl-interpol:*optional-delimiters-p* t)
(test (let ((% 23)) #?"$%a%b%") "23a%b%")
(test (let ((%a 23)) #?"$%a%b%") "23%b%")
(test (let ((%a% 23)) #?"$%a%b%") "23b%")
(test (let ((%a%b 23)) #?"$%a%b%") "23%")
(test (let ((%a%b% 23)) #?"$%a%b%") "23")
(setq cl-interpol:*optional-delimiters-p* nil)

(load (merge-pathnames "test2.lisp"
                       *cl-interpol-base-directory*))

(disable-interpol-syntax)

(cond ((zerop *failure-counter*)
        (format t "~&All tests passed."))
      (t
        (format t "~&~A tests failed!" *failure-counter*)))