--------------------------------------------------
CL-INTERPOL - String interpolation for Common Lisp
--------------------------------------------------

CL-INTERPOL is a library for Common Lisp which modifies the reader so
that you can have interpolation within strings similar to Perl or Unix Shell
scripts. It also provides various ways to insert arbitrary characters
into literal strings even if your editor/IDE doesn't support them.
Here's an example:

~~~lisp
* (let ((a 42))
    #?&quot;foo: \xC4\N{Latin capital letter U with diaeresis}\nbar: ${a}&quot;)
&quot;foo: &Auml;&Uuml;
bar: 42&quot;
~~~

CL-INTERPOL comes with a [BSD-style
license](http://www.opensource.org/licenses/bsd-license.php) so you
can basically do with it whatever you want.

Complete documentation for CL-INTERPOL can be found in the `docs`
directory or at [the project documentation
site](http://edicl.github.io/cl-interpol/).
