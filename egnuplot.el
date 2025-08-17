;;; egnuplot.el --- Gnuplot for Elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/egnuplot
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: utility, plotting

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; egnuplot provides a macro-based interface for generating and
;; executing Gnuplot scripts directly from Emacs Lisp.  With it, you
;; can create, customize, and automate plots using familiar Lisp data
;; structures, functions, and logic.
;;
;; The package’s special forms mirror common Gnuplot commands (such as
;; `set`, `plot`, and `table`), while allowing you to integrate
;; dynamic Elisp data and programmatic control.
;;
;; Example usage:
;;
;;   (egnuplot
;;     (set terminal 'pngcairo :size [800 400])
;;     (set output "myplot.png")
;;     (table $points '((0 0) (1 2) (2 1)))
;;     (plot (curve $points :with 'linespoints)))
;;
;; Use :dry-run t to preview the generated script without executing
;; Gnuplot.
;;
;; See the README for full usage details and options.

;;; Code:

(defun egnuplot--render-value (v)
  (cond
   ((and (consp v) (eq (car v) :gnuplot-raw)) (cadr v))
   ((stringp v) (prin1-to-string v))
   ((keywordp v)  (substring (symbol-name v) 1))
   ((symbolp v) (symbol-name v))
   ((vectorp v)  (if (= 1 (length v))
                     (prin1-to-string v)
                   (mapconcat #'egnuplot--render-value v ",")))
   (t (format "%s" v))))

(defmacro egnuplot (&rest args)
  "Generate and execute a Gnuplot script directly from Emacs Lisp forms.

EGNUPLOT allows you to construct Gnuplot scripts programmatically by
composing Lisp code that describes a plot.  The macro accepts a sequence
of \"special forms\" and raw Lisp values that are translated into
Gnuplot syntax, as well as options for script generation and execution.

Supported special forms include:

  - (set WHAT ...)      : Emit a Gnuplot `set` statement (e.g., terminal, output, etc).
                          If WHAT is 'output, the filename is captured and returned,
                          if gnuplot executes successfully.
  - (unset WHAT)        : Emit an `unset` statement for the given property.
  - (plot ...CURVES...) : Emit a `plot` command with any number of curve definitions.
  - (curve SRC ...)     : Describe a curve for plotting, with data, options, etc.
                         SRC is a table, function, or other Gnuplot curve source.
  - (table $name data)  : Create a Gnuplot data/table block named $name from DATA.
  - (data $name ...)    : Generic table/data block; lines provided as arguments.
  - (defun NAME (ARGS) BODY)
                        : Define a Gnuplot function.
  - (defconst NAME VALUE)
                        : Define a Gnuplot variable or constant.
  - (comment ...)       : Emit a Gnuplot comment (`# ...`).
  - (raw STRING)        : Insert raw Gnuplot syntax at the current location.
  - (reset)             : Emit the `reset` Gnuplot command.

You can mix these special forms with standard Lisp code to generate data
programmatically, parameterize plots, or create reusable plot snippets.

Keyword arguments may be given at the beginning of ARGS:
  :dry-run t -- Do not execute gnuplot, just return the constructed
  script as a string.

Return value:

  - If a `set output ...` was used and gnuplot runs successfully,
    returns the output file name.
  - If `:dry-run` is provided, returns the generated Gnuplot script as a
    string.
  - Otherwise, returns raw stdout/stderr from Gnuplot if no output file
    is specified.

Example usage:

  (egnuplot
    (set terminal 'pngcairo 'size [800 400])
    (set output \"out.png\")
    (table $data '((0 0) (1 1) (2 0)))
    (plot (curve $data 'with 'linespoints 'title \"Triangle\")))

See the README for a full description of how data, tables, curves, and
other forms are mapped from Elisp to Gnuplot."
  (let* ((opts nil)
         (body args))
    (while (and body (keywordp (car body)))
      (push (pop body) opts)
      (push (pop body) opts))
    (setq opts (reverse opts))
    `(let* ((__egnuplot-opts (list ,@opts))
            (__egnuplot-dry-run (plist-get __egnuplot-opts :dry-run))
            __egnuplot-output
            __egnuplot-exit-code
            __egnuplot-proc-output)
       (cl-macrolet ((data (name &rest lines)
                           `(concat
                             ,(symbol-name name) " << EOD\n"
                             (string-join (mapcar (lambda (x) (format "%s" x)) (list ,@lines)) "\n")
                             "\nEOD"))
                     (table (name list)
                            `(data ,name
                                   (mapconcat
                                    (lambda (line) (mapconcat #'prin1-to-string line " "))
                                    ,list "\n")))
                     (curve (src &rest opts)
                            `(concat
                              ,(if (and (symbolp src)
                                        (string-prefix-p "$" (symbol-name src)))
                                   (symbol-name src)
                                 `(egnuplot--render-value ,src))
                              " "
                              (string-join (mapcar #'egnuplot--render-value (list ,@opts)) " ")))
                     (set (thing &rest opts)
                          (let ((name (symbol-name thing)))
                            `(progn
                               (when (equal ,name "output")
                                 (setq __egnuplot-output ,(car opts)))
                               (concat
                                "set "
                                ,name
                                " "
                                (string-join (mapcar #'egnuplot--render-value (list ,@opts)) " ")) )))
                     (unset (key)
                            `(concat "unset " ,(symbol-name thing)))
                     (defun (name args rhs)
                         `(concat ,(symbol-name name)
                                  "("
                                  ,(string-join (mapcar #'symbol-name args) ",")
                                  ") = "
                                  ,rhs))
                     (defconst (name rhs)
                       `(concat ,(symbol-name name)
                                " = "
                                ,rhs)))
         (cl-flet ((raw (s) (list :gnuplot-raw s))
                   (plot (&rest curves)
                         (concat
                          "plot "
                          (mapconcat #'identity curves ",\\\n     ")))
                   (run (&rest args)
                        (mapconcat #'egnuplot--render-value args " "))
                   (reset ()
                          "reset")
                   (comment (&rest args)
                            (apply #'concat "# " args)))
           (let ((script (mapconcat #'identity
                                    (list ,@body)
                                    "\n")))
             (cond
              (__egnuplot-dry-run script)
              (t (with-temp-buffer
                   (insert script)
                   (setq __egnuplot-exit-code
                         (call-process-region (point-min) (point-max)
                                              "gnuplot"
                                              t t nil))
                   (setq __egnuplot-proc-output (buffer-string)))
                 (cond
                  ((and (eq __egnuplot-exit-code 0) __egnuplot-output) __egnuplot-output)
                  ((eq __egnuplot-exit-code 0) __egnuplot-proc-output)
                  (t (error "egnuplot error :: %s" __egnuplot-proc-output)))))))))))

;;;; Footer

(provide 'egnuplot)

;;; egnuplot.el ends here
