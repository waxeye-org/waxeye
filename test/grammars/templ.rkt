;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(template

 "foo=${bar};"
 (template (string #\f #\o #\o #\=) (code #\b #\a #\r) (string #\;))

 "public static void ${(get-method-name)}() {
    System.out.println(${(get-expr)});
}
"
 (template (string *) (code *) (string *) (code *) (string *))

 "def ${(get-name)}(${(get-args)}):${(i)}
${(get-code)}${(u)}"
 (template (string *) (code *) (string *) (code *) (string *) (code *) (string *) (code *) (code *))
)

(code

 "${}"
 (code)

 "${(foo 1)}"
 (code #\( #\f #\o #\o #\space #\1 #\))

 "${ a + b}"
 (code #\space #\a #\space #\+ #\space #\b)

 "${\\}}"
 (code #\})

 "${\\n}"
 (code #\\ #\n)

 "${$}"
 (code #\$)

 "${${}"
 (code #\$ #\{)

 "${}}"
 fail

 "${{}}"
 fail

 "${${}}"
 fail

 "${\\}"
 fail

 "${${\\}}"
 (code #\$ #\{ #\})
)

(string

 ";oisdcn;aosc;p981y2ep9nC"
 pass

 "$"
 pass

 "${"
 pass

 "$"
 pass

 "\\${}"
 pass

 "${\\}"
 pass

 ""
 fail
)
