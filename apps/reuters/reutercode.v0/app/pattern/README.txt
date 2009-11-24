* FPattern.cc:	Filter Pattern

[IBM1, Sun, Oracle, IBM2]
WHERE IBM1.price > 58.65
AND   Sun.price < 34.85
AND   Oracle.price < 34.85
AND   IBM2.price > 58.65

---------------------------------------

* PPattern.cc:	Parameterized Pattern

[IBM1, Sun, Oracle, IBM2]
WHERE IBM1.price > Sun.price + 20
Oracle.price < IBM2.price - 20

---------------------------------------

* PPattern2.cc	Parameterized Pattern

[IBM, Sun, Oracle, Google]
WHERE IBM.price > Sun.price + 1
AND   Oracle.price < Google.price - 1

---------------------------------------

* simple.cc	Simple Pattern

[IBM, Sun, Oracle]
WHERE IBM.price > Sun.price + 1 

----------------------------------------

* simple2.cc	Simple Right Pattern

[IBM, Sun, Oracle]
WHERE Sun.price > Oracle.price + 1
