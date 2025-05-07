# sudoku-gen
A program that generates 9x9 or 16x16 Sudoku puzzles.

Using the same generation logic as this program, we created data for TIC-80's [SUDOKU8000](https://tic80.com/play?cart=4203)
## Prerequisites
The prerequisites are JRE (Java Runtime Environment), alloy4.2_2015-02-22.jar downloaded from https://alloytools.org/download.html, and abcl.jar included in abcl-bin-1.9.2, which can be downloaded from https://armedbear.common-lisp.dev.

Place these two JAR files in the same directory as sudoku-gen.lisp.
## Program Execution Command
The following command generates 100 9x9 Sudoku puzzles. The results are output to a file named sudoku-gen-output-yyyy-mm-dd-hh-mi-ss.txt.
```
java -cp alloy4.2_2015-02-22.jar;abcl.jar org.armedbear.lisp.Main --batch --load sudoku-gen.lisp --eval "(in-package :sudoku-gen)" --eval "(sudoku-gen 100)"
```
For 16x16, use the following command. The results are output to a file named sudoku-gen-16x16-output-yyyy-mm-dd-hh-mi-ss.txt.
```
java -cp alloy4.2_2015-02-22.jar;abcl.jar org.armedbear.lisp.Main --batch --load sudoku-gen.lisp --eval "(in-package :sudoku-gen)" --eval "(sudoku-gen-16x16 100)"
```
## Notes
### performance
