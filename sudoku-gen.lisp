;; -*- coding: utf-8; -*-

(defpackage :sudoku-gen
  (:use :common-lisp :threads :java)
  (:export :make-sudoku
	   :make-sudoku-16x16
	   :sudoku-gen
	   :sudoku-gen-16x16
	   :clear-generator))

(in-package :sudoku-gen)

(defparameter *verbose* nil)
(defparameter *workers* nil)
(defvar *solution-generator* nil)
(defvar *16x16-mode* nil)
(defparameter *puzzle-no* nil)
(defparameter *mailbox* (make-mailbox))
(defparameter *mio* (make-mutex) "Mutex for inter-thread mutual exclusion of *standard-output* and *error-output*.")
(defparameter *threshold* 49 "The lower limit of the number of blanks randomly selected. If there is no unique solution (or if *easy* is T, meaning there is no unique solution and it is also not easy), it will be randomly reselected.")
(defparameter *easy* t "If true, generates an easy Sudoku that can be solved using only memo-taking, without backtracking.")
(defparameter *n* 3 "The number of candidates searched to further increase the number of blanks.")

(defun sudoku-gen (n)
  (with-open-file (*standard-output*
		   (format nil "sudoku-gen-output-~a.txt" (tstmp))
		   :direction :output)
    (make-sudoku n)))

(compile 'sudoku-gen)

(defun sudoku-gen-16x16 (n)
  (with-open-file (*standard-output*
		   (format nil "sudoku-gen-16x16-output-~a.txt" (tstmp))
		   :direction :output)
    (make-sudoku-16x16 n)))

(compile 'sudoku-gen-16x16)

(defun clear-generator ()
  (setf *solution-generator* nil))

(compile 'clear-generator)

(defun make-sudoku-16x16 (n)
  (unless *16x16-mode*
    (setf *16x16-mode* t)
    (clear-generator))
  (let ((*world-src* *world-src-16x16*)
	(*groups* *groups-16x16*)
	(*initial-constraints* *initial-constraints-16x16*)
	(*threshold* 128)
	(*easy* t)
	(*n* 3))
    (make-sudoku-aux n)))

(compile 'make-sudoku-16x16)

(defun make-sudoku (n)
  (when *16x16-mode*
    (setf *16x16-mode* nil)
    (clear-generator))
  (make-sudoku-aux n))

(compile 'make-sudoku)

(defun make-sudoku-aux (n)
  (setf *workers* (make-worker-threads (* (available-processors) 1)))
  (format *error-output* "~&~a ~a start (*solution-generator*=~a *easy*=~a *threshold*=~a *n*=~a)"
	  (tstmp-for-log)
	  (if *16x16-mode* "16x16" "9x9")
	  *solution-generator*
	  *easy*
	  *threshold*
	  *n*)
  (let* ((max-queue-size (* (length *workers*) 2)))
    (unless *solution-generator*
      (setf *solution-generator* (make-generator *initial-constraints*)))
    (dotimes (i n)
      (setf *puzzle-no* (1+ i))
      (let ((solution (funcall *solution-generator*)))
	(print-queue-and-thread-status (format nil "enqueue No.~a" *puzzle-no*))
	(limited-max-length-mailbox-send *mailbox* solution max-queue-size)))
    (dotimes (i (length *workers*))
      (limited-max-length-mailbox-send *mailbox* nil max-queue-size))
    (dolist (thread *workers*)
      (thread-join thread)
      (print-queue-and-thread-status (format nil "join ~a" (thread-name thread))))))

(compile 'make-sudoku)

(defun make-worker-threads (n)
  (let ((symbols '(*standard-output*
		   *error-output*
		   *world-src*
		   *groups*
		   *threshold*
		   *easy*
		   *n*))
	(values (list *standard-output*
		      *error-output*
		      *world-src*
		      *groups*
		      *threshold*
		      *easy*
		      *n*)))
    (loop for i from 1 to n collect
	 (make-thread
	  (lambda ()
	    (progv symbols values
	      (loop
		 (let ((constraints (mailbox-read *mailbox*)))
		   (when (null constraints)
		     (print-queue-and-thread-status "finish")
		     (return))
		   (let* ((puzzle81 (encode-board (make-puzzle constraints)))
			  (puzzle81 (shuffle-game puzzle81)))
		     (with-mutex (*mio*)
		       (print puzzle81)
		       (finish-output)))))))
	  :name (format nil "worker~2,'0d" i)))))

(compile 'make-worker-threads)

(defun available-processors ()
  (jcall "availableProcessors" (jstatic "getRuntime" "java.lang.Runtime")))

(compile 'available-processors)

(defun mailbox-queue-size (mailbox &optional non-nil)
  (synchronized-on mailbox
		   (if non-nil
		       (count-if #'identity (threads::mailbox-queue mailbox))
		       (length (threads::mailbox-queue mailbox)))))

(compile 'mailbox-queue-size)

(defun fifo-mailbox-send (mailbox item)
  "Sends an item into the mailbox, notifying 1 waiter
to wake up for retrieval of that object."
  (synchronized-on mailbox
		   (setf (threads::mailbox-queue mailbox)
			 (append (threads::mailbox-queue mailbox) (list item)))
		   (object-notify mailbox)))

(compile 'fifo-mailbox-send)

(defun limited-max-length-mailbox-send (mailbox item max-length)
  (loop while (>= (mailbox-queue-size mailbox) max-length) do
       (sleep 0.1)) ;; Wait if the queue is full.
  (fifo-mailbox-send mailbox item))

(compile 'limited-max-length-mailbox-send)

(defun thread-state (thread)
   (string (jcall "getState" (threads::get-java-thread thread))))

(compile 'thread-state)

(defparameter *world-src* 
  "enum Num {N1,N2,N3,N4,N5,N6,N7,N8,N9}
abstract sig Pos {
	v:one Num
}
one sig P00,P01,P02,P03,P04,P05,P06,P07,P08,
	P10,P11,P12,P13,P14,P15,P16,P17,P18,
	P20,P21,P22,P23,P24,P25,P26,P27,P28,
	P30,P31,P32,P33,P34,P35,P36,P37,P38,
	P40,P41,P42,P43,P44,P45,P46,P47,P48,
	P50,P51,P52,P53,P54,P55,P56,P57,P58,
	P60,P61,P62,P63,P64,P65,P66,P67,P68,
	P70,P71,P72,P73,P74,P75,P76,P77,P78,
	P80,P81,P82,P83,P84,P85,P86,P87,P88 extends Pos {}
pred in_grp(ps:set Pos){
	ps.v = Num
}
pred not_in(va:Num, ps:set Pos){
	Num - va = ps.v
}
fact {
	in_grp[P00+P01+P02+P03+P04+P05+P06+P07+P08]
	in_grp[P10+P11+P12+P13+P14+P15+P16+P17+P18]
	in_grp[P20+P21+P22+P23+P24+P25+P26+P27+P28]
	in_grp[P30+P31+P32+P33+P34+P35+P36+P37+P38]
	in_grp[P40+P41+P42+P43+P44+P45+P46+P47+P48]
	in_grp[P50+P51+P52+P53+P54+P55+P56+P57+P58]
	in_grp[P60+P61+P62+P63+P64+P65+P66+P67+P68]
	in_grp[P70+P71+P72+P73+P74+P75+P76+P77+P78]
	in_grp[P80+P81+P82+P83+P84+P85+P86+P87+P88]

	in_grp[P00+P10+P20+P30+P40+P50+P60+P70+P80]
	in_grp[P01+P11+P21+P31+P41+P51+P61+P71+P81]
	in_grp[P02+P12+P22+P32+P42+P52+P62+P72+P82]
	in_grp[P03+P13+P23+P33+P43+P53+P63+P73+P83]
	in_grp[P04+P14+P24+P34+P44+P54+P64+P74+P84]
	in_grp[P05+P15+P25+P35+P45+P55+P65+P75+P85]
	in_grp[P06+P16+P26+P36+P46+P56+P66+P76+P86]
	in_grp[P07+P17+P27+P37+P47+P57+P67+P77+P87]
	in_grp[P08+P18+P28+P38+P48+P58+P68+P78+P88]

	in_grp[P00+P01+P02+P10+P11+P12+P20+P21+P22]
	in_grp[P03+P04+P05+P13+P14+P15+P23+P24+P25]
	in_grp[P06+P07+P08+P16+P17+P18+P26+P27+P28]
	in_grp[P30+P31+P32+P40+P41+P42+P50+P51+P52]
	in_grp[P33+P34+P35+P43+P44+P45+P53+P54+P55]
	in_grp[P36+P37+P38+P46+P47+P48+P56+P57+P58]
	in_grp[P60+P61+P62+P70+P71+P72+P80+P81+P82]
	in_grp[P63+P64+P65+P73+P74+P75+P83+P84+P85]
	in_grp[P66+P67+P68+P76+P77+P78+P86+P87+P88]
}
")

(defparameter *groups*
  '(("P00" "P01" "P02" "P03" "P04" "P05" "P06" "P07" "P08") ;; r0
    ("P10" "P11" "P12" "P13" "P14" "P15" "P16" "P17" "P18") ;; r1
    ("P20" "P21" "P22" "P23" "P24" "P25" "P26" "P27" "P28") ;; r2
    ("P30" "P31" "P32" "P33" "P34" "P35" "P36" "P37" "P38") ;; r3
    ("P40" "P41" "P42" "P43" "P44" "P45" "P46" "P47" "P48") ;; r4
    ("P50" "P51" "P52" "P53" "P54" "P55" "P56" "P57" "P58") ;; r5
    ("P60" "P61" "P62" "P63" "P64" "P65" "P66" "P67" "P68") ;; r6
    ("P70" "P71" "P72" "P73" "P74" "P75" "P76" "P77" "P78") ;; r7
    ("P80" "P81" "P82" "P83" "P84" "P85" "P86" "P87" "P88") ;; r8
    ("P00" "P10" "P20" "P30" "P40" "P50" "P60" "P70" "P80") ;; c0
    ("P01" "P11" "P21" "P31" "P41" "P51" "P61" "P71" "P81") ;; c1
    ("P02" "P12" "P22" "P32" "P42" "P52" "P62" "P72" "P82") ;; c2
    ("P03" "P13" "P23" "P33" "P43" "P53" "P63" "P73" "P83") ;; c3
    ("P04" "P14" "P24" "P34" "P44" "P54" "P64" "P74" "P84") ;; c4
    ("P05" "P15" "P25" "P35" "P45" "P55" "P65" "P75" "P85") ;; c5
    ("P06" "P16" "P26" "P36" "P46" "P56" "P66" "P76" "P86") ;; c6
    ("P07" "P17" "P27" "P37" "P47" "P57" "P67" "P77" "P87") ;; c7
    ("P08" "P18" "P28" "P38" "P48" "P58" "P68" "P78" "P88") ;; c8
    ("P00" "P01" "P02" "P10" "P11" "P12" "P20" "P21" "P22") ;; b0
    ("P03" "P04" "P05" "P13" "P14" "P15" "P23" "P24" "P25") ;; b1
    ("P06" "P07" "P08" "P16" "P17" "P18" "P26" "P27" "P28") ;; b2
    ("P30" "P31" "P32" "P40" "P41" "P42" "P50" "P51" "P52") ;; b3
    ("P33" "P34" "P35" "P43" "P44" "P45" "P53" "P54" "P55") ;; b4
    ("P36" "P37" "P38" "P46" "P47" "P48" "P56" "P57" "P58") ;; b5
    ("P60" "P61" "P62" "P70" "P71" "P72" "P80" "P81" "P82") ;; b6
    ("P63" "P64" "P65" "P73" "P74" "P75" "P83" "P84" "P85") ;; b7
    ("P66" "P67" "P68" "P76" "P77" "P78" "P86" "P87" "P88") ;; b8
    ))

(defun make-alloy-run-cmd (constraints)
  (format nil "run {~%~{~a~}}"
	  (mapcar (lambda (r-c-v)
		    (let* ((*print-base* 17)
			   (row (first r-c-v))
			   (col (second r-c-v))
			   (val (third r-c-v))
			   (cell-name (format nil "P~a~a" row col))
			   (gs (remove-if-not (lambda (g)
						(member cell-name g :test #'equal))
					      *groups*)))
		      (concatenate 'string
				   (format nil "  P~a~a.v=N~a~%" row col val)
				   (format nil "~{~a~%~}"
					   (mapcar (lambda (g)
						     (format nil
							     "  not_in[N~a,~{~a~^+~}]"
							     val
							     (remove cell-name g :test #'equal)))
						   gs)))))
		  constraints)))

(compile 'make-alloy-run-cmd)

(defun make-generator (&optional constraints)
  "constraints are a list like ((row col val) ....)."
  (let* ((world (jstatic "parseEverything_fromString"
			 "edu.mit.csail.sdg.alloy4compiler.parser.CompUtil"
			 java:+null+
			 (concatenate 'string *world-src*
				      (make-alloy-run-cmd constraints))))
	 (cmds (jcall "getAllCommands" world))
	 (opt (jnew "edu.mit.csail.sdg.alloy4compiler.translator.A4Options"))
	 (dummy (jfield "solver" opt (java:jfield "edu.mit.csail.sdg.alloy4compiler.translator.A4Options$SatSolver" "SAT4J")))
	 (ans (jstatic "execute_command" "edu.mit.csail.sdg.alloy4compiler.translator.TranslateAlloyToKodkod"
		       java:+null+
		       (jcall "getAllReachableSigs" world)
		       (jcall "get" cmds 0)
		       opt)))
    (lambda ()
      (prog1
	  (when (jcall "satisfiable" ans)
	    (let* ((expr (jstatic "parseOneExpression_fromString"
				  "edu.mit.csail.sdg.alloy4compiler.parser.CompUtil"
				  world "v"))
		   (tuple-set (jcall "eval" ans expr))
		   (iter (jcall "iterator" tuple-set)))
	      (do ((result))
		  ((not (jcall "hasNext" iter)) (reverse result))
		(let* ((tuple (jcall "next" iter))
		       (atom-0 (jcall "atom" tuple 0))
		       (atom-1 (jcall "atom" tuple 1)))
		  (push (read-cell-value atom-0 atom-1)
			result)))))
	(setf ans (jcall "next" ans))))))

(compile 'make-generator)

(defun read-cell-value (atom-0 atom-1)
  (let* ((cell-pos-n (parse-integer (subseq atom-0 1) :radix 17 :junk-allowed t))
	 (cell-row-col (multiple-value-list (truncate cell-pos-n 17)))
	 (cell-value (parse-integer (subseq atom-1 1) :radix 17 :junk-allowed t)))
    (list (first cell-row-col) (second cell-row-col) cell-value)))

(compile 'read-cell-value)

(defparameter *initial-constraints*
  '((0 0 1) (0 1 2) (0 2 3)
    (1 0 4) (1 1 5) (1 2 6)
    (2 0 7) (2 1 8) (2 2 9)))

(defparameter *initial-constraints-16x16*
  '((0 0 1) (0 1 2) (0 2 3) (0 3 4)
    (1 0 5) (1 1 6) (1 2 7) (1 3 8)
    (2 0 9) (2 1 10) (2 2 11) (2 3 12)
    (3 0 13) (3 1 14) (3 2 15) (3 3 16)))

(defparameter *world-src-16x16* 
  "enum Num {N1,N2,N3,N4,N5,N6,N7,N8,N9,NA,NB,NC,ND,NE,NF,NG}
abstract sig Pos {
	v:one Num
}
one sig P00,P01,P02,P03,P04,P05,P06,P07,P08,P09,P0A,P0B,P0C,P0D,P0E,P0F,
	P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P1A,P1B,P1C,P1D,P1E,P1F,
	P20,P21,P22,P23,P24,P25,P26,P27,P28,P29,P2A,P2B,P2C,P2D,P2E,P2F,
	P30,P31,P32,P33,P34,P35,P36,P37,P38,P39,P3A,P3B,P3C,P3D,P3E,P3F,
	P40,P41,P42,P43,P44,P45,P46,P47,P48,P49,P4A,P4B,P4C,P4D,P4E,P4F,
	P50,P51,P52,P53,P54,P55,P56,P57,P58,P59,P5A,P5B,P5C,P5D,P5E,P5F,
	P60,P61,P62,P63,P64,P65,P66,P67,P68,P69,P6A,P6B,P6C,P6D,P6E,P6F,
	P70,P71,P72,P73,P74,P75,P76,P77,P78,P79,P7A,P7B,P7C,P7D,P7E,P7F,
	P80,P81,P82,P83,P84,P85,P86,P87,P88,P89,P8A,P8B,P8C,P8D,P8E,P8F,
	P90,P91,P92,P93,P94,P95,P96,P97,P98,P99,P9A,P9B,P9C,P9D,P9E,P9F,
	PA0,PA1,PA2,PA3,PA4,PA5,PA6,PA7,PA8,PA9,PAA,PAB,PAC,PAD,PAE,PAF,
	PB0,PB1,PB2,PB3,PB4,PB5,PB6,PB7,PB8,PB9,PBA,PBB,PBC,PBD,PBE,PBF,
	PC0,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PCA,PCB,PCC,PCD,PCE,PCF,
	PD0,PD1,PD2,PD3,PD4,PD5,PD6,PD7,PD8,PD9,PDA,PDB,PDC,PDD,PDE,PDF,
	PE0,PE1,PE2,PE3,PE4,PE5,PE6,PE7,PE8,PE9,PEA,PEB,PEC,PED,PEE,PEF,
	PF0,PF1,PF2,PF3,PF4,PF5,PF6,PF7,PF8,PF9,PFA,PFB,PFC,PFD,PFE,PFF extends Pos {}
pred in_grp(ps:set Pos){
	ps.v = Num
	and
	no disj p,p':ps | p.v = p'.v
}
pred not_in(va:Num, ps:set Pos){
	Num - va = ps.v
}
fact {
	in_grp[P00+P01+P02+P03+P04+P05+P06+P07+P08+P09+P0A+P0B+P0C+P0D+P0E+P0F]
	in_grp[P10+P11+P12+P13+P14+P15+P16+P17+P18+P19+P1A+P1B+P1C+P1D+P1E+P1F]
	in_grp[P20+P21+P22+P23+P24+P25+P26+P27+P28+P29+P2A+P2B+P2C+P2D+P2E+P2F]
	in_grp[P30+P31+P32+P33+P34+P35+P36+P37+P38+P39+P3A+P3B+P3C+P3D+P3E+P3F]
	in_grp[P40+P41+P42+P43+P44+P45+P46+P47+P48+P49+P4A+P4B+P4C+P4D+P4E+P4F]
	in_grp[P50+P51+P52+P53+P54+P55+P56+P57+P58+P59+P5A+P5B+P5C+P5D+P5E+P5F]
	in_grp[P60+P61+P62+P63+P64+P65+P66+P67+P68+P69+P6A+P6B+P6C+P6D+P6E+P6F]
	in_grp[P70+P71+P72+P73+P74+P75+P76+P77+P78+P79+P7A+P7B+P7C+P7D+P7E+P7F]
	in_grp[P80+P81+P82+P83+P84+P85+P86+P87+P88+P89+P8A+P8B+P8C+P8D+P8E+P8F]
	in_grp[P90+P91+P92+P93+P94+P95+P96+P97+P98+P99+P9A+P9B+P9C+P9D+P9E+P9F]
	in_grp[PA0+PA1+PA2+PA3+PA4+PA5+PA6+PA7+PA8+PA9+PAA+PAB+PAC+PAD+PAE+PAF]
	in_grp[PB0+PB1+PB2+PB3+PB4+PB5+PB6+PB7+PB8+PB9+PBA+PBB+PBC+PBD+PBE+PBF]
	in_grp[PC0+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PCA+PCB+PCC+PCD+PCE+PCF]
	in_grp[PD0+PD1+PD2+PD3+PD4+PD5+PD6+PD7+PD8+PD9+PDA+PDB+PDC+PDD+PDE+PDF]
	in_grp[PE0+PE1+PE2+PE3+PE4+PE5+PE6+PE7+PE8+PE9+PEA+PEB+PEC+PED+PEE+PEF]
	in_grp[PF0+PF1+PF2+PF3+PF4+PF5+PF6+PF7+PF8+PF9+PFA+PFB+PFC+PFD+PFE+PFF]

	in_grp[P00+P10+P20+P30+P40+P50+P60+P70+P80+P90+PA0+PB0+PC0+PD0+PE0+PF0]
	in_grp[P01+P11+P21+P31+P41+P51+P61+P71+P81+P91+PA1+PB1+PC1+PD1+PE1+PF1]
	in_grp[P02+P12+P22+P32+P42+P52+P62+P72+P82+P92+PA2+PB2+PC2+PD2+PE2+PF2]
	in_grp[P03+P13+P23+P33+P43+P53+P63+P73+P83+P93+PA3+PB3+PC3+PD3+PE3+PF3]
	in_grp[P04+P14+P24+P34+P44+P54+P64+P74+P84+P94+PA4+PB4+PC4+PD4+PE4+PF4]
	in_grp[P05+P15+P25+P35+P45+P55+P65+P75+P85+P95+PA5+PB5+PC5+PD5+PE5+PF5]
	in_grp[P06+P16+P26+P36+P46+P56+P66+P76+P86+P96+PA6+PB6+PC6+PD6+PE6+PF6]
	in_grp[P07+P17+P27+P37+P47+P57+P67+P77+P87+P97+PA7+PB7+PC7+PD7+PE7+PF7]
	in_grp[P08+P18+P28+P38+P48+P58+P68+P78+P88+P98+PA8+PB8+PC8+PD8+PE8+PF8]
	in_grp[P09+P19+P29+P39+P49+P59+P69+P79+P89+P99+PA9+PB9+PC9+PD9+PE9+PF9]
	in_grp[P0A+P1A+P2A+P3A+P4A+P5A+P6A+P7A+P8A+P9A+PAA+PBA+PCA+PDA+PEA+PFA]
	in_grp[P0B+P1B+P2B+P3B+P4B+P5B+P6B+P7B+P8B+P9B+PAB+PBB+PCB+PDB+PEB+PFB]
	in_grp[P0C+P1C+P2C+P3C+P4C+P5C+P6C+P7C+P8C+P9C+PAC+PBC+PCC+PDC+PEC+PFC]
	in_grp[P0D+P1D+P2D+P3D+P4D+P5D+P6D+P7D+P8D+P9D+PAD+PBD+PCD+PDD+PED+PFD]
	in_grp[P0E+P1E+P2E+P3E+P4E+P5E+P6E+P7E+P8E+P9E+PAE+PBE+PCE+PDE+PEE+PFE]
	in_grp[P0F+P1F+P2F+P3F+P4F+P5F+P6F+P7F+P8F+P9F+PAF+PBF+PCF+PDF+PEF+PFF]

	in_grp[P00+P01+P02+P03+P10+P11+P12+P13+P20+P21+P22+P23+P30+P31+P32+P33]
	in_grp[P04+P05+P06+P07+P14+P15+P16+P17+P24+P25+P26+P27+P34+P35+P36+P37]
	in_grp[P08+P09+P0A+P0B+P18+P19+P1A+P1B+P28+P29+P2A+P2B+P38+P39+P3A+P3B]
	in_grp[P0C+P0D+P0E+P0F+P1C+P1D+P1E+P1F+P2C+P2D+P2E+P2F+P3C+P3D+P3E+P3F]
	in_grp[P40+P41+P42+P43+P50+P51+P52+P53+P60+P61+P62+P63+P70+P71+P72+P73]
	in_grp[P44+P45+P46+P47+P54+P55+P56+P57+P64+P65+P66+P67+P74+P75+P76+P77]
	in_grp[P48+P49+P4A+P4B+P58+P59+P5A+P5B+P68+P69+P6A+P6B+P78+P79+P7A+P7B]
	in_grp[P4C+P4D+P4E+P4F+P5C+P5D+P5E+P5F+P6C+P6D+P6E+P6F+P7C+P7D+P7E+P7F]
	in_grp[P80+P81+P82+P83+P90+P91+P92+P93+PA0+PA1+PA2+PA3+PB0+PB1+PB2+PB3]
	in_grp[P84+P85+P86+P87+P94+P95+P96+P97+PA4+PA5+PA6+PA7+PB4+PB5+PB6+PB7]
	in_grp[P88+P89+P8A+P8B+P98+P99+P9A+P9B+PA8+PA9+PAA+PAB+PB8+PB9+PBA+PBB]
	in_grp[P8C+P8D+P8E+P8F+P9C+P9D+P9E+P9F+PAC+PAD+PAE+PAF+PBC+PBD+PBE+PBF]
	in_grp[PC0+PC1+PC2+PC3+PD0+PD1+PD2+PD3+PE0+PE1+PE2+PE3+PF0+PF1+PF2+PF3]
	in_grp[PC4+PC5+PC6+PC7+PD4+PD5+PD6+PD7+PE4+PE5+PE6+PE7+PF4+PF5+PF6+PF7]
	in_grp[PC8+PC9+PCA+PCB+PD8+PD9+PDA+PDB+PE8+PE9+PEA+PEB+PF8+PF9+PFA+PFB]
	in_grp[PCC+PCD+PCE+PCF+PDC+PDD+PDE+PDF+PEC+PED+PEE+PEF+PFC+PFD+PFE+PFF]
}
")

(defparameter *groups-16x16*
  '(("P00" "P01" "P02" "P03" "P04" "P05" "P06" "P07" "P08" "P09" "P0A" "P0B" "P0C" "P0D" "P0E" "P0F")
    ("P10" "P11" "P12" "P13" "P14" "P15" "P16" "P17" "P18" "P19" "P1A" "P1B" "P1C" "P1D" "P1E" "P1F")
    ("P20" "P21" "P22" "P23" "P24" "P25" "P26" "P27" "P28" "P29" "P2A" "P2B" "P2C" "P2D" "P2E" "P2F")
    ("P30" "P31" "P32" "P33" "P34" "P35" "P36" "P37" "P38" "P39" "P3A" "P3B" "P3C" "P3D" "P3E" "P3F")
    ("P40" "P41" "P42" "P43" "P44" "P45" "P46" "P47" "P48" "P49" "P4A" "P4B" "P4C" "P4D" "P4E" "P4F")
    ("P50" "P51" "P52" "P53" "P54" "P55" "P56" "P57" "P58" "P59" "P5A" "P5B" "P5C" "P5D" "P5E" "P5F")
    ("P60" "P61" "P62" "P63" "P64" "P65" "P66" "P67" "P68" "P69" "P6A" "P6B" "P6C" "P6D" "P6E" "P6F")
    ("P70" "P71" "P72" "P73" "P74" "P75" "P76" "P77" "P78" "P79" "P7A" "P7B" "P7C" "P7D" "P7E" "P7F")
    ("P80" "P81" "P82" "P83" "P84" "P85" "P86" "P87" "P88" "P89" "P8A" "P8B" "P8C" "P8D" "P8E" "P8F")
    ("P90" "P91" "P92" "P93" "P94" "P95" "P96" "P97" "P98" "P99" "P9A" "P9B" "P9C" "P9D" "P9E" "P9F")
    ("PA0" "PA1" "PA2" "PA3" "PA4" "PA5" "PA6" "PA7" "PA8" "PA9" "PAA" "PAB" "PAC" "PAD" "PAE" "PAF")
    ("PB0" "PB1" "PB2" "PB3" "PB4" "PB5" "PB6" "PB7" "PB8" "PB9" "PBA" "PBB" "PBC" "PBD" "PBE" "PBF")
    ("PC0" "PC1" "PC2" "PC3" "PC4" "PC5" "PC6" "PC7" "PC8" "PC9" "PCA" "PCB" "PCC" "PCD" "PCE" "PCF")
    ("PD0" "PD1" "PD2" "PD3" "PD4" "PD5" "PD6" "PD7" "PD8" "PD9" "PDA" "PDB" "PDC" "PDD" "PDE" "PDF")
    ("PE0" "PE1" "PE2" "PE3" "PE4" "PE5" "PE6" "PE7" "PE8" "PE9" "PEA" "PEB" "PEC" "PED" "PEE" "PEF")
    ("PF0" "PF1" "PF2" "PF3" "PF4" "PF5" "PF6" "PF7" "PF8" "PF9" "PFA" "PFB" "PFC" "PFD" "PFE" "PFF")
    
    ("P00" "P10" "P20" "P30" "P40" "P50" "P60" "P70" "P80" "P90" "PA0" "PB0" "PC0" "PD0" "PE0" "PF0")
    ("P01" "P11" "P21" "P31" "P41" "P51" "P61" "P71" "P81" "P91" "PA1" "PB1" "PC1" "PD1" "PE1" "PF1")
    ("P02" "P12" "P22" "P32" "P42" "P52" "P62" "P72" "P82" "P92" "PA2" "PB2" "PC2" "PD2" "PE2" "PF2")
    ("P03" "P13" "P23" "P33" "P43" "P53" "P63" "P73" "P83" "P93" "PA3" "PB3" "PC3" "PD3" "PE3" "PF3")
    ("P04" "P14" "P24" "P34" "P44" "P54" "P64" "P74" "P84" "P94" "PA4" "PB4" "PC4" "PD4" "PE4" "PF4")
    ("P05" "P15" "P25" "P35" "P45" "P55" "P65" "P75" "P85" "P95" "PA5" "PB5" "PC5" "PD5" "PE5" "PF5")
    ("P06" "P16" "P26" "P36" "P46" "P56" "P66" "P76" "P86" "P96" "PA6" "PB6" "PC6" "PD6" "PE6" "PF6")
    ("P07" "P17" "P27" "P37" "P47" "P57" "P67" "P77" "P87" "P97" "PA7" "PB7" "PC7" "PD7" "PE7" "PF7")
    ("P08" "P18" "P28" "P38" "P48" "P58" "P68" "P78" "P88" "P98" "PA8" "PB8" "PC8" "PD8" "PE8" "PF8")
    ("P09" "P19" "P29" "P39" "P49" "P59" "P69" "P79" "P89" "P99" "PA9" "PB9" "PC9" "PD9" "PE9" "PF9")
    ("P0A" "P1A" "P2A" "P3A" "P4A" "P5A" "P6A" "P7A" "P8A" "P9A" "PAA" "PBA" "PCA" "PDA" "PEA" "PFA")
    ("P0B" "P1B" "P2B" "P3B" "P4B" "P5B" "P6B" "P7B" "P8B" "P9B" "PAB" "PBB" "PCB" "PDB" "PEB" "PFB")
    ("P0C" "P1C" "P2C" "P3C" "P4C" "P5C" "P6C" "P7C" "P8C" "P9C" "PAC" "PBC" "PCC" "PDC" "PEC" "PFC")
    ("P0D" "P1D" "P2D" "P3D" "P4D" "P5D" "P6D" "P7D" "P8D" "P9D" "PAD" "PBD" "PCD" "PDD" "PED" "PFD")
    ("P0E" "P1E" "P2E" "P3E" "P4E" "P5E" "P6E" "P7E" "P8E" "P9E" "PAE" "PBE" "PCE" "PDE" "PEE" "PFE")
    ("P0F" "P1F" "P2F" "P3F" "P4F" "P5F" "P6F" "P7F" "P8F" "P9F" "PAF" "PBF" "PCF" "PDF" "PEF" "PFF")
    
    ("P00" "P01" "P02" "P03" "P10" "P11" "P12" "P13" "P20" "P21" "P22" "P23" "P30" "P31" "P32" "P33")
    ("P04" "P05" "P06" "P07" "P14" "P15" "P16" "P17" "P24" "P25" "P26" "P27" "P34" "P35" "P36" "P37")
    ("P08" "P09" "P0A" "P0B" "P18" "P19" "P1A" "P1B" "P28" "P29" "P2A" "P2B" "P38" "P39" "P3A" "P3B")
    ("P0C" "P0D" "P0E" "P0F" "P1C" "P1D" "P1E" "P1F" "P2C" "P2D" "P2E" "P2F" "P3C" "P3D" "P3E" "P3F")
    ("P40" "P41" "P42" "P43" "P50" "P51" "P52" "P53" "P60" "P61" "P62" "P63" "P70" "P71" "P72" "P73")
    ("P44" "P45" "P46" "P47" "P54" "P55" "P56" "P57" "P64" "P65" "P66" "P67" "P74" "P75" "P76" "P77")
    ("P48" "P49" "P4A" "P4B" "P58" "P59" "P5A" "P5B" "P68" "P69" "P6A" "P6B" "P78" "P79" "P7A" "P7B")
    ("P4C" "P4D" "P4E" "P4F" "P5C" "P5D" "P5E" "P5F" "P6C" "P6D" "P6E" "P6F" "P7C" "P7D" "P7E" "P7F")
    ("P80" "P81" "P82" "P83" "P90" "P91" "P92" "P93" "PA0" "PA1" "PA2" "PA3" "PB0" "PB1" "PB2" "PB3")
    ("P84" "P85" "P86" "P87" "P94" "P95" "P96" "P97" "PA4" "PA5" "PA6" "PA7" "PB4" "PB5" "PB6" "PB7")
    ("P88" "P89" "P8A" "P8B" "P98" "P99" "P9A" "P9B" "PA8" "PA9" "PAA" "PAB" "PB8" "PB9" "PBA" "PBB")
    ("P8C" "P8D" "P8E" "P8F" "P9C" "P9D" "P9E" "P9F" "PAC" "PAD" "PAE" "PAF" "PBC" "PBD" "PBE" "PBF")
    ("PC0" "PC1" "PC2" "PC3" "PD0" "PD1" "PD2" "PD3" "PE0" "PE1" "PE2" "PE3" "PF0" "PF1" "PF2" "PF3")
    ("PC4" "PC5" "PC6" "PC7" "PD4" "PD5" "PD6" "PD7" "PE4" "PE5" "PE6" "PE7" "PF4" "PF5" "PF6" "PF7")
    ("PC8" "PC9" "PCA" "PCB" "PD8" "PD9" "PDA" "PDB" "PE8" "PE9" "PEA" "PEB" "PF8" "PF9" "PFA" "PFB")
    ("PCC" "PCD" "PCE" "PCF" "PDC" "PDD" "PDE" "PDF" "PEC" "PED" "PEE" "PEF" "PFC" "PFD" "PFE" "PFF")
))

(defun print-queue-and-thread-status (&optional message)
  (with-mutex (*mio*)
    (format *error-output*
	    "~&~a puzzle=~a q=~a ~{~a~}"
	    (tstmp-for-log)
	    *puzzle-no*
	    (mailbox-queue-size *mailbox* t)
	    (mapcar (lambda (thread)
		      (char (thread-state thread) 0))
		    *workers*))
    (when message
      (format *error-output* " ~a ~a" (thread-name (threads:current-thread)) message))))

(compile 'print-queue-and-thread-status)

(defun make-puzzle (board)
  (block outer
    (loop
       (block inner
	 (let* ((init-constraints (shuffle board))
		(ubound (length init-constraints))
		(lbound *threshold*))
	   (loop
	      (let* ((num-empty-cells (floor (+ ubound lbound) 2))
		     (constraints (nthcdr num-empty-cells init-constraints)))
		(when *verbose*
		  (print-queue-and-thread-status (format nil "checking ~a ~a ~a" lbound num-empty-cells ubound)))
		(when (<= ubound *threshold*)
		  (return-from inner))
		(let ((b3 (not (easy-constraints-check constraints))))
		  (if b3
		      (setf ubound num-empty-cells)
		      (if (not (uniq-constraints constraints))
			  (setf ubound num-empty-cells)
			  (progn
			    (setf lbound num-empty-cells)
			    (when (= (- ubound lbound) 1)
			      (return-from outer (car (make-it-harder (list constraints))))))))))))))))

(compile 'make-puzzle)

(defun encode-board (constraints)
  (let* ((*print-base* 17)
	 (dim (if *16x16-mode* 16 9))
	 (alst (mapcar (lambda (r-c-v)
			 (cons (list (first r-c-v) (second r-c-v)) (third r-c-v)))
		       constraints))
	 (nums))
    (dotimes (row dim)
      (dotimes (col dim)
	(let ((v (cdr (assoc (list row col) alst :test #'equal))))
	  (push (if v v 0) nums))))
    (format nil "~{~a~}" (reverse nums))))

(compile 'encode-board)

(defun make-it-harder (constraints-list)
  (when *verbose*
    (print-queue-and-thread-status
     (format nil "searching ~a"
	     (- (if *16x16-mode* 256 81)
		(length (car constraints-list))))))
  (let ((new-constraints-list
	 (take-n-filter *n*
			(lambda (constraints)
			  (and (easy-constraints-check constraints)
			       (uniq-constraints constraints)))
			(shuffle
			 (remove-duplicates
			  (reduce #'append 
				  (mapcar #'harder-constraints constraints-list))
			  :test #'equal)))))
    (cond ((null new-constraints-list)
	   constraints-list)
	  (t
	   (make-it-harder new-constraints-list)))))

(compile 'make-it-harder)

(defun uniq-constraints (constraints)
  (let ((fn (make-generator constraints)))
    (not (and (funcall fn)
	      (funcall fn)))))

(compile 'uniq-constraints)

(defun easy-constraints-check (constraints)
  (or (not *easy*)
      (easy-constraints constraints)))

(compile 'easy-constraints-check)

(defun easy-constraints (constraints)
  "Determine whether a Sudoku puzzle can be solved easily using simple memo-taking."
  ;; (print-queue-and-thread-status (format nil "*16x16-mode*=~a (length (e....)=~a" *16x16-mode* (length (extend-constraints constraints))))
  (= (if *16x16-mode* 256 81)
     (length (extend-constraints constraints))))

(compile 'easy-constraints)

(defun take-n-filter (n fn lst &optional acc)
  (cond ((or (< n 1)
	     (null lst))
	 (reverse acc))
	(t
	 (let ((x (funcall fn (car lst))))
	   (take-n-filter (if x (1- n) n)
			  fn
			  (cdr lst)
			  (if x (cons (car lst) acc) acc))))))

(compile 'take-n-filter)

(defun harder-constraints (constraints)
  (let ((result))
    (dolist (p constraints result)
      (push (remove-if (lambda (x)
			 (equal x p))
		       constraints)
	    result))))

(compile 'harder-constraints)

(defun extend-constraints (constraints)
  (let ((dim (if *16x16-mode* 16 9))
	(result))
    (loop
       (let ((memo (make-memo (append result constraints)))
	     (result-1))
	 (let ((xs (trivial-check memo)))
	   (when xs
	     (push xs result-1)))
;;
	 (dotimes (row dim)
	   (let ((xs (grp-check memo (lambda (m)
	 			       (= row (first m))))))
	     (when xs
	       (push xs result-1))))
	 (dotimes (col dim)
	   (let ((xs (grp-check memo (lambda (m)
	 			       (= col (second m))))))
	     (when xs
	       (push xs result-1))))
	 (dotimes (blk dim)
	   (let* ((box-dim (if *16x16-mode* 4 3))
		  (xs (grp-check memo (lambda (m)
	 			       (and (= (truncate blk box-dim) (truncate (first m) box-dim))
	 				    (= (mod blk box-dim) (truncate (second m) box-dim)))))))
	     (when xs
	       (push xs result-1))))
;;
	 (if result-1
	     (setf result (remove-duplicates (reduce #'append result-1 :initial-value result) :test #'equal))
	     (return))))
    (let ((new-constraints (append result constraints)))
      (values new-constraints
	      (sort (make-memo new-constraints)
		    (lambda (x y)
		      (< (length (third x))
			 (length (third y)))))))))

(compile 'extend-constraints)

(defun empty-cells (constraints)
  (set-difference (if *16x16-mode*
		      '((0 0) (0 1) (0 2) (0 3) (0 4) (0 5) (0 6) (0 7) (0 8) (0 9) (0 10) (0 11) (0 12) (0 13) (0 14) (0 15)
			(1 0) (1 1) (1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8) (1 9) (1 10) (1 11) (1 12) (1 13) (1 14) (1 15)
			(2 0) (2 1) (2 2) (2 3) (2 4) (2 5) (2 6) (2 7) (2 8) (2 9) (2 10) (2 11) (2 12) (2 13) (2 14) (2 15)
			(3 0) (3 1) (3 2) (3 3) (3 4) (3 5) (3 6) (3 7) (3 8) (3 9) (3 10) (3 11) (3 12) (3 13) (3 14) (3 15)
			(4 0) (4 1) (4 2) (4 3) (4 4) (4 5) (4 6) (4 7) (4 8) (4 9) (4 10) (4 11) (4 12) (4 13) (4 14) (4 15)
			(5 0) (5 1) (5 2) (5 3) (5 4) (5 5) (5 6) (5 7) (5 8) (5 9) (5 10) (5 11) (5 12) (5 13) (5 14) (5 15)
			(6 0) (6 1) (6 2) (6 3) (6 4) (6 5) (6 6) (6 7) (6 8) (6 9) (6 10) (6 11) (6 12) (6 13) (6 14) (6 15)
			(7 0) (7 1) (7 2) (7 3) (7 4) (7 5) (7 6) (7 7) (7 8) (7 9) (7 10) (7 11) (7 12) (7 13) (7 14) (7 15)
			(8 0) (8 1) (8 2) (8 3) (8 4) (8 5) (8 6) (8 7) (8 8) (8 9) (8 10) (8 11) (8 12) (8 13) (8 14) (8 15)
			(9 0) (9 1) (9 2) (9 3) (9 4) (9 5) (9 6) (9 7) (9 8) (9 9) (9 10) (9 11) (9 12) (9 13) (9 14) (9 15)
			(10 0) (10 1) (10 2) (10 3) (10 4) (10 5) (10 6) (10 7) (10 8) (10 9) (10 10) (10 11) (10 12) (10 13) (10 14) (10 15)
			(11 0) (11 1) (11 2) (11 3) (11 4) (11 5) (11 6) (11 7) (11 8) (11 9) (11 10) (11 11) (11 12) (11 13) (11 14) (11 15)
			(12 0) (12 1) (12 2) (12 3) (12 4) (12 5) (12 6) (12 7) (12 8) (12 9) (12 10) (12 11) (12 12) (12 13) (12 14) (12 15)
			(13 0) (13 1) (13 2) (13 3) (13 4) (13 5) (13 6) (13 7) (13 8) (13 9) (13 10) (13 11) (13 12) (13 13) (13 14) (13 15)
			(14 0) (14 1) (14 2) (14 3) (14 4) (14 5) (14 6) (14 7) (14 8) (14 9) (14 10) (14 11) (14 12) (14 13) (14 14) (14 15)
			(15 0) (15 1) (15 2) (15 3) (15 4) (15 5) (15 6) (15 7) (15 8) (15 9) (15 10) (15 11) (15 12) (15 13) (15 14) (15 15))
		      '((0 0) (0 1) (0 2) (0 3) (0 4) (0 5) (0 6) (0 7) (0 8)
			(1 0) (1 1) (1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8)
			(2 0) (2 1) (2 2) (2 3) (2 4) (2 5) (2 6) (2 7) (2 8)
			(3 0) (3 1) (3 2) (3 3) (3 4) (3 5) (3 6) (3 7) (3 8)
			(4 0) (4 1) (4 2) (4 3) (4 4) (4 5) (4 6) (4 7) (4 8)
			(5 0) (5 1) (5 2) (5 3) (5 4) (5 5) (5 6) (5 7) (5 8)
			(6 0) (6 1) (6 2) (6 3) (6 4) (6 5) (6 6) (6 7) (6 8)
			(7 0) (7 1) (7 2) (7 3) (7 4) (7 5) (7 6) (7 7) (7 8)
			(8 0) (8 1) (8 2) (8 3) (8 4) (8 5) (8 6) (8 7) (8 8)))
		  constraints
		  :test (lambda (x y)
			  (and (equal (first x) (first y))
			       (equal (second x) (second y))))))

(compile 'empty-cells)

(defun make-memo (constraints)
  (let* ((cs (empty-cells constraints)))
    (mapcar (lambda (c)
	      (let ((row (first c))
		    (col (second c)))
		(list row col (cell-memo row col constraints))))
	    cs)))

(compile 'make-memo)

(defun cell-memo (row col constraints)
  (let* ((xs (if *16x16-mode*
		 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
		 '(1 2 3 4 5 6 7 8 9)))
	 (box-dim (if *16x16-mode* 4 3)))
    (set-difference xs
		    (mapcar #'third
			    (remove-if-not (lambda (c)
					     (or (= (first c) row)
						 (= (second c) col)
						 (and (= (truncate (first c) box-dim) (truncate row box-dim))
						      (= (truncate (second c) box-dim) (truncate col box-dim)))))
					   constraints)))))

(compile 'cell-memo)

(defun trivial-check (memo)
  (mapcar (lambda (m)
	    (list (first m) (second m) (first (third m))))
	  (remove-if-not (lambda (m)
			   (= 1 (length (third m))))
			 memo)))

(compile 'trivial-check)

(defun grp-check (memo grp-p)
  (let ((tbl (make-hash-table))
	(grp-memo (remove-if-not grp-p memo)))
    (dolist (m grp-memo)
      (dolist (v (third m))
	(incf (gethash v tbl 0))))
    (let ((vs)
	  (result))
      (maphash (lambda (v n)
		 (when (= 1 n)
		   (push v vs)))
	       tbl)
      (dolist (v vs result)
	(let ((m (find-if (lambda (m)
			    (member v (third m)))
			  grp-memo)))
	  (push (list (first m) (second m) v) result))))))

(compile 'grp-check)

(defun shuffle-game-digits (81-digits)
  ;; 81-digits : string of length=81, [0-9]*
  (let* ((81-digits (string-upcase 81-digits))
	 (mapping-from (take-n (if *16x16-mode* 16 9) (coerce "123456789ABCDEFG" 'list)))
	 (mapping-to (shuffle mapping-from)))
    (coerce (loop for i from 0 to (1- (length 81-digits)) collect
		 (let* ((old-c (char 81-digits i))
			(pos (position old-c mapping-from)))
		   (if pos
		       (nth pos mapping-to)
		       old-c)))
	    'string)))

(compile 'shuffle-game-digits)

(defun flip-board (81-digits &optional (ntimes 1))
  (if (oddp ntimes)
    (let ((rows (group-n (if *16x16-mode* 16 9) (coerce 81-digits 'list))))
      (coerce (reduce #'append (mapcar #'reverse rows)) 'string))
    81-digits))

(compile 'flip-board)

(defun rotate-board (81-digits &optional (n90degrees 1))
  (cond ((zerop n90degrees)
	 81-digits)
	(t
	 (let* ((rows (group-n (if *16x16-mode* 16 9) (coerce 81-digits 'list)))
		(new-rows (apply #'mapcar #'list rows))
		(new-81-digits (coerce (reduce #'append new-rows) 'string)))
	 (rotate-board (flip-board new-81-digits) (1- n90degrees))))))

(compile 'rotate-board)

(defun shuffle-game (81-digits)
  (flip-board (rotate-board (shuffle-game-digits 81-digits) (random 4)) (random 2)))

(compile 'shuffle-game)

(defun drop-n (n lst)
  (nthcdr n lst))

(compile 'drop-n)

(defun group-n (n lst)
  (do ((result)
       (lst lst (drop-n n lst)))
      ((null lst) (reverse result))
    (push (take-n n lst) result)))

(compile 'group-n)

(defun take-n (n lst)
  (butlast lst (max 0 (- (length lst) n))))

(compile 'take-n)

(defun shuffle (lst)
  (nshuffle (copy-list lst)))

(compile 'shuffle)

(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(compile 'nshuffle)

(defun tstmp ()
  (multiple-value-bind (s m h d mo y) (get-decoded-time)
    (format nil "~{~2,'0d~^-~}" (list y mo d h m s))))

(compile 'tstmp)

(defun tstmp-for-log ()
  (multiple-value-bind (s m h d mo y) (get-decoded-time)
    (format nil "[~4,'0d-~2,'0d-~2,'0d][~2,'0d:~2,'0d:~2,'0d]" y mo d h m s)))

(compile 'tstmp-for-log)