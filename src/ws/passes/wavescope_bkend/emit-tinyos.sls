#!r6rs

;;;; [2008.05.21] Factoring out sublclasses of the emitC2 class into
;;;; their own files.  This file implements the TinyOS backend.

;;;; .author Ryan Newton

(library (ws passes wavescope_bkend emit-tinyos)
  (export 
	   ;emit-c2-generate-timed-code
	   ;<emitC2>
	   ;<emitC2-timed>
	   <tinyos>
	   <tinyos-timed>
	   ;<java>
	   ;<javaME>
	   )
  (import (except (rnrs (6)) error) (except (rnrs r5rs) force delay)
	  (rnrs mutable-strings)
	  (ws compat compat)
	  (ws common)
	  (ws passes wavescope_bkend insert-refcounts)
	  (ws passes wavescope_bkend emit-c)
	  (ws compiler_components c_generator)
	  (ws util bos_oop) 
	  (ws compiler_components c_generator)
	  (ws passes wavescope_bkend emit-c2)
	  ;(ws util rn-match) ;; TEMPTOGGLE
	  )
  

;; I'm not sure how the "namespace" is managed for AM messages.  Broadcast is zero right?
;; This is a constant offset which is applied to all the AM messages generated by WScript.
(define AM_OFFSET 10)

;; Note: wstiny uses different memory layouts for things like arrays!

(define-class <tinyos> (<emitC2>) 
  ;; Has some new fields to store accumulated bits of text:
  (top-acc config-acc module-acc boot-acc impl-acc proto-acc cleanup-acc
   ;;This is a flag to tell us if printf has already been included.:
   print-included
   amsender-count
   foreign-source-hook
   flushdone ;; A hook for code to go inside flushDone
   ))

(__spec initialise <tinyos> (self prog)
	;; Don't include wsc2.h!!
	(slot-set! self 'include-files (list (** "\"" (REGIMENTD) "/src/linked_lib/wstiny.h\"")))       
	(slot-set! self 'top-acc '())
	(slot-set! self 'config-acc '())
	(slot-set! self 'module-acc '())
	(slot-set! self 'boot-acc '())
	(slot-set! self 'impl-acc '())
	(slot-set! self 'proto-acc '())
	(slot-set! self 'cleanup-acc '())

	(slot-set! self 'print-included #f)
	(slot-set! self 'amsender-count 0)
	(slot-set! self 'flushdone "")
	)

(define __Type 
  (specialise! Type <tinyos> 
    (lambda (next self ty)
      (match ty
	[Int     "int16_t"] ;; [2008.04.04] Hmm... could just leave it as "int".
	[,else (next)]
	))))

(define __PrimApp
  (specialise! PrimApp <tinyos>
   (lambda (next self app kont mayberetty)
     (define (Simp x) (Simple self x))
     (match app
       [(,alloc-prim . ,_) 
	(guard (eq-any? alloc-prim 'cons 'Array:make 'Array:makeUNSAFE))
	(error 'emitC2:tinyos "Cannot generate code for dynamic allocation currently: ~s" 
	       (cons alloc-prim _))]

       [(sqrtF ,[Simp -> x]) (kont (list "sqrtf("x")"))]
       [(logF ,[Simp -> x]) (kont (list "logf("x")"))]
       [(logD ,[Simp -> x]) (error 'emit-c2:tinyos "no logD (double) on msp430 currently")]

       [(getID) (kont "TOS_NODE_ID")]
       [(realtime) (kont "TopTimer.getNow()")]

#;
       [(TOS:clock32khz)
	(inspect 'gotclock)

	"
  components new Msp430CounterC(TMilli) as Cntr;
  components Msp430TimerC;
  Cntr.Msp430Timer -> Msp430TimerC.TimerB;  
  WSQuery.Cntr -> Cntr.Counter;
"
]

       [,else (next)]))))

;; [2008.02.19] This is currently just to catch an error condition:
(define __Value 
  (specialise! Value <tinyos> 
    (lambda (next self)
      (lambda (xp kont)
	(match xp
	  ;; [2008.02.14] Disabling for tinyOS because now 
	  ;; string constants should ALL be statically allocated.
	  [',vec (guard (vector? vec) (vector-andmap char? vec))
		 (error 'emitC2:Value "should have lifted all the strings: ~s" 
			(list->string (vector->list vec)))]
	  [,_ ((next) xp kont)])))))


;; Increments and decrements do nothing.  Everything is statically allocated currently:
(__specreplace gen-incr-code <tinyos> (self ty ptr msg) (make-lines (format "/* refcnt incr stub ~s */\n" ty)))
(__specreplace gen-decr-code <tinyos> (self ty ptr msg) (make-lines (format "/* refcnt decr stub ~s */\n" ty)))

(__specreplace IterStartHook <tinyos> (self name arg argty) 
   "did_i_emit = FALSE;\n")
(__specreplace IterEndHook   <tinyos> (self name arg argty) 
   "if (!did_i_emit) cleanup_after_traversal();\n")

;; Now emit needs to set the flag saying that the processing chain continues.
(define _Emit 
  (specialise! Emit <tinyos> 
    (lambda (next self down* ty)
      (lambda (expr)
	(append-lines (make-lines "did_i_emit = TRUE;\n")
		      ((next) expr))))))

;; Wrap work functions as tasks.
(__specreplace GenWorkFunction <tinyos> (self name arg vqarg argty code)
  (define _arg (Var self arg))
  (define _argty (Type self argty))
  (define _name (Var self name))


;; TODO TODO TODO  TODO  TODO  TODO TODO 
;; FACTOR THIS PROTOTYPE GENERATION UP TO THE SUPER CLASS:

  ;; Add a prototype for the function, this removes the need for
  ;; careful ordering of operator definitions.
  (define prototype `("void ",(Var self name) "(",_argty" ",_arg")"))
  (slot-cons! self 'proto-acc (list prototype ";\n"))

  (values
   (make-lines "") ;; DUMMY PROTOTYPE, FIXME
   (make-lines 
   (list 
    ;; First the task:
    (block `("task void ",_name"_task()")
	   (list
	    "char "(Var self vqarg)";\n"
	    _argty" "_arg" = *(("_argty" *)WS_STRM_BUF);\n"
	    (IterStartHook self name arg argty)
	    (lines-text code)
	    (IterEndHook self name arg argty)))
    ;; And then the function for posting the task:
    ;;
    ;; TODO: if we want to do a *breadth* first traversal we need to
    ;; allocate multiple WS_STRM_BUF's when we have branches.
    ;; Alternatively, if we wish, truly, to do a depth-first
    ;; traversal, then we need to either split up fan-out boxes and
    ;; pass a continuation, or we need to (again) have a separate
    ;; stack of stream_bufs on the side to get back to.  

    ;; More sensibly, we can simply allocate a one-element buffer for
    ;; each box.  That will make breadth-first easy.
    ;;
    (block prototype
	   `(;"memcpy(",_arg", WS_STRM_BUF, sizeof(",_argty"));\n"
	     "(*(",_argty" *)WS_STRM_BUF) = ",_arg";\n"
	     ,(PostTask self name)
	     ))
	 "\n"))))

(define-generic PostTask)
(__spec PostTask <tinyos> (self name)
  (list "post "(Var self name)"_task();\n"))

;; For those allocation-forms that we can do statically:
;; Like SplitBinding returns two values: decl & init code.
;;
;; FIXME: This currently assumes no reference counts it uses tinyos
;; specific representations that don't have the RC fields because
;; there's currently no GC.
(__specreplace StaticAllocate <tinyos> (self lhs ty rhs)
  (match rhs
    [',vec (guard (vector? vec));(assert-type (Array Char) ',vec)
      (match ty
	[(Array Char)
	 ;; FIXME: Shouldn't neglect length slot just because it's a char array:
	 ;; Strip out any null characters??
	 (let* ([ls (vector->list vec)]
		[ls2 ;; Hack, a null terminator on the end is redundant for a string const:
		 (if (fx= 0 (char->integer (vector-ref vec (sub1 (vector-length vec)))))
		     (rdc ls) 
		     ls)]
		[str (list->string ls2)])
	   (values ;(make-lines (format "const char* ~a = ~s;\n" (text->string (Var self lhs)) str))
	    (make-lines (format "const char* ~a = ~s;\n" (text->string (Var self lhs)) str))
	    (make-lines "")))]

	;; Here we need to include the length slot:
	[(Array ,num) (guard (memq num num-types))
	 (define eltsize (datum->width num '()))
	 (define tmp (Var self (unique-name "tmptoparr")))
	 (define ARRLENSIZE 2)
	 (define padelts (exact (ceiling (/ ARRLENSIZE eltsize)))) ;; how many do we need to pad?
	 (define vr (text->string (Var self lhs)))
	 (define _num (Type self num))
	 (values 
	  (make-lines 
	   (list (format " ~a* ~a;\n" _num vr)
		 (format " ~a ~a[~a] = " _num tmp (+ padelts (vector-length vec)))
		 "{ "
		 (insert-between ", " 
				 (append 
				  (make-list padelts "0")
				  (map (lambda (x) (Const self x id)) (vector->list vec))))
		 " };\n\n"))
	  (make-lines (list (format "~a = ~a + ~a;\n" vr tmp padelts)
			    "SETARRLEN("vr", "(number->string (vector-length vec))");\n")))])]

    ;; This is TINYOS specific currently:
    ;; gen-incr-code
    [(assert-type (Array ,elt) (Array:make ,e ,x))
     (match (peel-annotations e)
       [',[number->string -> n]
	(define tmp (Var self (unique-name "tmptoparr")))
	(define _elt (Type self elt))
	(define _lhs (Var self lhs))
	(values ;(make-lines `(,(Type self elt)" ",(Var self lhs)"[",n"];\n"))
	        (make-lines (list "char "tmp"[sizeof("_elt") * "n" + ARRLENSIZE];\n"
				  _elt"* "_lhs" = ("_elt" *)("tmp" + ARRLENSIZE);\n"
			      ))
		(make-lines (list 
			     ;"((uint16_t*)"tmp")[0] = "n";\n"
			     "SETARRLEN("_lhs", "n");\n"
			     (if x 
				 (match (peel-annotations x)
				   [',c (list "// Init static array: \n"
					      "{ int i; for(i=0; i<"n"; i++) "_lhs"[i] = "(Const self c id)"; }\n"
					      )]
				   )
				 "")
			     )))
	])]

    ;; Redirect:
    [(assert-type (Array ,elt) (Array:makeUNSAFE ,e))
     (StaticAllocate self lhs ty `(assert-type (Array ,elt) (Array:make ,e #f)))]    

    [(assert-type ,_ ,[x y]) (values x y)]
    ))


;; We just memcpy the representation straight into the packet.
;; FIXME: This does not work for arrays yet:
;;
;; Alas, even if we had general purpose marshalling functionality,
;; this would not help us much here.  This code is all about unpacking
;; the TinyOS binary format, not one that we get to choose ourselves.
;; Luckily, MIG helps us.
(__specreplace Cutpoint <tinyos> (self ty in out)
   ;; Name of this cutpoints sender interface:
   (define basesend (format "BASESender~a" (slot-ref self 'amsender-count)))   
   (define ctpsend (format "CTPSender~a" (slot-ref self 'amsender-count)))
   (define ctprecv (format "CTPReceive~a" (slot-ref self 'amsender-count)))

   ;(define ampkt (format "AMPacket~a" (slot-ref self 'amsender-count)))
   (define AM_NUM (number->string (+ AM_OFFSET (slot-ref self 'amsender-count))))
   (define is-array? #f)
   (define _ty  (match ty [(Stream (Array ,elt)) (set! is-array? #t) (Type self elt)]
		          [(Stream ,elt)         (Type self elt)]))
   ;; Binds bytesize and does memcpy to "payload".
   (define copyit
     (match ty
       [(Stream (Array ,elt)) 
	(list "uint16_t bytesize = ARRLENSIZE + (sizeof("(Type self elt)") * ARRLEN(x));\n"
	      (let ([err ;"wserror(\"message exceeds max message payload\")"
		     "call Leds.led0Off();call Leds.led1On();call Leds.led2On();while(1){};"])
		(list 
		 "#ifdef WSRADIOMODE\n"
		 (block "if (bytesize > call CPacket.maxPayloadLength()) "
			err)
		 "#else\n"
		 (block "if (bytesize > call Packet.maxPayloadLength()) "
			err)
		 "#endif\n"))
	      "else if (!x) ((ARRLENTYPE *)payload)[0] = 0;\n"
	      "else my_memcpy(payload, ARRPTR(x), bytesize);")]
       [(Stream ,_)  (list "uint16_t bytesize = sizeof("_ty");\n"
			   "my_memcpy(payload, &x, sizeof(x));")]))
   (define fun (list "
  // CutPoint to server side:
  void "(Var self out)"("_ty(if is-array? "*" "")" x) {
#ifdef WSRADIOMODE
    "_ty"* payload = ("_ty" *)(call CPacket.getPayload(&radio_pkt, NULL));
#else
    "_ty"* payload = ("_ty" *)(call Packet.getPayload(&serial_pkt, NULL));
#endif

"(indent copyit "    ")"

    dbg_clear(\"WSQuery\", \"Sending on serial interface\\n\");
    // The collection tree protocol just gives us Send, not AMSend:
#ifdef WSRADIOMODE
    //call Leds.led0Toggle();
    if (!radio_busy && call "ctpsend".send(&radio_pkt, bytesize) == SUCCESS) {
      radio_busy = TRUE;
    }
    // Note: even if we're sending results up the CTP, we may want 
    // to print messages to the serial port (say for running on motelab).
    #ifdef PRINTFLOADED
      call PrintfFlush.flush();
    #endif 

#else
    if (!serial_busy && call "basesend".send(AM_BROADCAST_ADDR, &serial_pkt, bytesize) == SUCCESS) {
      serial_busy = TRUE;
    }
#endif


    // HACK: assuming ONE cutpoint for now.  And if we're in -split
    // mode, we need to do this here, because printF is assumed to be deactivated.
    cleanup_after_traversal();
  }
"))
   (when (zero? (slot-ref self 'amsender-count))
     (slot-cons! self 'config-acc (list "

  // This is for WS code that tries to get realtime() or clock()
  // Is there overhead to creating an additional instantiation here?
//  components new TimerMilliC() as TopTimer; 
//  WSQuery.TopTimer -> TopTimer;

#ifdef WSRADIOMODE
  #define AMCOMPONENT ActiveMessageC

  //#define AMCOMPONENT Collector

  components CtpP as Collector;
  components new TimerMilliC(); // Wait.. is this used? [2008.04.10]

  WSQuery.RoutingControl -> Collector;
  WSQuery.RootControl -> Collector;
  WSQuery.CPacket     -> Collector;

  // The Collection Tree version ALSO needs a serial interface for the base:
  components SerialActiveMessageC as Serial;
  WSQuery.SerialControl -> Serial;

#else 
  #define AMCOMPONENT SerialActiveMessageC
#endif
  // Here we connect to the main component for sending WS results:
  components AMCOMPONENT;
  WSQuery.AMControl -> AMCOMPONENT;
  WSQuery.Packet    -> AMCOMPONENT;
  WSQuery.AMPacket  -> AMCOMPONENT;
"))
     (slot-cons! self 'boot-acc (list "
  call AMControl.start();
  #ifdef WSRADIOMODE
    call SerialControl.start();
  #endif
"))
     (slot-cons! self 'module-acc (list "
  uses interface Packet;
  uses interface AMPacket;

#ifdef WSRADIOMODE
  // [2008.03.31] Adding support for collection tree protocol:
  uses interface SplitControl as SerialControl;
  uses interface StdControl as RoutingControl;
  uses interface RootControl;
  uses interface Packet as CPacket;
#endif

")))

   (printf " CUTTING query at type ~s\n" ty)

   ;; Everything below happens FOR EACH cut point:
(slot-cons! self 'top-acc (list "
// This is a dummy struct for MIG's benefit:
struct cuttype"AM_NUM" {
  "(match ty 
     [(Stream (Array ,elt))
      (list 
       "uint16_t len;\n"
       "  // This is a little odd, we just make it the max size, even though we won't use it.\n"
       "  "_ty" theitem[255];\n")]
     [,else (list _ty" theitem;")])"
};
enum {
  AM_CUTTYPE"AM_NUM" = "AM_NUM"
};
"))
   (slot-cons! self 'config-acc (list "
#ifdef WSRADIOMODE
  components new CollectionSenderC("AM_NUM") as "ctpsend";
  WSQuery."ctpsend" -> "ctpsend";
  WSQuery."ctprecv" -> Collector.Receive["AM_NUM"];
  // We also need the serial channel for getting the results off the base station.
#endif
  components new SerialAMSenderC("AM_NUM") as "basesend";
  WSQuery."basesend" -> "basesend";

  //WSQuery.AMPacket -> "basesend";
"))
   (slot-cons! self 'module-acc (list "
#ifdef WSRADIOMODE
  uses interface Send as "ctpsend"; 
  // Receive at the root of the collection tree:
  uses interface Receive as "ctprecv";
#endif
  uses interface AMSend as "basesend";

  //uses interface Packet;
  uses interface SplitControl as AMControl;
"))
   (slot-cons! self 'proto-acc (list "

  bool serial_busy = FALSE;
  bool radio_busy = FALSE;
  //message_t  _initial_pkt_storage;
  //message_t* relay_pkt = &_initial_pkt_storage;
  message_t radio_pkt;
  message_t serial_pkt;
  //message_t relay_pkt;

"))
   (slot-cons! self 'impl-acc (list "
  event void "basesend".sendDone(message_t* msg, error_t error) {
      dbg_clear(\"WSQuery\", \" Finished sending on serial interface\\n\");
    if (&serial_pkt == msg) {
      serial_busy = FALSE;
    } else wserror(\"error in serial interface\")
  }

#ifdef WSRADIOMODE
  event void "ctpsend".sendDone(message_t* msg, error_t error) {
    if (&radio_pkt == msg) {
      radio_busy = FALSE;
    } else wserror(\"error in radio interface\")
  }
  event void AMControl.startDone(error_t err) {
    if (err != SUCCESS) call AMControl.start();
    else {
      call RoutingControl.start();
      if (TOS_NODE_ID == 1) {
        //call Leds.led1On();
	call RootControl.setRoot();
      }
      // else call Timer.startPeriodic(2000);
    }
  }

  // FIXME: THIS IS UNFINISHED:
  event message_t* 
  "ctprecv".receive(message_t* msg, void* srcpayload, uint8_t len) {
    // We could swap buffers here, but that's an optimization.
    //message_t* temp = serial_pkt;

    "_ty"* payload = ("_ty" *)(call Packet.getPayload(&serial_pkt, NULL));
    //"_ty"* src  = ("_ty" *)(call CPacket.getPayload(msg, NULL));

    call Leds.led2Toggle();    
    if (! call RootControl.isRoot()) {
      wserror(\"I am not root.  Should not receive CTP messages.\")
    }

    my_memcpy(payload, srcpayload, len);

    if (!serial_busy && call "basesend".send(AM_BROADCAST_ADDR, &serial_pkt, len) == SUCCESS) {
      serial_busy = TRUE;
    }
    //return temp;
    return msg;
  }

  event void SerialControl.startDone(error_t err) {
    if (err != SUCCESS) call SerialControl.start();
  }
  event void SerialControl.stopDone(error_t err)  {}
#else
  event void AMControl.startDone(error_t error) {}
#endif

  event void AMControl.stopDone(error_t error) {}

"))   
   ;; Need to use a serialforwarder:
   ;(values (make-lines (format " /* CUTPOINT server ~a ~a */ \n" in out)) '() '())
   (slot-set! self 'amsender-count (add1 (slot-ref self 'amsender-count)))

   (values (make-lines fun)
	   '() ;; Binds
	   '() ;; Init code
	   )
   )



;; Add printf component to the accumulators.
(define-generic LoadPrintf)

(__specreplace LoadPrintf <tinyos> (self)
  (unless (slot-ref self 'print-included)
	       (slot-set! self 'print-included #t)
	       ;/opt/tinyos-2.x/tos/lib/printf/printf.h	       
	       ;; Making this an absolute link:
	       ;(add-include! self "\"printf.h\"")
	       (add-include! self (format "\"~a/lib/printf/printf.h\"" (getenv "TOSDIR")))

	       (slot-cons! self 'top-acc "
#define PRINTFLOADED
")
	       (slot-cons! self 'config-acc "
#ifndef TOSSIM
  components PrintfC;
  WSQuery.PrintfControl -> PrintfC;
  WSQuery.PrintfFlush   -> PrintfC;
#endif
")
	       (slot-cons! self 'module-acc "
#ifndef TOSSIM
  uses interface SplitControl as PrintfControl;
  uses interface PrintfFlush;
#endif
")
	       (slot-cons! self 'impl-acc (list "
#ifndef TOSSIM
event void PrintfControl.startDone(error_t error) {
  printf(\";; PrintfControl started (1/3)...\\n\");
  printf(\";; PrintfControl started (2/3)...\\n\");
  printf(\";; PrintfControl started (3/3)...\\n\");
  call PrintfFlush.flush();
  //call Leds.led0Toggle();
}

event void PrintfFlush.flushDone(error_t error) {
  "(slot-ref self 'flushdone)"
  // HACK: ASSUMING That flush only happens AFTER a traversal:
  cleanup_after_traversal();
}

event void PrintfControl.stopDone(error_t error) {
  //call PrintfFlush.flush();
}
#endif
"))
	       (slot-cons! self 'boot-acc "
#ifndef TOSSIM
  call PrintfControl.start();
#endif
")))

(define __Effect
  (specialise! Effect <tinyos>
    (lambda (next self)
      (lambda (xp)
	(match xp
	  [(print ,[(TyAndSimple self) -> ty x])	   
	   (begin
	     (LoadPrintf self)
	     (make-lines `("#ifdef TOSSIM\n"
			   "  dbg_clear(\"WSQuery\", \"",(type->printf-flag self ty)"\", ",x");\n"
			   "#else\n"
			   "  printf(\"",(type->printf-flag self ty)"\", ",x");\n"
			   "#endif\n"
			   ;"call PrintfFlush.flush();\n" ;; For now we flush when we get to the end of an event-chain (BASE)
			   ;"call Leds.led0Toggle();\n"
			   )))]
	  ;; Signal an error condition, currently just turns on a red light.
	  ;[(__wserror_ARRAY ,_) (make-lines TOS-signal-error)]
	  [,oth 
	   (let ([oper (next)])
	     (ASSERT procedure? oper)
	     (oper xp))])))))

(__specreplace ForeignSourceHook <tinyos> (self name callcode) 
	       callcode)

(define __Operator
  (specialise! Operator <tinyos> 
    (lambda (next self op)
      (match op
	[(__readFile . ,_) (error 'emit-tinyos:Operator "Cannot support readFile on tinyos")]
	[,oth (next)]))))

;; [2008.09.16] Modifying to return a list of c-* datatypes.
;; This method is still unpleasant because it does most of its work by
;; side-effecting fields.
(define __Source
 (specialise! Source <tinyos>
    (lambda (next self xp)
      (match xp
	;; TODO: streams of sensor data
	[((name ,nm) (output-type ,ty) (code ,cd) (outgoing ,down* ...))
	 (let loop ([cd cd])
	   (match cd 
	     [(inline_TOS ',top ',conf1 ',conf2 ',mod1 ',mod2 ',boot ',cleanup)
	      ;; FIXME: conf1!!!
	      (slot-cons! self 'top-acc top)
	      (slot-cons! self 'config-acc conf2)
	      (slot-cons! self 'module-acc mod1)
	      (slot-cons! self 'impl-acc mod2)
	      (slot-cons! self 'boot-acc boot)
	      (slot-cons! self 'cleanup-acc cleanup)
	      '()]
	     
	     ;; Allowing normal inline_C and treating it as a special case of inline_TOS:
	     [(inline_C ',top ',initfun)	      
	      (define boot (if (equal? initfun "") "" (string-append initfun "();\n")))
	      (loop `(inline_TOS ',top '"" '"" '"" '"" ',boot '""))]

	     [(__foreign_source ',name ',filels '(Stream ,type))
	      (define ty (Type self type))
	      (define arg (unique-name "tmp"))
	      (ASSERT "foreign source hack requires first 'filename' actually supply data rate in Hz, or -1 if unavailable" id
		      (and (not (null? filels)) (string->number (car filels))))
	      (for-each (lambda (file)
			  (let ([ext (extract-file-extension file)])
			    (cond
			     [(member ext '("nc" "h"))
			      (add-include! (list "\"" file "\""))]
			     [else (error 'emit-c:foreign 
					  "cannot load NesC extension from this type of file: ~s" file)])))
		(cdr filels))
	      ;; Create a function for the entrypoint.
	      ;; It should post a task!!
	      (slot-cons! self 'proto-acc `("void ",name"(",ty");\n"))
	      (slot-cons! self 'impl-acc  (block `("void ",name"(",ty" ",(Var self arg)")")
						 (ForeignSourceHook self name
								    (lines-text ((Emit self down* type) arg)))))
	      '()]
#;
	     [,_ (next)]))]))))


(__specreplace BuildTimerSourceDriver <tinyos> (self srcname* srccode* srcrates*)
  (unless (null? (filter id srcrates*))
    (error 'emitC2:BuildTimerSourceDriver 
	   "Should not be encountering built-in timer sources for tinyos backend currently: ~a" srcname*))
  (make-lines ""))


(__specreplace BuildOutputFiles <tinyos> (self includes freefundefs state ops init driver)
  ;(define _ (inspect (slot-ref self 'proto-acc)))
  (define config (list
"
configuration WSQueryApp { }
implementation {
  components MainC, WSQuery, LedsC;
  WSQuery -> MainC.Boot;
  WSQuery.Leds -> LedsC;
"(slot-ref self 'config-acc)"
}
"))
  (define module 
    (list 
      includes 
"#include \"Timer.h\"

"(text->string (map (curry StructDef self) (slot-ref self 'struct-defs)))"
"(slot-ref self 'top-acc)"

module WSQuery {
  uses interface Leds;
  uses interface Boot;

  // Adding a Timer that's always there:
//  uses interface Timer<TMilli> as TopTimer;

"(slot-ref self 'module-acc)"
}
implementation {

  char WS_STRM_BUF[128];
  bool did_i_emit = FALSE;
  // This lets us know if its safe to put a tuple in play:
  bool ws_currently_running = 0;
  // This tracks (or will track) the number of missed ticks/samples on the INPUT side:
  int32_t input_items_lost = 0;

  /* Prototypes */
  void initState();
"(slot-ref self 'proto-acc)"

  void cleanup_after_traversal();

  void BASE(char x) {
    #ifdef TOSSIM
      // For tossim we don't do a printf-flush; need to call this here:
      cleanup_after_traversal();
    #else
    #ifdef PRINTFLOADED
      call PrintfFlush.flush();
    #endif
    #endif
    // HACK FIXME:
    // We need to call the cleanup function after a traversal finishes.
    // A traversal won't necessarily make it all the way to BASE.
    // Therefor, each task needs to check if it ended a chain (failed to post another task).
    cleanup_after_traversal();
  }

"(insert-between "\n"
           (list 
	    ;; Don't need the freefundefs in TinyOS:
	    ;; (text->string (lines-text freefundefs))
                 state
                 ;toplevelsink
                 ops ;srcfuns 
                 init driver))"

"(slot-ref self 'impl-acc)"

//  event void TopTimer.fired() { }

  event void Boot.booted() {
    initState(); /* SHOULD POST A TASK */

"(indent (slot-ref self 'boot-acc) "    ")"
  }

  void cleanup_after_traversal() {
"(indent (slot-ref self 'cleanup-acc) "    ")"
    ws_currently_running = 0;\n
  }
}
"))

  ;; We return an association list of files to write.
  (vector
   (list (list "Makefile.tos2" (file->string (** (REGIMENTD) "/src/linked_lib/Makefile.tos2")))
	 (list "WSQueryApp.nc" (text->string config))
	 (list "WSQuery.nc"    (text->string module))
	 (list "query.py"      (file->string (** (REGIMENTD) "/src/linked_lib/run_query_tossim.py")))
	 (list "progtelos"     (file->string (** (REGIMENTD) "/src/linked_lib/progtelos")))
	 )
   ;; We also return a post-file-write thunk to execute:
   (lambda ()
     ;(printf " XXXXXXX POST COMMIT THUNK XXXXXXXXXX \n")
     (for i = 0 to (sub1 (slot-ref self 'amsender-count))
	  ;(printf "      CALLING MIG ~a\n" i)
	  ;; YUCK: we have to tell MIG about WSRADIOMODE too:
	  (let ([cmd (format "mig c -I~a -target=telosb WSQuery.nc cuttype~a -o WSQueryMsg~a.h" 
			     (string-append (getenv "TOSDIR") "/lib/printf")
			     (+ AM_OFFSET i) i)])
	    (display cmd)(newline)
	    (system cmd)))
     (system "chmod +x query.py")
     (system "chmod +x progtelos")
     (void))
   ))


;;================================================================================


(define-class <tinyos-timed> (<tinyos>) 
  (nametable))

;; Load the counter component:
(__spec initialise <tinyos-timed> (self prog) 
  (slot-cons! self 'config-acc " 
  components new Msp430CounterC(TMilli) as Cntr;
  components Msp430TimerC;
  Cntr.Msp430Timer -> Msp430TimerC.TimerB;  
  WSQuery.Cntr -> Cntr.Counter;\n")
  (slot-cons! self 'module-acc "uses interface Counter<TMilli,uint16_t> as Cntr;\n")
  (slot-cons! self 'proto-acc "  
   uint16_t overflow_count = 0;\n")
  (slot-cons! self 'impl-acc "async event void Cntr.overflow() { overflow_count++; }\n")
  (slot-set! self 'flushdone "dispatch(flushdispatch);")
  )

;; Returns text to put in the function body that implements the foreign source.
;; Callcode is code that invokes all the downstream processing of the event.
(__specreplace ForeignSourceHook <tinyos-timed> (self name callcode)
  (list
   (block "if (ws_currently_running)"
	  (format "printf(\"(ForeignBlocked ~a %u %u)\\n\", overflow_count, call Cntr.get());\n" name))
   (block "else"
	  (list (format "printf(\"(ForeignSource ~a %u %u)\\n\", overflow_count, call Cntr.get());\n" name)
		"ws_currently_running = 1;\n"
		"call PrintfFlush.flush();\n"
		callcode))))


;; These two *extend* rather than replace the code generated by the superclass:
(define __IterStartHook
  (specialise! IterStartHook <tinyos-timed> 
    (lambda (next self name arg argty)
    (LoadPrintf self)
    (list (next)
	  (format "printf(\"(Start ~a %u %u)\\n\", overflow_count, call Cntr.get());\n" name)))))
(define __IterEndHook
  (specialise! IterEndHook <tinyos-timed> 
    (lambda (next self name arg argty) 
    (LoadPrintf self)
    (list (next)
	  (format "printf(\"(End   ~a %u %u)\\n\", overflow_count, call Cntr.get());\n" name)
	  "call PrintfFlush.flush();\n"))))

(define nulldispatchcode "32767")

;; Generate nothing for the cutpoints... we're just interested in the Printf output:
;; We use this as an opportunity to flush:
(__specreplace Cutpoint <tinyos-timed> (self ty in out) 
   (define _ty  (match ty [(Stream ,elt) (Type self elt)]))
   (define fun (list "// Code generated in place of cutpoint:\n"
		"void "(Var self out)"("_ty" x) { 
  printf(\"(EndTraverse %u %u)\\n\\n\", overflow_count, call Cntr.get()); 
  flushdispatch = "nulldispatchcode";
  call PrintfFlush.flush(); 
  // HACK: ASSUMING ONLY ONE CUTPOINT CURRENTLY!!!!
  cleanup_after_traversal();
}"))
   (values (make-lines fun) '() '()))


;; Here we insert a bit of extra code to build the operator name table.
;; We use this to build a dispatcher.
(define __SortOperators
  (specialise! SortOperators <tinyos-timed>
    (lambda (next self ops)
      (define table (make-default-hash-table (length ops)))
      ;(define filtered (filter (compose not cutpoint?) ops))
      (define filtered
	;; Currently _merge doesn't generate tasks:
	(filter (lambda (op) (memq (car op) '(iterate))) ops)) ;; _merge
      (define (getname op) (cadr (ASSERT (assq 'name (cdr op)))))
      
      (slot-set! self 'nametable table)
      (for-eachi (lambda (ind op)
		   (hashtab-set! table (getname op) ind))
		 filtered)
      (ASSERT (< (length ops) (^ 2 16)))
      (slot-cons! self 'proto-acc (list "
  void dispatch(int);
  uint16_t flushdispatch = "nulldispatchcode";
"))
      (slot-cons! self 'impl-acc (list 
"
void dispatch(int tag) {
  switch (tag) {
"(mapi (lambda (ind op)
	 (format "    case ~a: post ~a_task(); break;\n" ind (getname op)))
       filtered)"
    case "nulldispatchcode": break;
  }
}"))
      (next)
      )))

(__specreplace PostTask <tinyos-timed> (self name)
  (define table (slot-ref self 'nametable))
  ;; FIXME CURRENTLY DOESN'T HANDLE FANOUT AT ALL!!
  (list "flushdispatch = "(number->string (hashtab-get table name))";\n"
	;"call PrintfFlush.flush();\n"
	))

;; TODO: use the Flushdone event as a trampoline to bounce us into the next task.
#;

;; Here we need to delay startup to wait for the profiler to connect.
;; This needs to be called LAST:
(define __BuildOutputFiles
  (specialise! BuildOutputFiles <tinyos-timed> 
    (lambda (next self includes freefundefs state ops init driver)
   (slot-cons! self 'config-acc "
  components new TimerMilliC() as Timer000;
  WSQuery.Timer000 -> Timer000;\n")
  (slot-cons! self 'module-acc "uses interface Timer<TMilli> as Timer000;\n")
  
  ;; Move the old boot code into a new dummy function:
  (slot-cons! self 'impl-acc (list "
void dummy_boot() {
"(slot-ref self 'boot-acc)"
}
event void Timer000.fired() {
  dummy_boot();
}
"))
  ;; All the real boot routine will do will be to set the timer:
  (slot-set! self 'boot-acc "call Timer000.startOneShot( 3000 );\n")
  (next)
  )))


) ;; End module

