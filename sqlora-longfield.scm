;; -*- mode: c -*-
(use lolevel)

#>!
struct longfield { void *buf; void *obj; int bufsize; int indicator; int returnlength; };
typedef struct longfield longfield;

longfield *make_longfield(void *buf, void *obj, int bufsize) {
  longfield *fld = (longfield *)malloc(sizeof(longfield));
  fld->buf = (void *)buf;
  fld->obj = (void *)obj;
  fld->bufsize = bufsize;
  fld->indicator = fld->returnlength = 0;
  return fld;
}

void free_longfield(longfield *fld) {
  free(fld);
}

int longfield_position(int cursor, int posn, int type, longfield *fld) {
  return sqlo_define_by_pos(cursor, posn, type, fld->buf, fld->bufsize, &fld->indicator, &fld->returnlength, 0);
}
<#

(define (make-longfield string-or-size)
  (receive (string size)
      (if (number? string-or-size)
	  (values #f string-or-size)
	  (values string-or-size (string-length string-or-size)))
    (let ((buf (make-static-byte-vector size 0)))
      (when string
	((foreign-lambda* void ((c-string src) (int length) (c-pointer dst))
			  "memcpy(dst, src, length);")
	 string size (static-byte-vector->pointer buf)))
      (values
       (make_longfield (static-byte-vector->pointer buf)
		       (object->pointer buf)
		       size)
       buf))))

(define (free-longfield fld)
 (object-release (pointer->object (longfield-obj fld)))
 (free_longfield fld))

(define (inject-into lst posn elt)
  (append (take lst posn)
          (cons elt (drop lst  posn))))

(define (sqlo-query-long-field sz qry #!optional (params #f))
  (receive (fld buf) (make-longfield sz)
    ;; fixme, use dynamic-wind here.
    (let* ((prefetch-hook (lambda (cursor)
                            (longfield_position
                             cursor 1 (coltype->number (list-ref (column-types cursor) 0))
                             fld)))
           (mutate-hook (lambda (row types) (inject-into row 0 (byte-vector->string buf)))))
      (let ((result (first (first (sqlo-query qry params
                                       prefetch-hook: prefetch-hook
                                       mutate-hook: mutate-hook)))))
        (free-longfield fld)
        result))))

#;;;
(bound-sqlo-query
 "update graham set data=:1 where id=:2"
 longfield: (string->longfield "banana")
 string: "fruit")

(define (bound-sqlo-query qry . params)
  (let ((pairs (chop params 2))
	(cursor (sqlo-prepare qry))
	(pointers #f))
    ;; set up the bound parameters.
    (dynamic-wind
	(lambda () #f)
	(lambda ()
	  (let do-pair ((pairs pairs) (pos 1) (ptrs '()))
	    (match pairs
		   ((? null?)
		    (set! pointers ptrs)
		    #f)
		   (((type value) . tail)
		    (receive (type-code pointer plength reclaim?)
			(switch type
			  (string: (let ((buf (string->buffer value)))
				     (values SQLOT_STR
					     buf
					     (add1 (string-length value))
					     #t)))
			  (longfield: (values SQLOT_LBI
					      (longfield-buf value)
					      (longfield-bufsize value)
					      #f)))
		      (#~sqlo_bind_by_pos int: cursor
					  int: pos
					  int: type-code
					  pointer: pointer
					  int: plength
					  0 0
					  return: int:)
		      (do-pair (cdr pairs) (add1 pos) (if reclaim?
							  (cons pointer ptrs)
							  ptrs))))))
	  ;; make the call.
	  (#~sqlo_execute int: cursor int: 1 return: int:))
	(lambda ()
	  ;; cleanup
	  (for-each free pointers)))
    ;; fetching?
    ))

(define (string->buffer s)
  ;; Take string s, write it into a new static buffer, and return a
  ;; pointer to the buffer. You will need to free the buffer.
  (let ((l (string-length s)))
     (/string->buffer/ s l)))

(define /string->buffer/
   (foreign-lambda* c-pointer ((c-string s) (int l))
		   "void *ptr; ptr=malloc(l+1); memcpy(ptr, s, l+1);"
		   ;;"printf(\"ptr holds %s.\\n\", (char *)ptr);"
		   "return(ptr);"))
