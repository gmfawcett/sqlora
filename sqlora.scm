(declare (uses enums carray))
(use lolevel lazy-ffi srfi-1 srfi-13)
(use regex)

#~"libsqlora8.so"

(eval-when (compile)
  (define DEBUG #f))
	   
(define-macro (dprint . body)
  (if DEBUG
      `(print ".... [ debug ]  " ,@body)
      `(noop ,@body)))

(define-macro (with-error-check stmt)
  (let ((res (gensym)))
    `(let ((,res ,stmt))
       (sqlo-error-check)
       ,res)))

(define-macro (with-error-retval-check stmt)
  (let ((res (gensym)))
    `(let ((,res ,stmt))
       (when (negative? ,res)
         (sqlo-error))
       ,res)))

(define (sqlo-error-check #!optional (extra ""))
  (when (negative? (#~sqlo_geterrcode (*conn*)
				      return: int:))
    (sqlo-error extra)))

(define (exn:sqlora raw-message)
  (match-let (((code msg)
               (cdr (string-search "(.+?): (.+)"
                                   raw-message))))
    (make-composite-condition
     (make-property-condition 'exn 'message raw-message)
     (make-property-condition 'oracle)
     (make-property-condition (string->symbol code)))))

(define (sqlo-error #!optional (extra ""))
  ;; fixme, extra not in use right now.
(error  (exn:sqlora (#~sqlo_geterror (*conn*) return: string:))))




(define sqlo-init
  (let ((called-once #f))
    (lambda (#!key (threaded #f) (max-conns 1) (max-cursors 100))
      (when called-once
        (error "cannot re-init sqlora!"))
      (#~sqlo_init (if threaded 1 0)
                   max-conns max-cursors return: int:)
      (set! called-once #t))))

(define *conn* (make-parameter #f))

(define (sqlo-connect connstring)
  (let-location ((dbh int))
    (with-error-retval-check
     (#~sqlo_connect (location dbh) connstring return: int:))
     (*conn* dbh)
      dbh))

(define (sqlo-attach connstring)
  (let-location ((dbh int))
    (with-error-retval-check
     (#~sqlo_server_attach (location dbh) connstring return: int:))
    (*conn* dbh)
    dbh))

(define (sqlo-session-begin uid pwd)
  (#~sqlo_session_begin (*conn*) string: uid
                         string: pwd
                         return: int:))

(define (sqlo-disconnect)
  (#~sqlo_finish (*conn*))
  (*conn* #f))

(define (sqlo-ora-version)
  (let ((v (make-static-byte-vector 80 0)))
    (set-finalizer! v object-release)
    (#~sqlo_server_version (*conn*)
			   v (byte-vector-length v))
    (byte-vector->string v)))

(define (sqlo-exists? table colname colval #!optional (where ""))
  (zero? (with-error-retval-check
          (#~sqlo_exists (*conn*)
                         string: table string: colname
                         string: colval string: where
                         return: int:))))

(define (sqlo-table-exists? table)
  (sqlo-exists? "USER_TABLES" "TABLE_NAME"
                (string-upcase table)))

(define char-array-elt
  (foreign-lambda* c-string ((c-pointer vals) (int n))
                   "return(((char **)vals)[n]);"))

(define (char-array->list ptr len)
  (map (lambda (n) (char-array-elt ptr n))
       (iota len)))

(define (fetch cursor)
  (let ((v (zero? (with-error-retval-check
                 (#~sqlo_fetch cursor 1 return: int:)))))
    (dprint "fetch: " v)
    v))

(define (row-values cursor)
  (let-location ((ncols int))
    (let ((ptr (#~sqlo_values cursor (location ncols) 1 
			      return: pointer:)))
      (char-array->list ptr ncols))))

(define (row-values/translate cursor types)
  (map translate-value (row-values cursor) types))

(define (translate-value value type)
  (case type
          ('SQLOT_NUM (with-input-from-string value read))
          (else value)))

(define (column-names cursor)
  (let-location ((written int) (space int))
    (#~sqlo_ocol_names2 cursor (location written) 
			(location space))
    (char-array->list (address->pointer space) written)))

(define (column-types cursor)
  (let loop ((n 1) (acc '()))
    (let ((type (#~sqlo_get_ocol_dtype cursor int: n return: int:)))
      (if (or (negative? type) (> n 100))
	  (reverse acc)
	  (loop (add1 n) (cons (coltype type) acc))))))

(define (sqlo-close cursor)
  (with-error-check (#~sqlo_close cursor return: int:)))

(define (sqlo-query statement           ; or cursor handle
                    #!optional (params #f)
                    #!key (metadata #t) (keep-alive #f) (no-fetch #f)
                    (prefetch-hook noop) (mutate-hook values))
  (let ((cursor
         (if (number? statement)
             ;; then the cursor is already open. use sqlo_reopen.
             (begin  (with-error-retval-check
                      (#~sqlo_reopen statement
                                     (length params)
                                     (apply c-string-array params)
                                     return: int:))
                    statement)
             ;; nope, this is a new statement, needs a new cursor.
             (if params
                 (#~sqlo_open (*conn*) statement
                              (length params)
                              (apply c-string-array params)
                              return: int:)
                 (#~sqlo_open (*conn*) statement
                              0 pointer: #f ; no params!
                              return: int:)))))
    (when (negative? cursor)
      (sqlo-error statement))
    (if no-fetch
        (fetch cursor)                  ; but only once!
        (let* ((names (and metadata (column-names cursor)))
               (types* (column-types cursor))
               (types (and metadata types*)))
          (let loop ((rows '()))
            (prefetch-hook cursor)
            (if (fetch cursor)
                (loop (cons (mutate-hook (row-values/translate cursor types*) types*) rows))
                (if keep-alive       ; first value will be the cursor.
                    (values cursor (reverse rows) names types)
                    (begin (unless (number? statement)
                             ;; we close unless we are reusing a cursor.
                             (sqlo-close cursor))
                           (values (reverse rows) names types)))))))))

(define (sqlo-exec statement)
  (with-error-retval-check
   (#~sqlo_exec (*conn*) string: statement return: int:)))

(define (sqlo-commit)
  (with-error-check (#~sqlo_commit (*conn*) return: int:)))

(define (sqlo-rollback)
  (with-error-check (#~sqlo_rollback (*conn*) return: int:)))

(define (sqlo-autocommit)
  (with-error-check (#~sqlo_autocommit (*conn*) return: int:)))

(define (sqlo-set-autocommit flag)
  (with-error-check
   (#~sqlo_set_autocommit (*conn*) (if flag 1 0) return: int:)))

(define (sqlo-prows cursor) (#~sqlo_prows cursor))



;; LOB stuff. don't use this. it's not finished yet.

(define (sqlo-prepare stmt)	       ; string -> cursor
  (with-error-retval-check (#~sqlo_prepare (*conn*) string: stmt return: int:)))

(define (sqlo-execute cursor)
  (with-error-retval-check
   (#~sqlo_execute int: cursor 1 return: int:)))

(define (read-single-lob statement)
  (condition-case
   (let-location ((lob (pointer void))
		  (len unsigned-int))
     (let* ((cursor (sqlo-prepare statement)))
       (with-error-retval-check
	(#~sqlo_alloc_lob_desc (*conn*)
			       #$lob
			       return: int:))
        
       (with-error-retval-check
	(#~sqlo_define_by_pos cursor 
			      1
			      SQLOT_CLOB
			      #$lob 0 0 0 0
			      return: int:))
       (with-error-retval-check (sqlo-execute cursor))
       (with-error-retval-check
	(#~sqlo_lob_get_length (*conn*)
			       lob
			       #$len
			       return: int:))

       (let* ((buffer (make-static-byte-vector len 65)))
	 (with-error-retval-check
	  (#~sqlo_lob_read_buffer (*conn*)
				  lob
				  unsigned-int: len
				  buffer
				  unsigned-int: len
				  return: int:))
	 (#~sqlo_free_lob_desc (*conn*) #$lob)
	 (#~sqlo_close cursor)
	 (blob->string buffer))))
   (e () 
      (warning "ORACLE-RELATED ERROR..." e) #f)))

(define (write-single-lob statement newvalue)
  (condition-case
   (let-location ((lob (pointer void))
		  (len unsigned-int))
     (let* ((cursor (sqlo-prepare statement))
	    (newlength (string-length newvalue)))
       (with-error-retval-check
	(#~sqlo_alloc_lob_desc (*conn*)
			       #$lob
			       return: int:))
        
       (with-error-retval-check
	(#~sqlo_bind_by_pos cursor 
			      1
			      SQLOT_CLOB
			      #$lob 0 0 0 0
			      return: int:))
       (with-error-retval-check (sqlo-execute cursor))
       (with-error-retval-check
	(#~sqlo_lob_write_buffer (*conn*)
				 lob
				 newlength
				 newvalue
				 newlength
				 SQLO_ONE_PIECE
				 return: int:))
       	 (#~sqlo_free_lob_desc (*conn*) #$lob)
	 (#~sqlo_close cursor)))
   (e () 
      (warning "ORACLE-RELATED ERROR..." e) #f)))
