(declare (unit enums))

(define *coltypes* '())
(define-macro (define-coltype sym num)
  `(begin
     (set! *coltypes* (cons (cons ,num (quote ,sym)) *coltypes*))
     (define ,sym ,num)))

(define (coltype n) (cdr (assoc n *coltypes*)))
(define (coltype->number t) (car (rassoc t *coltypes*)))

(define-coltype SQLOT_CHR 1) ; /**< (ORANET TYPE) character string */
(define-coltype SQLOT_NUM 2) ; /**< (ORANET TYPE) oracle numeric */
(define-coltype SQLOT_INT 3) ; /**< (ORANET TYPE) integer */
(define-coltype SQLOT_FLT 4) ; /**< (ORANET TYPE) Floating point number */
(define-coltype SQLOT_STR 5) ; /**< zero terminated string */
(define-coltype SQLOT_VNU 6) ; /**< NUM with preceding length byte */
(define-coltype SQLOT_PDN 7) ; /**< (ORANET TYPE) Packed Decimal Numeric */
(define-coltype SQLOT_LNG 8) ; /**< long */
(define-coltype SQLOT_VCS 9) ; /**< Variable character string */
(define-coltype SQLOT_NON 10) ; /**< Null/empty PCC Descriptor entry */
(define-coltype SQLOT_RID 11) ; /**< rowid */
(define-coltype SQLOT_DAT 12) ; /**< date in oracle format */
(define-coltype SQLOT_VBI 15) ; /**< binary in VCS format */
(define-coltype SQLOT_BIN 23) ; /**< binary data(DTYBIN) */
(define-coltype SQLOT_LBI 24) ; /**< long binary */
(define-coltype SQLOT_UIN 68) ; /**< unsigned integer */
(define-coltype SQLOT_SLS 91) ; /**< Display sign leading separate */
(define-coltype SQLOT_LVC 94) ; /**< Longer longs (char) */
(define-coltype SQLOT_LVB 95) ; /**< Longer long binary */ 
(define-coltype SQLOT_AFC 96) ; /**< Ansi fixed char */
(define-coltype SQLOT_AVC 97) ; /**< Ansi Var char */
(define-coltype SQLOT_CUR 102) ; /**< cursor  type */
(define-coltype SQLOT_RDD 104) ; /**< rowid descriptor */
(define-coltype SQLOT_LAB 105) ; /**< label type */
(define-coltype SQLOT_OSL 106) ; /**< oslabel type */
(define-coltype SQLOT_NTY 108) ; /**< named object type */
(define-coltype SQLOT_REF 110) ; /**< ref type */
(define-coltype SQLOT_CLOB 112) ; /**< character lob */
(define-coltype SQLOT_BLOB 113) ; /**< binary lob */
(define-coltype SQLOT_BFILEE 114) ; /**< binary file lob */
(define-coltype SQLOT_CFILEE 115) ; /**< character file lob */
(define-coltype SQLOT_RSET 116) ; /**< result set type */
(define-coltype SQLOT_NCO 122) ; /**< named collection type (varray or nested table) */
(define-coltype SQLOT_VST 155) ; /**< OCIString type */
(define-coltype SQLOT_ODT 156) ; /**< OCIDate type */

;/* datetimes and intervals */
(define-coltype SQLOT_DATE 184) ; /**< ANSI Date */
(define-coltype SQLOT_TIME 185) ; /**< TIME */
(define-coltype SQLOT_TIME_TZ 186) ; /**< TIME WITH TIME ZONE */
(define-coltype SQLOT_TIMESTAMP 187) ; /**< TIMESTAMP */
(define-coltype SQLOT_TIMESTAMP_TZ 188) ; /**< TIMESTAMP WITH TIME ZONE */
(define-coltype SQLOT_INTERVAL_YM 189) ; /**< INTERVAL YEAR TO MONTH */
(define-coltype SQLOT_INTERVAL_DS 190) ; /**< INTERVAL DAY TO SECOND */
(define-coltype SQLOT_TIMESTAMP_LTZ 232) ; /**< TIMESTAMP WITH LOCAL TZ */

; LOB stuff
(define SQLO_ONE_PIECE 0)