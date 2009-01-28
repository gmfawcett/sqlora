(declare (unit carray))
(use srfi-1 lolevel)

(define (c-string-array . attrs)
  (assert (every string? attrs))
  (if (null? attrs)
      (null-pointer)
      (letrec ((n (length attrs))
               (inner (foreign-lambda* c-pointer ((scheme-object attrs)) "
// C snippets courtesy of Hans Bulfone                                   
C_word item;
char **c_attrs;
int nattrs;
int i;
if (attrs == C_SCHEME_FALSE)
  {
    c_attrs = NULL;
  }
else
  {
    for(nattrs=0, item=attrs; item!=C_SCHEME_END_OF_LIST; ++nattrs, item=C_u_i_cdr(item))
      ;
    c_attrs=malloc(sizeof(char*)*(nattrs+1));
    c_attrs[nattrs] = NULL;
    for(i=0, item=attrs; item!=C_SCHEME_END_OF_LIST; ++i, item=C_u_i_cdr(item))
      {
        C_word attr = C_u_i_car(item);
        int l=C_header_size(attr);
        c_attrs[i] = malloc(l + 1);
        c_attrs[i][l] = 0;
        memcpy(c_attrs[i], C_c_string(attr), l);
      }
  }
return(c_attrs);"))
               (fin (foreign-lambda* void ((c-pointer ptr) (int n)) "
int i;
char **c_attrs = (char **)ptr;
for(i=0; i<n; ++i) {
      free(c_attrs[i]);
  }
    free(c_attrs);"))
               (ptr (tag-pointer (inner attrs)
                                 'c-string-array)))
        (set-finalizer! ptr (cut fin <> n))
        ptr)))