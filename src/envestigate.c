#include <Rinternals.h>
#include <R_ext/Rdynload.h>


SEXP is_hashed(SEXP env){
  return ScalarLogical((HASHTAB(env)!=R_NilValue)? TRUE: FALSE);
}

SEXP hash_table(SEXP env){
  SEXP ret, ans = R_NilValue, frame;
  SEXP ht = HASHTAB(env);
  int n, i, j;

  if (ht==R_NilValue) return ans;

  n = length(ht);

  PROTECT(ret = allocList(n));
  ans = ret;

  for (i = 0; i < n; i++){
    frame = VECTOR_ELT(ht,i);
    j = 0;
    /* Count number of elements */
    while (frame != R_NilValue) {
      j++;
      frame = CDR(frame);
    }
    if (j == 0){
      SETCAR(ans,R_NilValue);
    } else {
      SETCAR(ans,allocVector(STRSXP,j));
      frame = VECTOR_ELT(ht,i);
      j = 0;
      while (frame != R_NilValue) {
        SET_STRING_ELT(CAR(ans),j,PRINTNAME(TAG(frame)));
        j++;
        frame = CDR(frame);
      }
    }
    ans = CDR(ans);
  }
  
  UNPROTECT(1);
  return ret;
}

/* Lifted from R sources in src/main/envir.c*/
static int hash_function(const char *s)
{
  char *p;
  unsigned h = 0, g;
  for (p = (char *) s; *p; p++) {
    h = (h << 4) + (*p);
    if ((g = h & 0xf0000000) != 0) {
      h = h ^ (g >> 24);
      h = h ^ g;
    }
  }
  return h;
}

SEXP hash_value(SEXP str){
  return ScalarInteger(hash_function(CHAR(STRING_ELT(str,0))));
}

static R_CallMethodDef callMethods[]  = {
  {"C_is_hashed", (DL_FUNC)&is_hashed, 1},
  {"C_hash_table", (DL_FUNC)&hash_table, 1},
  {"C_hash_value", (DL_FUNC)&hash_value, 1},
  {NULL, NULL, 0}
};

void R_init_envestigate(DllInfo *info){
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}

