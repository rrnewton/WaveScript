/* Stub code to access FFTW functions from OCaml */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

#include <fftw.h>
#include <rfftw.h>
#include <stdlib.h>



#define FFTW_SET_DIR(dir,  backward) \
if (Bool_val(backward)) dir = FFTW_BACKWARD; else dir = FFTW_FORWARD;

#define FFTW_SET_FLAGS(flags,  measure, in_place, use_wisdom) \
if (Bool_val(measure)) flags = FFTW_MEASURE; else flags = FFTW_ESTIMATE; \
if (Bool_val(in_place)) flags |= FFTW_IN_PLACE; \
if (Bool_val(use_wisdom)) flags |= FFTW_USE_WISDOM; \
flags |= FFTW_THREADSAFE; /* ??? Does one want the choice ??? */


#define REAL_BIGARRAY_VAL(v) ((fftw_real *) Data_bigarray_val(v))
#define COMPLEX_BIGARRAY_VAL(v) ((fftw_complex *) Data_bigarray_val(v))


/*
 * Normalization functions
 ***********************************************************************/

value fftw_normalize_1(value vn, value vhowmany, value vm,
                       value vmstride, value vmdist)
{
    CAMLparam5(vn, vhowmany, vm, vmstride, vmdist);
    int n = Int_val(vn), howmany = Int_val(vhowmany);
    int mstride = Int_val(vmstride), mdist = Int_val(vmstride);
    int howmany_mdist = howmany * mdist;
    int n_mstride = n * mstride;
    fftw_complex *m = COMPLEX_BIGARRAY_VAL(vm);
    int j;
    fftw_complex *i, *i_end;

    for(j=0; j < howmany_mdist; j += mdist) {
        i = m + j; /* = &m[j] */
        i_end = i + n_mstride;
        for(; i < i_end; i += mstride) {
            i->re /= n;
            i->im /= n;
        }
    }

    CAMLreturn(Val_unit);
}


value fftw_normalize_nd(value vprodn, value vhowmany, value vm)
{
    CAMLparam3(vprodn, vhowmany, vm);
    int prodn = Int_val(vprodn), howmany = Int_val(vhowmany);
    fftw_complex *m = COMPLEX_BIGARRAY_VAL(vm);
    fftw_complex *i, *i_end = m + (howmany * prodn);

    for(i=m; i < i_end; i++) {
        i->re /= prodn;
        i->im /= prodn;
    }

    CAMLreturn(Val_unit);
}


value fftw_normalize_r(value vn, value vhowmany, value vm,
                       value vmstride, value vmdist)
{
    CAMLparam5(vn, vhowmany, vm, vmstride, vmdist);
    int n = Int_val(vn), howmany = Int_val(vhowmany);
    int mstride = Int_val(vmstride), mdist = Int_val(vmstride);
    int howmany_mdist = howmany * mdist;
    int n_mstride = n * mstride;
    fftw_real *m = REAL_BIGARRAY_VAL(vm);
    int j;
    fftw_real *i, *i_end;

    for(j=0; j < howmany_mdist; j += mdist) {
        i = m + j; /* = &m[j] */
        i_end = i + n_mstride;
        for(; i < i_end; i += mstride)
            *i /= n;
    }

    CAMLreturn(Val_unit);
}

value fftw_normalize_rnd(
    value vsize, value vprodn, value vhowmany, value vm)
{
    CAMLparam4(vsize, vprodn, vhowmany, vm);
    const int size = Int_val(vsize); /* size of *one* array */
    const int prodn = Int_val(vprodn), howmany = Int_val(vhowmany);
    fftw_real *m = REAL_BIGARRAY_VAL(vm);
    fftw_real *i, *i_end = m + (howmany * size);

    for(i=m; i < i_end; i++)
        *i /= prodn;

    CAMLreturn(Val_unit);
}


/* NULL bigarray
 * (to pass when one wants Data_bigarray_val() to return NULL)
 ************************************************************************/


static value null_bigarray;

value fftw_alloc_null_bigarray(value vunit)
{
    CAMLparam1(vunit);

    null_bigarray = alloc_bigarray_dims(
        BIGARRAY_COMPLEX64 | BIGARRAY_C_LAYOUT, 0, NULL);
    /* if anything is allocated, free it: */
    free(Data_bigarray_val(null_bigarray));
    Data_bigarray_val(null_bigarray) = NULL; /* points to no data! */

    CAMLreturn(Val_unit);
}



value fftw_get_null_bigarray(value vlayout)
{
    CAMLparam1(vlayout);
    CAMLreturn(null_bigarray);
}



/*
 * FFTW -- one dimensional complex transforms
 **********************************************************************/

#define FFTW_PLAN_VAL(v) (* (fftw_plan *) Data_custom_val(v))

#define ALLOC_FFTW_PLAN alloc_custom(&fftw_plan_ops, sizeof(fftw_plan),  \
                        sizeof(struct fftw_plan_struct), \
                        20 * sizeof(struct fftw_plan_struct))

static void free_fftw_plan(value v)
{
    fftw_destroy_plan(FFTW_PLAN_VAL(v));
}

static struct custom_operations fftw_plan_ops = {
    /* identifier */ "FFTW/CAMLinterface/" CAML_FFTW_VERSION "/fftw_plan",
    /* finalize */ free_fftw_plan,
    /* compare */ custom_compare_default,
    /* hash */ custom_hash_default,
    /* serialize */ custom_serialize_default,
    /* deserialize */ custom_deserialize_default
};


value fftw_create_plan_wrapper(value vn, value backward, value measure,
                               value in_place, value use_wisdom)
{
    CAMLparam5(vn, backward, measure, in_place, use_wisdom);
    value plan;
    fftw_direction dir;
    int flags;

    FFTW_SET_DIR(dir,  backward);
    FFTW_SET_FLAGS(flags,  measure, in_place, use_wisdom);
    plan = ALLOC_FFTW_PLAN;

    if ((FFTW_PLAN_VAL(plan) = fftw_create_plan(Int_val(vn), dir, flags))
        == NULL)
        failwith("Fftw2.create");

    CAMLreturn(plan);
}



value fftw_create_plan_specific_wrapper(
    value vn, value backward, value measure, value in_place, value use_wisdom,
    value vin, value vistride,
    value vout, value vostride )
{
    CAMLparam5(vn, backward, measure, in_place, use_wisdom);
    CAMLxparam4(vin, vistride, vout, vostride);
    value plan;
    fftw_direction dir;
    int flags;

    FFTW_SET_DIR(dir,  backward);
    FFTW_SET_FLAGS(flags,  measure, in_place, use_wisdom);
    plan = ALLOC_FFTW_PLAN;

    if ((FFTW_PLAN_VAL(plan) = fftw_create_plan_specific(
             Int_val(vn), dir, flags,
             COMPLEX_BIGARRAY_VAL(vin), Int_val(vistride),
             COMPLEX_BIGARRAY_VAL(vout), Int_val(vostride) )
            ) == NULL)
        failwith("Fftw2.create");

    CAMLreturn(plan);
}


value fftw_create_plan_specific_wrapper_bc(value * argv, int argn)
{
    return fftw_create_plan_specific_wrapper(
        argv[0], argv[1], argv[2], argv[3], argv[4],
        argv[5], argv[6], argv[7], argv[8] );
}




value fftw_wrapper(value plan, value vhowmany,
                   value vin, value vistride, value vidist,
                   value vout, value vostride, value vodist)
{
    CAMLparam5(plan, vhowmany, vin, vistride, vidist);
    CAMLxparam3(vout, vostride, vodist);

    enter_blocking_section();  /* Allow other threads */

    fftw(FFTW_PLAN_VAL(plan), Int_val(vhowmany),
         COMPLEX_BIGARRAY_VAL(vin), Int_val(vistride), Int_val(vidist),
         COMPLEX_BIGARRAY_VAL(vout), Int_val(vostride), Int_val(vodist));

    leave_blocking_section();  /* Disallow other threads */
    CAMLreturn(Val_unit);
}


value fftw_wrapper_bc(value * argv, int argn)
{
    return fftw_wrapper(argv[0], argv[1], argv[2], argv[3], argv[4],
                        argv[5], argv[6], argv[7]);
}



/*
 * FFTWND -- multidimensionnal complex transforms
 **********************************************************************/

#define FFTWND_PLAN_VAL(v) (* (fftwnd_plan *) Data_custom_val(v))

#define ALLOC_FFTWND_PLAN alloc_custom(&fftwnd_plan_ops, \
                          sizeof(fftwnd_plan), \
                          sizeof(fftwnd_data), 20 * sizeof(fftwnd_data))

static void free_fftwnd_plan(value v)
{
    fftwnd_destroy_plan(FFTWND_PLAN_VAL(v));
}

static struct custom_operations fftwnd_plan_ops = {
    /* identifier */ "FFTW/CAMLinterface/" CAML_FFTW_VERSION "/fftwnd_plan",
    /* finalize */ free_fftwnd_plan,
    /* compare */ custom_compare_default,
    /* hash */ custom_hash_default,
    /* serialize */ custom_serialize_default,
    /* deserialize */ custom_deserialize_default
};


static int* convert_int_array(value vn, int *rank, int fortran_layout)
{
    int *n, i;

    *rank = Wosize_val(vn);
    if ((n = malloc(*rank * sizeof(int))) == NULL)
        failwith("Fftw2.nd_create");
    if (fortran_layout) {
        for(i = 0; i < *rank; i++)
            n[*rank - 1 - i] = Int_val(Field(vn, i));
    } else { /* C layout */
        for(i = 0; i < *rank; i++)
            n[i] = Int_val(Field(vn, i));
    }
    return(n);
}


value fftwnd_create_plan_wrapper(
    value vn, value backward, value measure, value in_place,
    value use_wisdom, value vfortran_layout)
{
    CAMLparam5(vn, backward, measure, in_place, use_wisdom);
    CAMLxparam1(vfortran_layout);
    value plan;
    int rank, *n;
    fftw_direction dir;
    int flags;

    n = convert_int_array(vn, &rank, Bool_val(vfortran_layout));
    FFTW_SET_DIR(dir,  backward);
    FFTW_SET_FLAGS(flags,  measure, in_place, use_wisdom);
    plan = ALLOC_FFTWND_PLAN;

    if ((FFTWND_PLAN_VAL(plan) = fftwnd_create_plan(rank, n, dir, flags))
        == NULL)
        failwith("Fftw2.nd_create");

    free(n);
    CAMLreturn(plan);
}

value fftwnd_create_plan_wrapper_bc(value * argv, int argn)
{
    return fftwnd_create_plan_wrapper(
        argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}




value fftwnd_create_plan_specific_wrapper(
    value vn, value backward, value measure, value in_place, value use_wisdom,
    value vfortran_layout,  value vin, value vistride,
    value vout, value vostride )
{
    CAMLparam5(vn, backward, measure, in_place, use_wisdom);
    CAMLxparam5(vfortran_layout, vin, vistride, vout, vostride);
    value plan;
    int rank, *n;
    fftw_direction dir;
    int flags;

    n = convert_int_array(vn, &rank, Bool_val(vfortran_layout));
    FFTW_SET_DIR(dir,  backward);
    FFTW_SET_FLAGS(flags,  measure, in_place, use_wisdom);
    plan = ALLOC_FFTWND_PLAN;

    if ((FFTWND_PLAN_VAL(plan) = fftwnd_create_plan_specific(
             rank, n, dir, flags,
             COMPLEX_BIGARRAY_VAL(vin), Int_val(vistride),
             COMPLEX_BIGARRAY_VAL(vout), Int_val(vostride) )
            ) == NULL)
        failwith("Fftw2.nd_create");

    free(n);
    CAMLreturn(plan);
}

value fftwnd_create_plan_specific_wrapper_bc(value * argv, int argn)
{
    return fftwnd_create_plan_specific_wrapper(
        argv[0], argv[1], argv[2], argv[3], argv[4],
        argv[5], argv[6], argv[7], argv[8], argv[9] );
}



value fftwnd_wrapper(value plan, value vhowmany,
                     value vin, value vistride, value vidist,
                     value vout, value vostride, value vodist)
{
    CAMLparam5(plan, vhowmany, vin, vistride, vidist);
    CAMLxparam3(vout, vostride, vodist);

    enter_blocking_section();  /* Allow other threads */

    fftwnd(FFTWND_PLAN_VAL(plan), Int_val(vhowmany),
           COMPLEX_BIGARRAY_VAL(vin), Int_val(vistride), Int_val(vidist),
           COMPLEX_BIGARRAY_VAL(vout), Int_val(vostride), Int_val(vodist));

    leave_blocking_section();  /* Disallow other threads */
    CAMLreturn(Val_unit);
}



value fftwnd_wrapper_bc(value * argv, int argn)
{
    return fftwnd_wrapper(
        argv[0], argv[1], argv[2], argv[3], argv[4],
        argv[5], argv[6], argv[7]);
}




/*
 * RFFTW -- one-dimensionnal real transforms
 **********************************************************************/

#define RFFTW_PLAN_VAL(v) (* (rfftw_plan *) Data_custom_val(v))

#define ALLOC_RFFTW_PLAN alloc_custom(&rfftw_plan_ops, sizeof(rfftw_plan), \
                         sizeof(struct fftw_plan_struct), \
                         20 * sizeof(struct fftw_plan_struct))

static void free_rfftw_plan(value v)
{
    rfftw_destroy_plan(RFFTW_PLAN_VAL(v));
}

static struct custom_operations rfftw_plan_ops = {
    /* identifier */ "FFTW/CAMLinterface/" CAML_FFTW_VERSION "/rfftw_plan",
    /* finalize */ free_rfftw_plan,
    /* compare */ custom_compare_default,
    /* hash */ custom_hash_default,
    /* serialize */ custom_serialize_default,
    /* deserialize */ custom_deserialize_default
};


value rfftw_create_plan_wrapper(value vn, value backward, value measure,
                                value in_place, value use_wisdom)
{
    CAMLparam5(vn, backward, measure, in_place, use_wisdom);
    value plan;
    fftw_direction dir;
    int flags;

    FFTW_SET_DIR(dir,  backward);
    FFTW_SET_FLAGS(flags,  measure, in_place, use_wisdom);
    plan = ALLOC_RFFTW_PLAN;

    if ((RFFTW_PLAN_VAL(plan) = rfftw_create_plan(Int_val(vn), dir, flags))
        == NULL)
        failwith("Fftw2.r_create");

    CAMLreturn(plan);
}



value rfftw_create_plan_specific_wrapper(
    value vn, value backward, value measure, value in_place, value use_wisdom,
    value vin, value vistride,
    value vout, value vostride )
{
    CAMLparam5(vn, backward, measure, in_place, use_wisdom);
    CAMLxparam4(vin, vistride, vout, vostride);
    value plan;
    fftw_direction dir;
    int flags;

    FFTW_SET_DIR(dir,  backward);
    FFTW_SET_FLAGS(flags,  measure, in_place, use_wisdom);
    plan = ALLOC_RFFTW_PLAN;

    if ((RFFTW_PLAN_VAL(plan) = rfftw_create_plan_specific(
             Int_val(vn), dir, flags,
             REAL_BIGARRAY_VAL(vin), Int_val(vistride),
             REAL_BIGARRAY_VAL(vout), Int_val(vostride) )
            ) == NULL)
        failwith("Fftw2.r_create");

    CAMLreturn(plan);
}


value rfftw_create_plan_specific_wrapper_bc(value * argv, int argn)
{
    return rfftw_create_plan_specific_wrapper(
        argv[0], argv[1], argv[2], argv[3], argv[4],
        argv[5], argv[6], argv[7], argv[8] );
}




value rfftw_wrapper(value plan, value vhowmany,
                    value vin, value vistride, value vidist,
                    value vout, value vostride, value vodist)
{
    CAMLparam5(plan, vhowmany, vin, vistride, vidist);
    CAMLxparam3(vout, vostride, vodist);

    enter_blocking_section();  /* Allow other threads */

    rfftw(FFTW_PLAN_VAL(plan), Int_val(vhowmany),
          REAL_BIGARRAY_VAL(vin), Int_val(vistride), Int_val(vidist),
          REAL_BIGARRAY_VAL(vout), Int_val(vostride), Int_val(vodist));

    leave_blocking_section();  /* Disallow other threads */
    CAMLreturn(Val_unit);
}


value rfftw_wrapper_bc(value *argv, int argn)
{
    return rfftw_wrapper(argv[0], argv[1], argv[2], argv[3], argv[4],
                         argv[5], argv[6], argv[7]);
}







/*
 * RFFTWND -- multi-dimensionnal real transforms
 **********************************************************************/

#define RFFTWND_PLAN_VAL(v) (* (rfftwnd_plan *) Data_custom_val(v))

#define ALLOC_RFFTWND_PLAN alloc_custom(&rfftwnd_plan_ops, \
                           sizeof(rfftwnd_plan), \
                           sizeof(fftwnd_data), 20 * sizeof(fftwnd_data))

static void free_rfftwnd_plan(value v)
{
    rfftwnd_destroy_plan(RFFTWND_PLAN_VAL(v));
}

static struct custom_operations rfftwnd_plan_ops = {
    /* identifier */ "FFTW/CAMLinterface/" CAML_FFTW_VERSION "/rfftwnd_plan",
    /* finalize */ free_rfftwnd_plan,
    /* compare */ custom_compare_default,
    /* hash */ custom_hash_default,
    /* serialize */ custom_serialize_default,
    /* deserialize */ custom_deserialize_default
};




value rfftwnd_create_plan_wrapper(
    value vn, value backward, value measure, value in_place,
    value use_wisdom, value vfortran_layout)
{
    CAMLparam5(vn, backward, measure, in_place, use_wisdom);
    CAMLxparam1(vfortran_layout);
    value plan;
    int rank, *n;
    fftw_direction dir;
    int flags;

    n = convert_int_array(vn, &rank, Bool_val(vfortran_layout));
    FFTW_SET_DIR(dir,  backward);
    FFTW_SET_FLAGS(flags,  measure, in_place, use_wisdom);
    plan = ALLOC_RFFTWND_PLAN;

    if ((RFFTWND_PLAN_VAL(plan) = rfftwnd_create_plan(rank, n, dir, flags))
        == NULL)
        failwith("Fftw2.rnd_create");

    free(n);
    CAMLreturn(plan);
}

value rfftwnd_create_plan_wrapper_bc(value * argv, int argn)
{
    return rfftwnd_create_plan_wrapper(
        argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}



value rfftwnd_real_to_complex_wrapper(
    value plan, value vhowmany,
    value vin, value vistride, value vidist,
    value vout, value vostride, value vodist)
{
    CAMLparam5(plan, vhowmany, vin, vistride, vidist);
    CAMLxparam3(vout, vostride, vodist);
    
    enter_blocking_section();  /* Allow other threads */

    rfftwnd_real_to_complex(
        RFFTWND_PLAN_VAL(plan), Int_val(vhowmany),
        REAL_BIGARRAY_VAL(vin), Int_val(vistride), Int_val(vidist),
        COMPLEX_BIGARRAY_VAL(vout), Int_val(vostride), Int_val(vodist));

    leave_blocking_section();  /* Disallow other threads */
    CAMLreturn(Val_unit);
}


value rfftwnd_real_to_complex_wrapper_bc(value * argv, int argn)
{
    return rfftwnd_real_to_complex_wrapper(
        argv[0], argv[1], argv[2], argv[3], argv[4],
        argv[5], argv[6], argv[7]);
}



value rfftwnd_complex_to_real_wrapper(
    value plan, value vhowmany,
    value vin, value vistride, value vidist,
    value vout, value vostride, value vodist)
{
    CAMLparam5(plan, vhowmany, vin, vistride, vidist);
    CAMLxparam3(vout, vostride, vodist);

    enter_blocking_section();  /* Allow other threads */

    rfftwnd_complex_to_real(
        RFFTWND_PLAN_VAL(plan), Int_val(vhowmany),
        COMPLEX_BIGARRAY_VAL(vin), Int_val(vistride), Int_val(vidist),
        REAL_BIGARRAY_VAL(vout), Int_val(vostride), Int_val(vodist));

    leave_blocking_section();  /* Disallow other threads */
    CAMLreturn(Val_unit);
}


value rfftwnd_complex_to_real_wrapper_bc(value * argv, int argn)
{
    return rfftwnd_complex_to_real_wrapper(
        argv[0], argv[1], argv[2], argv[3], argv[4],
        argv[5], argv[6], argv[7]);
}





/*
 * Wisdom nanagment
 ***********************************************************************/

/*** Export ***/

/* fold over wisdom */
static
void call_fold_emitter(char c, /* value */ void *data)
{
  static value * closure_emitter = NULL;
  if (closure_emitter == NULL)
    closure_emitter = caml_named_value("Fftw wisdom emitter fold");

  *((value *) data) =
      callback2(*closure_emitter, Val_int(c), *((value *) data) );
}

value fftw_export_wisdom_fold(value init)
{
  CAMLparam1(init);
  value data;

  data = init;
  fftw_export_wisdom(call_fold_emitter,  &data);
  CAMLreturn( data );
}

/* iter over wisdom */

static
void call_iter_emitter(char c, void *data)
{
  static value * closure_emitter = NULL;
  if (closure_emitter == NULL)
    closure_emitter = caml_named_value("Fftw wisdom emitter iter");

  callback(*closure_emitter, Val_int(c));
}

value fftw_export_wisdom_iter(value vunit)
{
  CAMLparam1(vunit);

  fftw_export_wisdom(call_fold_emitter, NULL);
  CAMLreturn( Val_unit );
}

/* To file */
value fftw_export_wisdom_to_file_wrapper(value vname)
{
    CAMLparam1(vname);
    FILE *out;

    out = fopen(String_val(vname), "w");
    fftw_export_wisdom_to_file(out);
    fclose(out);
    CAMLreturn(Val_unit);
}

/* To string */
value fftw_export_wisdom_to_string_wrapper(value vunit)
{
    CAMLparam1(vunit);
    CAMLreturn( copy_string(fftw_export_wisdom_to_string()) );
}


/*** Import ***/

/* fold over wisdom */
static
int call_fold_get_input(/* value */ void *data)
{
  static value * closure_emitter = NULL;
  value temp;
  if (closure_emitter == NULL)
    closure_emitter = caml_named_value("Fftw wisdom get_input fold");

  temp = callback(*closure_emitter, *((value *) data) );
  *((value *) data) = Field(temp, 0);
  return( Int_val(Field(temp, 1)) );
}

value fftw_import_wisdom_fold(value init)
{
  CAMLparam1(init);
  value data;

  data = init;
  fftw_import_wisdom(call_fold_get_input,  &data);
  CAMLreturn( data );
}

/* iter over wisdom */
static
int call_iter_get_input(void *data)
{
  static value * closure_emitter = NULL;
  if (closure_emitter == NULL)
    closure_emitter = caml_named_value("Fftw wisdom get_input iter");

  return(Int_val( callback(*closure_emitter, Val_unit) ));
}

value fftw_import_wisdom_iter(value vunit)
{
  CAMLparam1(vunit);
  fftw_import_wisdom(call_iter_get_input, NULL);
  CAMLreturn( Val_unit );
}

/* From file */
value fftw_import_wisdom_from_file_wrapper(value vname)
{
    CAMLparam1(vname);
    FILE *in;

    in = fopen(String_val(vname), "r");
    fftw_import_wisdom_from_file(in);
    fclose(in);
    CAMLreturn(Val_unit);
}

/* From string */
value fftw_import_wisdom_from_string_wrapper(value vs)
{
    CAMLparam1(vs);
    fftw_import_wisdom_from_string(String_val(vs));
    CAMLreturn(Val_unit);
}


/*** Forget ***/

value fftw_forget_wisdom_wrapper(value vunit)
{
    CAMLparam1(vunit);
    fftw_forget_wisdom();
    CAMLreturn(Val_unit);
}
