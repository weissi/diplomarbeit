#include <sstream>

#include <string.h>
#include <assert.h>

#include <NTL/GF2E.h>
#include <NTL/GF2X.h>
#include <NTL/GF2XFactoring.h>

#include <pthread.h>

#include "ntl_interface_easy.h"

NTL_CLIENT

/*
 * No mem debugging right now :-)
 * #define MEM_DEBUGGING
 */

static bool initialized = false;
volatile pthread_t my_tid = 0;

#ifdef __cplusplus
extern "C" {
#endif
    static void ff_init(void) {
        GF2X P = BuildIrred_GF2X(256);
        GF2E::init(P);
    }

    static inline void check_init(void) {
        if (!initialized) {
            ff_init();
            initialized = true;
        }

        /*
        if (my_tid == 0) {
            my_tid = pthread_self();
        } else {
            assert(my_tid == pthread_self());
        }
        */
    }

    static inline GF2E *allocGF2E(void) {
        GF2E *e = new GF2E();
#ifdef MEM_DEBUGGING
        cout << "GF2E: ALLOC: " << (void*)e << endl;
#endif
        return e;
    }

    OpaqueElement ff_zero_element(void) {
        check_init();
        return (OpaqueElement)&GF2E::zero();
    }

    void ff_print_element(OpaqueElement opaque_e) {
        check_init();
        GF2E *e = (GF2E *)opaque_e;
        cout << *e << endl;
    }

    void ff_free_element(OpaqueElement opaque_e) {
        check_init();
        if (opaque_e != ff_zero_element()) {
            GF2E *e = (GF2E *)opaque_e;
#ifdef MEM_DEBUGGING
            cout << "GF2E:  FREE: " << (void*)e << endl;
#endif
            delete e;
        }
    }

    OpaqueElement ff_random_element(void) {
        check_init();
        GF2E *e = allocGF2E();
        *e = random_GF2E();
        return (OpaqueElement)e;
    }

    OpaqueElement ff_element_from_string(const char *s) {
        check_init();
        GF2E *e = allocGF2E();
        std::istringstream ss(s);
        ss >> *e;
        return (OpaqueElement)e;
    }

    const char *ff_element_to_string(OpaqueElement opaque_e) {
        check_init();
        GF2E *e = (GF2E *)opaque_e;
        std::ostringstream ss("");
        ss << *e;
        ss.clear();

        const std::string& tmp = ss.str();
        const char *s = tmp.c_str();

        char *r = (char *)malloc(strlen(s) + 1);
        assert(r != NULL);
        strcpy(r, s);
        return r;
    }

    OpaqueElement ff_one_element(void) {
        check_init();
        GF2E *one = allocGF2E();
        conv(*one, 1L);
        return (OpaqueElement)one;
    }

    OpaqueElement ff_add_elements(OpaqueElement opaque_l,
                                  OpaqueElement opaque_r) {
        check_init();
        GF2E *l = (GF2E *)opaque_l;
        GF2E *r = (GF2E *)opaque_r;
        GF2E *o = allocGF2E();
        *o = *l + *r;
        return (OpaqueElement)o;
    }

    OpaqueElement ff_sub_elements(OpaqueElement opaque_l,
                                  OpaqueElement opaque_r) {
        check_init();
        GF2E *l = (GF2E *)opaque_l;
        GF2E *r = (GF2E *)opaque_r;
        GF2E *o = allocGF2E();
        *o = *l - *r;
        return (OpaqueElement)o;
    }

    OpaqueElement ff_mul_elements(OpaqueElement opaque_l,
                                  OpaqueElement opaque_r) {
        check_init();
        GF2E *l = (GF2E *)opaque_l;
        GF2E *r = (GF2E *)opaque_r;
        GF2E *o = allocGF2E();
        *o = *l * *r;
        return (OpaqueElement)o;
    }

    OpaqueElement ff_div_elements(OpaqueElement opaque_l,
                                  OpaqueElement opaque_r) {
        check_init();
        GF2E *l = (GF2E *)opaque_l;
        GF2E *r = (GF2E *)opaque_r;
        GF2E *o = allocGF2E();
        *o = *l / *r;
        return (OpaqueElement)o;
    }

    OpaqueElement ff_invert_element(OpaqueElement opaque_e) {
        check_init();
        OpaqueElement one = ff_one_element();
        OpaqueElement inv = ff_div_elements(one, opaque_e);
        ff_free_element(one);
        return (OpaqueElement)inv;
    }

    int ff_equals(OpaqueElement opaque_l, OpaqueElement opaque_r) {
        check_init();
        GF2E *l = (GF2E *)opaque_l;
        GF2E *r = (GF2E *)opaque_r;

        return *l == *r;
    }

    OpaqueElement ff_element_from_bytes(const unsigned char *bytes,size_t len) {
        check_init();
        GF2X x;
        GF2E *e = allocGF2E();

        GF2XFromBytes(x, bytes, len);
        conv(*e, x);

        return (OpaqueElement)e;
    }

    char *ff_element_to_bytes(OpaqueElement opaque_e, size_t *len) {
        check_init();
        GF2E *e = (GF2E *)opaque_e;
        GF2X x = rep(*e);

        *len = 32;

        char *buf = (char *)malloc(*len);
        assert(NULL != buf);

        BytesFromGF2X((unsigned char *)buf, x, *len);
        return buf;
    }

    size_t ff_sizeof_element(void) {
        return sizeof(GF2E);
    }

    void ff_copy_element(UnsafeOpaqueElement opaque_dest,
                         OpaqueElement opaque_src) {
        GF2E *dest = (GF2E *)opaque_dest;
        GF2E *src = (GF2E *)opaque_src;

        memset(dest, 0, sizeof(GF2E));

#ifdef MEM_DEBUGGING
        cout << "Copying from " << opaque_src << " to " << opaque_dest << endl;
#endif

        *dest = *src;
        assert(src->_GF2E__rep.xrep.rep != dest->_GF2E__rep.xrep.rep);
    }

#ifdef __cplusplus
}
#endif
