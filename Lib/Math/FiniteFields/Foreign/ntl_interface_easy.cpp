#include <sstream>

#include <string.h>
#include <assert.h>

#include <NTL/GF2E.h>
#include <NTL/GF2X.h>
#include <NTL/GF2XFactoring.h>

#include "ntl_interface_easy.h"

NTL_CLIENT

static bool initialized = false;

#ifdef __cplusplus
extern "C" {
#endif
    void ff_init(void) {
        GF2X P = BuildIrred_GF2X(256);
        GF2E::init(P);
    }

    inline void check_init(void) {
        if (!initialized) {
            ff_init();
            initialized = true;
        }
    }

    void ff_finalize(void) {
    }

    OpaqueElement ff_zero_element(void) {
        check_init();
        return &GF2E::zero();
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
            delete e;
        }
    }

    OpaqueElement ff_random_element(void) {
        check_init();
        GF2E *e = new GF2E();
        *e = random_GF2E();
        return e;
    }

    OpaqueElement ff_element_from_string(const char *s) {
        check_init();
        GF2E *e = new GF2E();
        stringstream ss(s);
        ss >> *e;
        return e;
    }

    const char *ff_element_to_string(OpaqueElement opaque_e) {
        check_init();
        GF2E *e = (GF2E *)opaque_e;
        stringstream ss("");
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
        GF2E *one = new GF2E();
        conv(*one, 1L);
        return one;
    }

    OpaqueElement ff_add_elements(OpaqueElement opaque_l,
                                  OpaqueElement opaque_r) {
        check_init();
        GF2E *l = (GF2E *)opaque_l;
        GF2E *r = (GF2E *)opaque_r;
        GF2E *o = new GF2E();
        *o = *l + *r;
        return o;
    }

    OpaqueElement ff_sub_elements(OpaqueElement opaque_l,
                                  OpaqueElement opaque_r) {
        check_init();
        GF2E *l = (GF2E *)opaque_l;
        GF2E *r = (GF2E *)opaque_r;
        GF2E *o = new GF2E();
        *o = *l - *r;
        return o;
    }

    OpaqueElement ff_mul_elements(OpaqueElement opaque_l,
                                  OpaqueElement opaque_r) {
        check_init();
        GF2E *l = (GF2E *)opaque_l;
        GF2E *r = (GF2E *)opaque_r;
        GF2E *o = new GF2E();
        *o = *l * *r;
        return o;
    }

    OpaqueElement ff_div_elements(OpaqueElement opaque_l,
                                  OpaqueElement opaque_r) {
        check_init();
        GF2E *l = (GF2E *)opaque_l;
        GF2E *r = (GF2E *)opaque_r;
        GF2E *o = new GF2E();
        *o = *l / *r;
        return o;
    }

    OpaqueElement ff_invert_element(OpaqueElement opaque_e) {
        check_init();
        OpaqueElement one = ff_one_element();
        OpaqueElement inv = ff_div_elements(one, opaque_e);
        ff_free_element(one);
        return inv;
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
        GF2E *e = new GF2E();

        GF2XFromBytes(x, bytes, len);
        conv(*e, x);

        return e;
    }

#ifdef __cplusplus
}
#endif
