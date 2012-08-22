#include <sstream>

#include <string.h>
#include <assert.h>

#include <NTL/GF2E.h>
#include <NTL/GF2X.h>
#include <NTL/GF2XFactoring.h>

#include "ntl_interface_easy.h"

NTL_CLIENT

#ifdef __cplusplus
extern "C" {
#endif
    void ff_init(void) {
        GF2X P = BuildIrred_GF2X(256);
        GF2E::init(P);
    }

    void ff_finalize(void) {
    }

    OpaqueElement ff_zero_element(void) {
        return &GF2E::zero();
    }

    void ff_print_element(OpaqueElement opaque_e) {
        GF2E *e = (GF2E *)opaque_e;
        cout << *e << endl;
    }

    void ff_free_element(OpaqueElement opaque_e) {
        if (opaque_e != ff_zero_element()) {
            GF2E *e = (GF2E *)opaque_e;
            delete e;
        }
    }

    OpaqueElement ff_random_element(void) {
        GF2E *e = new GF2E();
        *e = random_GF2E();
        return e;
    }

    OpaqueElement ff_element_from_string(const char *s) {
        GF2E *e = new GF2E();
        stringstream ss(s);
        ss >> *e;
        return e;
    }

    const char *ff_element_to_string(OpaqueElement opaque_e) {
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
        GF2E *one = new GF2E();
        conv(*one, 1L);
        return one;
    }

    OpaqueElement ff_add_elements(OpaqueElement opaque_l, OpaqueElement opaque_r) {
        GF2E *l = (GF2E *)opaque_l;
        GF2E *r = (GF2E *)opaque_r;
        GF2E *o = new GF2E();
        *o = *l + *r;
        return o;
    }

    OpaqueElement ff_sub_elements(OpaqueElement opaque_l, OpaqueElement opaque_r) {
        GF2E *l = (GF2E *)opaque_l;
        GF2E *r = (GF2E *)opaque_r;
        GF2E *o = new GF2E();
        *o = *l - *r;
        return o;
    }

    OpaqueElement ff_mul_elements(OpaqueElement opaque_l, OpaqueElement opaque_r) {
        GF2E *l = (GF2E *)opaque_l;
        GF2E *r = (GF2E *)opaque_r;
        GF2E *o = new GF2E();
        *o = *l * *r;
        return o;
    }

    OpaqueElement ff_div_elements(OpaqueElement opaque_l, OpaqueElement opaque_r) {
        GF2E *l = (GF2E *)opaque_l;
        GF2E *r = (GF2E *)opaque_r;
        GF2E *o = new GF2E();
        *o = *l / *r;
        return o;
    }

    OpaqueElement ff_invert_element(OpaqueElement opaque_e) {
        return ff_div_elements(ff_one_element(), opaque_e);
    }

    int ff_equals(OpaqueElement opaque_l, OpaqueElement opaque_r) {
        GF2E *l = (GF2E *)opaque_l;
        GF2E *r = (GF2E *)opaque_r;

        return *l == *r;
    }

    OpaqueElement ff_element_from_bytes(const char *bytes, size_t len) {
        GF2X *x = new GF2X();
        GF2E *e = new GF2E();

        GF2XFromBytes(*x, (const unsigned char *)bytes, len);
        conv(*e, *x);

        delete x;

        return e;
    }

#ifdef __cplusplus
}
#endif
