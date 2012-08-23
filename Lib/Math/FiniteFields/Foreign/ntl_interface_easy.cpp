#include <sstream>
#include <tr1/memory>

#include <string.h>
#include <assert.h>
#include <limits.h>

#include <NTL/GF2E.h>
#include <NTL/GF2X.h>
#include <NTL/GF2XFactoring.h>

#include <stdio.h>

#include "ntl_interface_easy.h"

NTL_CLIENT

typedef std::tr1::shared_ptr<GF2E> GF2ESmartPtr;

GF2ESmartPtr *cachedInstances = NULL;
long initializedToPower = -1;

#ifdef __cplusplus
extern "C" {
#endif
    void doNotDeleteGF2E(GF2E *p) {
    }

    void deleteGF2E(GF2E *p) {
        delete p;
    }

    GF2ESmartPtr *newGF2E() {
        GF2ESmartPtr *p = new std::tr1::shared_ptr<GF2E>(new GF2E(), deleteGF2E);
        return p;
    }

    GF2ESmartPtr *newGF2EReference(GF2ESmartPtr *e) {
        GF2ESmartPtr *p = new std::tr1::shared_ptr<GF2E>(*e);
        return p;
    }

    GF2ESmartPtr *_getCachedElement(unsigned char e) {
        GF2ESmartPtr *p = new std::tr1::shared_ptr<GF2E>(cachedInstances[e]);
        return p;
    }

    GF2ESmartPtr *_real_element_from_bytes(const unsigned char *bytes, size_t len) {
        if (len == 0) {
            GF2ESmartPtr *p = new std::tr1::shared_ptr<GF2E>((GF2E*)&GF2E::zero(),
                                                             doNotDeleteGF2E);
            return p;
        } else {
            GF2ESmartPtr *e = NULL;

            GF2X x;
            e = newGF2E();

            GF2XFromBytes(x, bytes, len);
            conv(*e->get(), x);

            return e;
        }
    }

    GF2ESmartPtr *_element_from_bytes(const unsigned char *bytes, size_t len) {
        if (len == 0) {
            return _getCachedElement(0x00);
        } else if (len == 1) {
            return _getCachedElement(bytes[0]);
        } else {
            return _real_element_from_bytes(bytes, len);
        }
    }

    void ff_init(void) {
        GF2X P = BuildIrred_GF2X(256);
        GF2E::init(P);

        if (cachedInstances == NULL) {
            cachedInstances = new GF2ESmartPtr[UCHAR_MAX + 1];
            for (int i=0; i<=UCHAR_MAX; i++) {
                const unsigned char c = (const unsigned char)i;
                GF2ESmartPtr *pPtr = _real_element_from_bytes(&c, sizeof(c));
                GF2ESmartPtr p = *pPtr;
                delete pPtr;

                cachedInstances[i] = p;
            }
            initializedToPower = 256;
        }
    }

    inline void checkInit(void) {
        if (0 > initializedToPower) {
            ff_init();
        }
    }

    OpaqueElement ff_zero_element(void) {
        checkInit();
        return _getCachedElement(0x00);
    }

    void ff_print_element(OpaqueElement opaque_e) {
        checkInit();
        GF2ESmartPtr *e = (GF2ESmartPtr *)opaque_e;
        cout << *e->get() << endl;
    }

    void ff_free_element(OpaqueElement opaque_e) {
        checkInit();
        GF2ESmartPtr *e = (GF2ESmartPtr *)opaque_e;
        if ( e->use_count() > 1) {
        }
        delete e;
    }

    OpaqueElement ff_random_element(void) {
        checkInit();
        GF2ESmartPtr *e = newGF2E();
        *e->get() = random_GF2E();
        return e;
    }

    OpaqueElement ff_element_from_string(const char *s) {
        checkInit();
        GF2ESmartPtr *e = newGF2E();
        stringstream ss(s);
        ss >> *e->get();
        return e;
    }

    const char *ff_element_to_string(OpaqueElement opaque_e) {
        checkInit();
        GF2ESmartPtr *e = (GF2ESmartPtr *)opaque_e;
        stringstream ss("");
        ss << *e->get();
        ss.clear();

        const std::string& tmp = ss.str();
        const char *s = tmp.c_str();

        char *r = (char *)malloc(strlen(s) + 1);
        assert(r != NULL);
        strcpy(r, s);
        return r;
    }

    OpaqueElement ff_one_element(void) {
        checkInit();
        return _getCachedElement(0x01);
    }

    OpaqueElement ff_add_elements(OpaqueElement opaque_l, OpaqueElement opaque_r) {
        checkInit();
        GF2ESmartPtr *l = (GF2ESmartPtr *)opaque_l;
        GF2ESmartPtr *r = (GF2ESmartPtr *)opaque_r;

        if (l->get() == cachedInstances[0].get()) {
            return newGF2EReference(r);
        }

        if (r->get() == cachedInstances[0].get()) {
            return newGF2EReference(l);
        }

        GF2ESmartPtr *o = newGF2E();
        *(o->get()) = *l->get() + *r->get();
        return o;
    }

    OpaqueElement ff_sub_elements(OpaqueElement opaque_l, OpaqueElement opaque_r) {
        checkInit();
        GF2ESmartPtr *l = (GF2ESmartPtr *)opaque_l;
        GF2ESmartPtr *r = (GF2ESmartPtr *)opaque_r;

        if (r->get() == cachedInstances[0].get()) {
            return newGF2EReference(l);
        }

        GF2ESmartPtr *o = newGF2E();
        *o->get() = *l->get() - *r->get();
        return o;
    }

    OpaqueElement ff_mul_elements(OpaqueElement opaque_l, OpaqueElement opaque_r) {
        checkInit();
        GF2ESmartPtr *l = (GF2ESmartPtr *)opaque_l;
        GF2ESmartPtr *r = (GF2ESmartPtr *)opaque_r;

        if (l->get() == cachedInstances[1].get()) {
            return newGF2EReference(r);
        }

        if (r->get() == cachedInstances[1].get()) {
            return newGF2EReference(l);
        }

        GF2ESmartPtr *o = newGF2E();
        *o->get() = *l->get() * *r->get();
        return o;
    }

    OpaqueElement ff_div_elements(OpaqueElement opaque_l, OpaqueElement opaque_r) {
        checkInit();
        GF2ESmartPtr *l = (GF2ESmartPtr *)opaque_l;
        GF2ESmartPtr *r = (GF2ESmartPtr *)opaque_r;

        if (r->get() == cachedInstances[1].get()) {
            return newGF2EReference(l);
        }

        GF2ESmartPtr *o = newGF2E();
        *o->get() = *l->get() / *r->get();
        return o;
    }

    OpaqueElement ff_invert_element(OpaqueElement opaque_e) {
        checkInit();
        OpaqueElement one = ff_one_element();
        OpaqueElement inv = ff_div_elements(one, opaque_e);
        ff_free_element(one);
        return inv;
    }

    int ff_equals(OpaqueElement opaque_l, OpaqueElement opaque_r) {
        checkInit();
        GF2ESmartPtr *l = (GF2ESmartPtr *)opaque_l;
        GF2ESmartPtr *r = (GF2ESmartPtr *)opaque_r;

        if (l == NULL || r == NULL || l->get() == NULL || r->get() == NULL) {
            return 0;
        }

        if (l->get() == r->get()) {
            return 1;
        }

        return *l->get() == *r->get();
    }

    OpaqueElement ff_element_from_bytes(const unsigned char *bytes, size_t len) {
        checkInit();

        return _element_from_bytes(bytes, len);
    }

#ifdef __cplusplus
}
#endif
