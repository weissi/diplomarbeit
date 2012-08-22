#ifndef NTL_INTERFACE_EASY__H
#define NTL_INTERFACE_EASY__H

#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

    typedef const void * OpaqueElement;

    void ff_init(void);
    void ff_finalize(void);

    void ff_print_element(OpaqueElement e);
    OpaqueElement ff_random_element(void);

    void ff_free_element(OpaqueElement e);

    OpaqueElement ff_element_from_string(const char *s);
    const char *ff_element_to_string(OpaqueElement e);

    OpaqueElement ff_zero_element(void);
    OpaqueElement ff_one_element(void);

    OpaqueElement ff_add_elements(OpaqueElement l, OpaqueElement r);
    OpaqueElement ff_sub_elements(OpaqueElement l, OpaqueElement r);
    OpaqueElement ff_mul_elements(OpaqueElement l, OpaqueElement r);
    OpaqueElement ff_div_elements(OpaqueElement l, OpaqueElement r);
    OpaqueElement ff_invert_element(OpaqueElement e);

    int ff_equals(OpaqueElement l, OpaqueElement r);

    OpaqueElement ff_element_from_bytes(const char *bytes, size_t len);

#ifdef __cplusplus
}
#endif

#endif
