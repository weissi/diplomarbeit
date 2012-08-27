#ifndef NTL_INTERFACE_EASY__H
#define NTL_INTERFACE_EASY__H

#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

    typedef const void * OpaqueElement;
    typedef const void * UnsafeOpaqueElement;

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

    OpaqueElement ff_element_from_bytes(const unsigned char *bytes, size_t len);

    size_t ff_sizeof_element(void);

    void ff_copy_element(UnsafeOpaqueElement dest, OpaqueElement src);

#ifdef __cplusplus
}
#endif

#endif
