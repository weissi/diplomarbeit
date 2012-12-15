/*******************************************************************************
 *  This file is part of diplomarbeit ("Diplomarbeit Johannes Weiß").          *
 *                                                                             *
 *  diplomarbeit is free software: you can redistribute it and/or modify       *
 *  it under the terms of the GNU General Public License as published by       *
 *  the Free Software Foundation, either version 3 of the License, or          *
 *  (at your option) any later version.                                        *
 *                                                                             *
 *  diplomarbeit is distributed in the hope that it will be useful,            *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *
 *  GNU General Public License for more details.                               *
 *                                                                             *
 *  You should have received a copy of the GNU General Public License          *
 *  along with diplomarbeit.  If not, see <http://www.gnu.org/licenses/>.      *
 *                                                                             *
 *  Copyright 2012, Johannes Weiß                                              *
 ******************************************************************************/

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
    char *ff_element_to_bytes(OpaqueElement opaque_e, size_t *len);

    size_t ff_sizeof_element(void);

    void ff_copy_element(UnsafeOpaqueElement dest, OpaqueElement src);

#ifdef __cplusplus
}
#endif

#endif
