#ifndef __UTIL_H
#define __UTIL_H

/**
 * Converts a char to a hex nibble. Assumes the input char is within [0-9a-fA-F],
 * otherwise returns 0. If using this, it's prudent to either check that your char is in that
 * range, or if zero is returned, check that the hex char specified is in fact '0'
 */
inline uint8_t
hex_to_nibble(char c) {
    if (c >= '0' && c <= '9') {
        return ((uint8_t) (c - '0'));
    } else if (c >= 'A' && c <= 'F') {
        return ((uint8_t) (c - 'A')) + 10;
    } else if (c >= 'a' && c <= 'f') {
        return ((uint8_t) (c - 'a')) + 10;
    } else {
        return 0;
    }
}

#endif /* __UTIL_H */
