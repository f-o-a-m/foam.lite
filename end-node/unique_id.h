/**
 * Provides a common interface for extracting platform-specific unique device identifiers.
 * In the case of the STM32F303RE we used in the LTSP, these live as 96bits (3x 32-bit values)
 * starting at address 0x1FFF7A10. 
 * 
 * This interface looks for TARGET_whatever to try and attempt to set PLATFORM_UID_BASE
 * if it isn't set already. Naturally, this means you can readily override it by compiling
 * with -DPLATFORM_UID_BASE or simply #define'ing it prior to including "unique_id.h"
 */

#ifndef UNIQUE_ID_H__
#define UNIQUE_ID_H__

#include <stdint.h>

typedef struct {
  uint32_t uid0;
  uint32_t uid1;
  uint32_t uid2;
} device_uid_t;

device_uid_t get_device_uid();

#endif /* UNIQUE_ID_H__ */