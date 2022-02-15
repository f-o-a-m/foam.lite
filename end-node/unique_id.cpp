#include "unique_id.h"

#if defined ( TARGET_NUCLEO_F303RE )
  #if !defined ( PLATFORM_UID_BASE )
    // via RM0316 Rev. 8 "Reference Manual STM32F303xB/C/D/E [, ...] ARM®-based MCUs"
    //          pp. 1121 "34. Device electronic signature"
    #define PLATFORM_UID_BASE  0x1FFFF7AC
  #endif

#elif defined ( TARGET_FEATHER_F405 )
  #if !defined ( PLATFORM_UID_BASE )
    // via RM0090 Rev. 19 "Reference Manual STM32F405/415, [...] advanced Arm®-based 32-bit MCUs"
    //          pp. 1714 "39. Device electronic signature"
    #define PLATFORM_UID_BASE  0x1FFF7A10
  #endif
#endif

#if !defined ( PLATFORM_UID_BASE )
  #error "PLATFORM_UID_BASE undefined, which is necessary for get_device_uid() functionality"
#else
uint32_t *platform_uid_base = ((uint32_t*)PLATFORM_UID_BASE);
#endif

device_uid_t get_device_uid() {
  device_uid_t ret = {
    .uid0 = platform_uid_base[0],
    .uid1 = platform_uid_base[1],
    .uid2 = platform_uid_base[2],
  };

  return ret;
}