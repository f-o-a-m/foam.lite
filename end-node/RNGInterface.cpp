#include "RNGInterface.h"

#if defined ( TARGET_FEATHER_F405 )
#include "stm32f4xx.h"
#define RNG_READY (RNG->SR & RNG_SR_DRDY)
#else
#include <stdlib.h>
#define RNG_READY (true)
#endif

RNGInterface rng;

RNGInterface::RNGInterface() {
#if defined ( TARGET_FEATHER_F405 )
  // Enable RNG clock source
	RCC->AHB2ENR |= RCC_AHB2ENR_RNGEN;

	// Enable RNG Peripheral enable
	RNG->CR |= RNG_CR_RNGEN;
#endif
}
RNGInterface::~RNGInterface() {
#if defined ( TARGET_FEATHER_F405 )
    // Enable RNG Peripheral enable
    RNG->CR &= ~RNG_CR_RNGEN;
    // Disable RNG clock source
    RCC->AHB2ENR &= ~RCC_AHB2ENR_RNGEN;
#endif
}

void RNGInterface::seed(uint32_t s)
{
#if defined ( TARGET_FEATHER_F405 )
#else
  srand(s);
#endif
}

bool RNGInterface::is_ready(void)
{
  return RNG_READY;
}

uint32_t RNGInterface::get(void)
{
#if defined ( TARGET_FEATHER_F405 )
    while (!RNG_READY) {}
    return RNG->DR;
#else
    return rand();
#endif
}
