#ifndef RNG_INTERFACE_H__
#define RNG_INTERFACE_H__

#include "stdint.h"

class RNGInterface
{
  public:
    RNGInterface(void);
    ~RNGInterface(void);
    void init(void);
    void deinit(void);

    bool is_ready(void);

    void seed(uint32_t);
    uint32_t get(void);
};

extern RNGInterface rng;

#endif /* RNG_INTERFACE_H__ */
