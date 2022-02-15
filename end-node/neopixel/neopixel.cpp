#include "mbed.h"
#include "neopixel.h"

#define __nop() asm("nop")

NeoPixelOut::NeoPixelOut(PinName pin) : DigitalOut(pin)
{
     normalize = false;
     global_scale = 0.5f;
}

// The timing should be approximately 800ns/300ns, 300ns/800ns
void NeoPixelOut::byte(register uint32_t byte)
{
    for (int i = 0; i < 8; i++) {
        gpio_write(&gpio, 1);

        // duty cycle determines bit value
        if (byte & 0x80) {
            // one
            for(int j = 0; j < 12; j++) __nop();

            gpio_write(&gpio, 0);
            for(int j = 0; j < 4; j++) __nop();
        }
        else {
            // zero
            for(int j = 0; j < 4; j++) __nop();

            gpio_write(&gpio, 0);
            for(int j = 0; j < 12; j++) __nop();
        }

        byte = byte << 1; // shift to next bit
    }

}

void NeoPixelOut::send(Pixel *colors, uint32_t count, bool flipwait)
{
    // Disable interrupts in the critical section
    __disable_irq();

    Pixel* rgb;
    float fr,fg,fb;
    for (uint32_t i = 0; i < count; i++) {
        rgb = colors++;
        fr = (int)rgb->r;
        fg = (int)rgb->g;
        fb = (int)rgb->b;

        if (normalize) {
            float scale = 255.0f/(fr+fg+fb);
            fr *= scale;
            fg *= scale;
            fb *= scale;
        }

        fr *= global_scale;
        fg *= global_scale;
        fb *= global_scale;

        if (fr > 255) fr = 255;
        if (fg > 255) fg = 255;
        if (fb > 255) fb = 255;
        if (fr < 0) fr = 0;
        if (fg < 0) fg = 0;
        if (fb < 0) fb = 0;

        // Black magic to fix distorted timing
        #ifdef __HAL_FLASH_INSTRUCTION_CACHE_DISABLE
        __HAL_FLASH_INSTRUCTION_CACHE_DISABLE();
        #endif

        byte((int)fg);
        byte((int)fr);
        byte((int)fb);

        #ifdef __HAL_FLASH_INSTRUCTION_CACHE_ENABLE
        __HAL_FLASH_INSTRUCTION_CACHE_ENABLE();
        #endif
    }

    __enable_irq();

    if (flipwait) flip();
}


void NeoPixelOut::flip(void)
{
    wait_us(50);
}
