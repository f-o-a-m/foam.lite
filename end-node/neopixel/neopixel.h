#ifndef NEOPIXEL_H
#define NEOPIXEL_H
#include "mbed.h"

/*
// Example

NeoPixelOut npx(D12);

int main() {
    wait(0.2); // wait for HSE to stabilize

    npx.global_scale = 1.0f; // Adjust brightness
    npx.normalize = true; // Equalize brightness to make r + g + b = 255

    Pixel strip[6];
    strip[0].hex = 0xFF0000;
    strip[1].hex = 0xFFFF00;
    strip[2].hex = 0x00FF00;
    strip[3].hex = 0x00FFFF;
    strip[4].hex = 0x0000FF;
    strip[5].hex = 0xFF00FF;

    npx.send(strip, 6);

    while(1);
}
*/



/**
 * @brief Struct for easy manipulation of RGB colors.
 *
 * Set components in the xrgb.r (etc.) and you will get
 * the hex in xrgb.num.
 */
union Pixel {
    /** Struct for access to individual color components */
    struct __attribute__((packed)) {
        uint8_t b;
        uint8_t g;
        uint8_t r;
        uint8_t a; // unused
    };

    /** RGB color as a single uint32_t */
    uint32_t hex;
};


class NeoPixelOut : DigitalOut {
private:
    void byte(uint32_t b);

public:
    bool normalize;
    float global_scale;

    NeoPixelOut(PinName pin);

    void send(Pixel *colors, uint32_t count, bool flipwait=true);

    /** Wait long enough to make the colors show up */
    void flip(void);
};


#endif /* NEOPIXEL_H */