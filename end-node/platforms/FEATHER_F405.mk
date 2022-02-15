OBJECTS += neopixel/neopixel.o

# These are our overrides over the precompiled HAL, lol
OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TARGET_STM/TARGET_STM32F4/TARGET_STM32F405/device/system_stm32f4xx.o
OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TARGET_STM/TARGET_STM32F4/TARGET_STM32F405/device/system_clock.o

# CCM experiment, commented for the time being
OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/startup_STM32F40x.o

# Vendored startup code
# SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/vendored_startup_STM32F40x.o

SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/PeripheralPins.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/analogin_api.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/analogin_device.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/analogout_api.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/analogout_device.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/can_api.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/flash_api.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/gpio_api.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/gpio_irq_api.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/gpio_irq_device.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/hal_init_pre.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/i2c_api.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/lp_ticker.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/mbed_board.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/mbed_crc_api.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/mbed_overrides.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/mbed_retarget.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/mbed_sdk_boot.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/mbed_tz_context.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/pinmap.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/port_api.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/pwmout_api.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/pwmout_device.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/rtc_api.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/serial_api.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/serial_device.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/sleep.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/spi_api.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/stm_spi_api.o
SYS_OBJECTS += mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/trng_api.o

OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_rcc.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_rcc_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_uart.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_usart.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_rcc.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_usart.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_utils.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_adc.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_adc_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_can.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/Legacy/stm32f4xx_hal_can.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_cec.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_cortex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_crc.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_cryp.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_cryp_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_dac.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_dac_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_dcmi.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_dcmi_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_dfsdm.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_dma.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_dma2d.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_dma_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_dsi.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_eth.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_flash.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_flash_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_flash_ramfunc.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_fmpi2c.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_fmpi2c_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_gpio.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_hash.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_hash_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_hcd.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_i2c.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_i2c_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_i2s.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_i2s_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_irda.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_iwdg.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_lptim.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_ltdc.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_ltdc_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_mmc.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_nand.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_nor.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_pccard.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_pcd.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_pcd_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_pwr.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_pwr_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_qspi.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_rcc_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_rng.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_rtc.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_rtc_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_sai.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_sai_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_sd.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_sdram.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_smartcard.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_spdifrx.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_spi.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_sram.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_tim.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_tim_ex.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_uart.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_usart.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_hal_wwdg.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_adc.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_crc.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_dac.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_dma.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_dma2d.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_exti.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_fmc.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_fsmc.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_gpio.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_i2c.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_lptim.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_pwr.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_rng.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_rtc.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_sdmmc.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_spi.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_tim.o
OBJECTS += stm32-hal/f4xx-hal-v1.25.0/Src/stm32f4xx_ll_usb.o

INCLUDE_PATHS += -I../stm32-hal/f4xx-hal-v1.25.0/Inc
INCLUDE_PATHS += -I../mbed/targets/TARGET_STM/TARGET_FEATHER_F405
INCLUDE_PATHS += -I../mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TARGET_STM
INCLUDE_PATHS += -I../mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TARGET_STM/TARGET_STM32F4
INCLUDE_PATHS += -I../mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TARGET_STM/TARGET_STM32F4/TARGET_STM32F405
INCLUDE_PATHS += -I../mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TARGET_STM/TARGET_STM32F4/TARGET_STM32F405/TARGET_FEATHER_F405
INCLUDE_PATHS += -I../mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TARGET_STM/TARGET_STM32F4/TARGET_STM32F405/device
INCLUDE_PATHS += -I../mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TARGET_STM/TARGET_STM32F4/device

INCLUDE_PATHS += -I../neopixel


LIBRARY_PATHS := -L../mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM
LINKER_SCRIPT ?= ../mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TOOLCHAIN_GCC_ARM/STM32F405.ld

C_FLAGS += -DTARGET_FEATHER_F405
C_FLAGS += -DNEOPIXEL_ENABLED

CXX_FLAGS += -DTARGET_FEATHER_F405
CXX_FLAGS += -DNEOPIXEL_ENABLED

TARGET_USES_DFU := true
TARGET_DFU_VID := 0483
TARGET_DFU_PID := df11
TARGET_DFU_ALT_ID := 0
TARGET_DFU_DFUSE_ADDR := 0x8000000


ASM_FLAGS += -I../mbed/targets/TARGET_STM/TARGET_FEATHER_F405
ASM_FLAGS += -I../mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TARGET_STM
ASM_FLAGS += -I../mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TARGET_STM/TARGET_STM32F4
ASM_FLAGS += -I../mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TARGET_STM/TARGET_STM32F4/TARGET_STM32F405
ASM_FLAGS += -I../mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TARGET_STM/TARGET_STM32F4/TARGET_STM32F405/TARGET_FEATHER_F405
ASM_FLAGS += -I../mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TARGET_STM/TARGET_STM32F4/TARGET_STM32F405/device
ASM_FLAGS += -I../mbed/targets/TARGET_STM/TARGET_FEATHER_F405/TARGET_STM/TARGET_STM32F4/device

C_FLAGS += -DTARGET_FF_FEATHER
C_FLAGS += -DTARGET_FAMILY_STM32
C_FLAGS += -DTARGET_STM32F405xG
C_FLAGS += -DTARGET_STM32F4
C_FLAGS += -DTARGET_STM32F405RG

CXX_FLAGS += -DTARGET_FF_FEATHER
CXX_FLAGS += -DTARGET_FAMILY_STM32
CXX_FLAGS += -DTARGET_STM32F405xG
CXX_FLAGS += -DTARGET_STM32F4
CXX_FLAGS += -DTARGET_STM32F405RG
