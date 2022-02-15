/*
 * Copyright 2019 Virscient Limited - All Rights Reserved
 *
 * Unauthorised copying of this software in binary or source code form
 * via any medium is strictly prohibited. This software is proprietary
 * and confidential.
 *
 * This software is provided "as is", as a beta release.
 *
 * Use of beta software is at your own risk.  Virscient disclaims to
 * the fullest extent authorised by law any and all warranties,
 * whether express or implied, including, without limitation, any
 * implied warranties of merchantability or fitness for a particular
 * purpose.
 *
 * In no event shall Virscient be liable for any consequential,
 * incidental or special damages whatsoever arising out of the use
 * of or inability to use this software, even if the user has
 * advised Virscient of the possibility of such damages.
 *
 * This beta software, whether in source code, object code, or
 * binary form, and any log or trace files, or performance results
 * generated through its use, are to be treated as confidential
 * information under the terms of the confidentiality agreement
 * existing between you and Virscient.
 *
 * For more information, email info@virscient.com
 */


#ifndef LTSP_H__
#define LTSP_H__


#include "mbed.h"
#include "sx1276-hal.h"


/* ------------------------Application defines------------------------------ */


/** Project name. */
#define PROJECT_NAME    ("LoRa Timing Synchronisation Prototype")


/** Major version number. */
#define VERSION_MAJOR        (1)

/** Minor version number. */
#define VERSION_MINOR        (0)

/** Patch level number. */
#define VERSION_PATCHLEVEL   (0)


/* ------------------------AT interface defines----------------------------- */


/** AT command interface baudrate. */
#define AT_INTERFACE_BAUDRATE   (115200)


/** Set to \c true to enable serial port echo back to the host, else \c
 *  false. */
#define AT_INTERFACE_ECHO       (true)

/** Pin to use to enable/disable ATInterface echoback
 *  Note: the value is only read on startup, and the intended behavior is active-low
 *  (i.e., tie it to ground to enable echoback)
 *  I believe that mBed OS sets up an internal pull-up by default.
 */
#define AT_INTERFACE_ECHOBACK_PIN   (PC_3)

/* ---------------------------Debug output---------------------------------- */


/** If defined, this enables debug output. */
//#define DEBUG_ENABLE
/** Debug baudrate. */
#define DEBUG_BAUDRATE          (115200)
/** Debug output pin. */
#define DEBUG_PIN               (PC_4)
/** If set to 1, this enables debug output from the radio. Set to 0 to
 *  disable. */
#define DEBUG_RADIO_ENABLED     (1)
/** If set to 1, this enables debug output from the AT parser. Set to 0 to
 *  disable. This is a LOT of debug output, your best bet is to keep it off */
#define DEBUG_AT_ENABLED        (1)


#ifdef  DEBUG_ENABLE
/** Debug serial port descriptor - instantiated in main.cpp. */
extern RawSerial *debug_serial;

#define DEBUG(...) do \
{ \
    if (debug_serial != NULL) \
    { \
        debug_serial->printf(__VA_ARGS__); \
    } \
} while(0)

#define DEBUG_IF(x,...)     if (x) { DEBUG(__VA_ARGS__); }
#else
#define DEBUG(...)          do {} while(0)
#define DEBUG_IF(x,...)     do {} while(0)
#endif

/** Use for debug for the AT command parser. */
#define DEBUG_AT(...)       DEBUG_IF(DEBUG_AT_ENABLED, __VA_ARGS__)
/** Use for debug for the radio. */
#define DEBUG_RADIO(...)    DEBUG_IF(DEBUG_RADIO_ENABLED, __VA_ARGS__)

/** Used for simple debug statements that print the file and line number invoked from */
#define DEBUG_LINENUMBER(x) DEBUG(__FILE__ ":" MBED_STRINGIFY(__LINE__) ": " x "\r\n")
#define DEBUG_AT_LINENUMBER(x) DEBUG_AT(__FILE__ ":" MBED_STRINGIFY(__LINE__) ": " x "\r\n")
#define DEBUG_RADIO_LINENUMBER(x) DEBUG_AT(__FILE__ ":" MBED_STRINGIFY(__LINE__) ": " x "\r\n")

/* -------------------------Timer configuration------------------------------ */

#if defined ( TARGET_NUCLEO_F303RE )
/** Clock frequency into the timer. */
#define TIMER_CLOCK_FREQUENCY (144000000)
/** Default clock prescaler value. Clock is configured for 144MHz. */
#define PRESCALER_DEFAULT       (0)
/** Default clock period value. Timer wraps after reaching this value (currently
 * set to 10 seconds) */
#define CLOCK_PERIOD_DEFAULT     (10 * TIMER_CLOCK_FREQUENCY)
#elif defined ( TARGET_FEATHER_F405 )
/** Clock frequency into the timer. */
#define TIMER_CLOCK_FREQUENCY (84000000)
/** Default clock prescaler value. Clock is configured for 84MHz. */
#define PRESCALER_DEFAULT       (0)
/** Default clock period value. Timer wraps after reaching this value (currently
 * set to 10 seconds) */
#define CLOCK_PERIOD_DEFAULT     (10 * TIMER_CLOCK_FREQUENCY)
#else
#error Unsupported or no TARGET_ specified...
#endif


/** Structure defining timer parameters as used for timer configuration. */
typedef struct timer_params_t
{
    /** Prescaler. */
    uint32_t prescaler;

    /** Period value. */
    uint32_t period;
} timer_params_t;


/**
 * Initialisation macro for \ref timer_params_t. This can be used when
 * instantiating a \ref timer_params_t struct to ensure it is correctly
 * initialised to default values.
 *
 * Example usage:
 *     timer_params_t timer_params = TIMER_PARAMS_DEFAULTS;
 */
#define TIMER_PARAMS_DEFAULTS                                   \
        {                                                       \
            .prescaler = PRESCALER_DEFAULT,                     \
            .period = CLOCK_PERIOD_DEFAULT,                     \
        }


/**
 * Configure the timer with new parameters.
 *
 * \param timer_params  The timer parameters to use.
 */
extern void
configure_timer(timer_params_t *timer_params);

/**
 * Gets the timer's configuration parameters
 *
 * \returns a copy of the last set timer parameters
 */
extern timer_params_t
get_timer_configuration(void);


/* -------------------------Radio configuration------------------------------ */


/**
 * Default RF frequency in Hz.
 *
 * The ISM band for North America is from 902-928MHz. There are 64, 125kHz
 * uplink channels from 902.3 to 914.9MHz in 200kHz increments. There are
 * an additional eight 500KHz uplink channels in 1.6MHz increments from 903MHz
 * to 914.9MHz. The eight downlink channels are 500kHz wide starting from
 * 923.3MHz to 927.5MHz
 */
#define RF_FREQUENCY                                    (923300000)


/**
 * Default TX output power in dBm.
 *
 * \note the maximum output power in North America 902-928MHz band is +30dBm.
 */
#define TX_OUTPUT_POWER                                 (14)


/** Maximum TX output power in dBm as defined in VS-302847-SP-3 Section 5.2. */
#define MAX_TX_OUTPUT_POWER                             (20)


/**
 * LoRa bandwidth enumeration.
 *
 * As passed to SetRxConfig()/SetTxConfig() (radio.h)
 */
typedef enum lora_bandwidth_t
{
    /** 125 kHz. */
    LORA_BANDWIDTH_125_KHZ      = 0,
    /** 250 kHz. */
    LORA_BANDWIDTH_250_KHZ      = 1,
    /** 500 kHz. */
    LORA_BANDWIDTH_500_KHZ      = 2,
} lora_bandwidth_t;


/**
 * LoRa datarate enumeration.
 *
 * As passed to SetRxConfig()/SetTxConfig() (radio.h)
 */
typedef enum lora_datarate_t
{
    /** 64 chips. */
    LORA_DATARATE_64            = 6,
    /** 128 chips. */
    LORA_DATARATE_128           = 7,
    /** 256 chips. */
    LORA_DATARATE_256           = 8,
    /** 512 chips. */
    LORA_DATARATE_512           = 9,
    /** 1024 chips. */
    LORA_DATARATE_1024          = 10,
    /** 2048 chips. */
    LORA_DATARATE_2048          = 11,
    /** 4096 chips. */
    LORA_DATARATE_4096          = 12,
} lora_datarate_t;


/**
 * LoRa coding rate enumeration.
 *
 * As passed to SetRxConfig()/SetTxConfig() (radio.h)
 */
typedef enum lora_coding_rate_t
{
    /** 4/5. */
    LORA_CODING_RATE_4_5        = 1,
    /** 4/6. */
    LORA_CODING_RATE_4_6        = 2,
    /** 4/7. */
    LORA_CODING_RATE_4_7        = 3,
    /** 4/8. */
    LORA_CODING_RATE_4_8        = 4,
} lora_coding_rate_t;


/**
 * Default preamble length (in symbols) for transmit and receive. Note that
 * the hardware adds four more symbols on receive (see SetRxConfig() in
 * radio.h).
 */
#define LORA_PREAMBLE_LENGTH                        (8)


/**
 * Default symbol timeout (in symbols).
 */
#define LORA_SYMBOL_TIMEOUT                         (5)


/** Default transmit timeout value in milliseconds. */
#define TX_TIMEOUT_VALUE                            (2000)


/** Buffer size in octets. */
// todo: i dont think this is actually used anywhere... and was repeated in ltsp.c
// i dont think it somehow sets some `#define`s in upstream `#include`s since it's
// declared downstream of all includes...
#define BUFFER_SIZE                                 (128)


/** Enumeration of application states. */
typedef enum app_state_t
{
    APP_STATE_LOWPOWER = 0,
    APP_STATE_IDLE,

    APP_STATE_RX,
    APP_STATE_RX_TIMEOUT,
    APP_STATE_RX_ERROR,

    APP_STATE_TX,
    APP_STATE_TX_TIMEOUT
} app_state_t;


/** Structure defining radio parameters as used for radio configuration. */
typedef struct radio_params_t
{
    /** Channel frequency in Hz. */
    uint32_t frequency;

    /** Transmit output power in dBm. */
    int8_t tx_power;

    /** Bandwidth. */
    lora_bandwidth_t bandwidth;

    /** Datarate. */
    lora_datarate_t datarate;

    /** Coding rate. */
    lora_coding_rate_t coding_rate;

    /**
     * Preamble length (in symbols) for transmit and receive. Note that
     * the hardware adds four more symbols on receive (see SetRxConfig() in
     * radio.h).
     */
    uint16_t preamble_length;

    /** CRC enable (true for enabled, false for disabled). */
    bool crc_enabled;
} radio_params_t;


/**
 * Initialisation macro for \ref radio_params_t. This can be used when
 * instantiating a \ref radio_params_t struct to ensure it is correctly
 * initialised to default values.
 *
 * Example usage:
 *     radio_params_t radio_params = RADIO_PARAMS_DEFAULTS;
 */
#define RADIO_PARAMS_DEFAULTS                                   \
        {                                                       \
            .frequency = RF_FREQUENCY,                          \
            .tx_power = TX_OUTPUT_POWER,                        \
            .bandwidth = LORA_BANDWIDTH_500_KHZ,                \
            .datarate = LORA_DATARATE_128,                      \
            .coding_rate = LORA_CODING_RATE_4_5,                \
            .preamble_length = LORA_PREAMBLE_LENGTH,            \
            .crc_enabled = true,                                \
        }


/**
 * Configure the radio with new parameters.
 *
 * \param radio_params  The radio parameters to use.
 *
 * \returns \c true if the radio was configured successfully, else \c false.
 */
extern bool
configure_radio(radio_params_t *radio_params);


/**
 * Gets the radio's configuration parameters
 *
 * \returns a copy of the last set radio parameters
 */
extern radio_params_t
get_radio_configuration(void);


/**
 * Initiate sending of Timing and Data Packets.
 *
 * \param timing_packets    The number of timing packets to send.
 * \param interval          The interval between packets. TODO: units
 * \param data              Pointer to data to send.
 * \param length            Length of \p data.
 * \param was_attx          true if this transmission was initiated by AT+TX, false if AT+TXDATA
 *
 * \returns \c true if packet sending was started successfully, else \c false
 *          (i.e. busy).
 */
extern bool
send_packets(uint8_t timing_packets, uint32_t interval, uint8_t *data,
             unsigned length, bool was_attx);


/** Maximum AT+TXDATA data length as defined in VS-302847-SP-3 Section 5.4. */
#define MAX_AT_TXDATA_LENGTH            (460)

/** Maximum supported AT+KECCAK256 data length. */
#define MAX_AT_KECCAK_LENGTH            (8192)

// 230 max packet length is our target, given some chosen DR/BW bandwidth
// theoretical max is 250, but we're giving ourself margin
// note that most settings only allow for up to 130 bytes payload length

/** Maximum transmit data length, once the AT+TXDATA has been encoded.
 *  One is added for the leading 0x00 identifier octet. */
#define MAX_TRANSMIT_DATA_LENGTH        ((MAX_AT_TXDATA_LENGTH / 2) + 1)


/** Maximum RXDATA data length as defined in VS-302847-SP-3 Section 6.2. */
#define MAX_RXDATA_LENGTH               (MAX_TRANSMIT_DATA_LENGTH * 2)


#endif /* LTSP_H__ */
