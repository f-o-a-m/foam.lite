/*
 * Copyright 2019 Virscient Limited - All Rights Reserved
 *
 * Unauthorised copying of this software in binary or source code form
 * via any medium is strictly prohibited. This software is proprietary
 * and confidential.
 *
 * For more information, email info@virscient.com
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
 * existing between you and Virscient
 */

#include <algorithm>

#include "mbed.h"
#include "sx1276-hal.h"

#include "ltsp.h"
#include "ATInterface.h"
#include "RNGInterface.h"
#include "Blockchain.h"

#if defined ( NEOPIXEL_ENABLED )
#include "neopixel/neopixel.h"
NeoPixelOut neopixel(NEOPIXEL);
Pixel neopixel_chain[1];
#endif

#if defined ( TARGET_NUCLEO_F303RE )

/** GPIO peripheral for our timestamping. */
#define TIMESTAMP_GPIO (GPIOB)
/** GPIO pin for our timestamping. */
#define TIMESTAMP_PIN (GPIO_PIN_11)
/** HAL macro to enable the clock to the GPIO driver used for the timestamping IRQ */
#define RCC_TIMESTAMP_GPIO_CLK_ENABLE __HAL_RCC_GPIOB_CLK_ENABLE
/** GPIO Alternate Function for TIM2 input capture */
#define TIMESTAMP_PIN_TIMER_ALTERNATE_FUNCTION GPIO_AF1_TIM2
/** Timer input capture channel for our timestamping. */
#define TIMESTAMP_CHANNEL (TIM_CHANNEL_4)
/** Timer input capture flag for our timestamping. */
#define TIMESTAMP_COMPARE_FLAG (TIM_FLAG_CC4)
/** Timer input capture overflow flag for our timestamping. */
#define TIMESTAMP_OVERFLOW_FLAG (TIM_FLAG_CC4OF)
/** Timer output compare channel for transmit timing. */
#define TRANSMIT_CHANNEL (TIM_CHANNEL_1)
/** Timer output compare flag for transmit timing. */
#define TRANSMIT_COMPARE_FLAG (TIM_FLAG_CC1)
/** Timer output compare interrupt flag for transmit timing. */
#define TRANSMIT_COMPARE_IT_FLAG (TIM_IT_CC1)

#elif defined ( TARGET_FEATHER_F405 )

/** GPIO peripheral for our timestamping. */
#define TIMESTAMP_GPIO (GPIOA)
/** GPIO pin for our timestamping. */
#define TIMESTAMP_PIN (GPIO_PIN_5)
/** HAL macro to enable the clock to the GPIO driver used for the timestamping IRQ */
#define RCC_TIMESTAMP_GPIO_CLK_ENABLE __HAL_RCC_GPIOA_CLK_ENABLE
/** GPIO Alternate Function for TIM2 input capture */
#define TIMESTAMP_PIN_TIMER_ALTERNATE_FUNCTION GPIO_AF1_TIM2
/** Timer input capture channel for our timestamping. */
#define TIMESTAMP_CHANNEL (TIM_CHANNEL_1)
/** Timer input capture flag for our timestamping. */
#define TIMESTAMP_COMPARE_FLAG (TIM_FLAG_CC1)
/** Timer input capture overflow flag for our timestamping. */
#define TIMESTAMP_OVERFLOW_FLAG (TIM_FLAG_CC1OF)
/** Timer output compare channel for transmit timing. */
#define TRANSMIT_CHANNEL (TIM_CHANNEL_4)
/** Timer output compare flag for transmit timing. */
#define TRANSMIT_COMPARE_FLAG (TIM_FLAG_CC4)
/** Timer output compare interrupt flag for transmit timing. */
#define TRANSMIT_COMPARE_IT_FLAG (TIM_IT_CC4)

#else
#error "No timestamp IRQ pin defined for the current platform"
#endif


/** Tracks the application state. */
volatile app_state_t state = APP_STATE_LOWPOWER;


/**
 * Radio events function pointer.
 */
static RadioEvents_t radio_events;


/**
 * SX1276 radio descriptor.
 */
SX1276MB1xAS radio(NULL);


/** Structure defining application data. */
typedef struct app_data_t
{
    /** Tracks RSSI value of last received packet. */
    int16_t rssi_value;

    /** Tracks SNR value of last received packet. */
    int8_t snr_value;

    /** Transmit data buffer. */
    uint8_t buffer[MAX_TRANSMIT_DATA_LENGTH];

    /** Length of data to transmit. */
    uint16_t tx_length;

    /** true if transmission was triggered by AT+TX, false if AT+TXDATA. */
	volatile bool was_attx;

    /** Tracks the number of remaining packets to send. */
    uint16_t packets_remaining;

    /** Array of timestamps for timing packets. */
    uint32_t timestamps[UINT8_MAX];

    /** Index into timestamps. */
    uint16_t timestamp_index;

    /* Timestamp of received packets. */
    uint32_t rx_time;

    /** Transmit interval. */
    uint32_t interval;

    /** True if our transmit timer expired while we're busy. */
    volatile bool tx_timeout;

    /** Radio parameters. */
    radio_params_t radio_params;

    /** Timer parameters. */
    timer_params_t timer_params;
} app_data_t;


/** Application data declaration and initialisation. */
static app_data_t app_data;


/** Tracks whether there is currently a command received which needs to be
 *  processed. */
static volatile bool cmd_for_processing = false;


/**
 * AT command interface command ready callback.
 */
static void
cmd_callback(void);


/**
 * AT command interface character received callback.
 */
static void
char_recv_callback(void);

/**
 * mBed DigitalIn pin to read echoback configuration on startup
 */
static DigitalIn at_interface_echoback_input(AT_INTERFACE_ECHOBACK_PIN);

/** AT command interface instantiation. */
static Blockchain blockchain;

#if defined ( TARGET_NUCLEO_F303RE )
#define AT_INTERFACE_ARGS USBTX, USBRX, AT_INTERFACE_BAUDRATE, \
                          cmd_callback, char_recv_callback, \
                          AT_INTERFACE_ECHO, &blockchain
#elif defined ( TARGET_FEATHER_F405 )
#define AT_INTERFACE_ARGS STDIO_UART_TX, STDIO_UART_RX, AT_INTERFACE_BAUDRATE, \
                          cmd_callback, char_recv_callback, \
                          AT_INTERFACE_ECHO, &blockchain
#else
#error Unsupported or no TARGET_ specified...
#endif
static ATInterface at_interface(AT_INTERFACE_ARGS);


#ifdef DEBUG_ENABLE
/** Debug serial port instantiation. */
//static Serial inst_debug_serial(DEBUG_PIN, NC, DEBUG_BAUDRATE);
RawSerial *debug_serial = &at_interface.serial; // &inst_debug_serial;
#endif


/** Timer declaration. */
static TIM_HandleTypeDef timer_instance;


/**
 * Loads the timer value saved by our input capture peripheral when the radio
 * signalled RxDone or TxDone.
 *
 * \returns the value of the timer when the radio signaled TxDone or RxDone.
 */
static uint32_t
get_radio_timestamp(void)
{
    uint32_t timestamp;

    /* Save the time the packet left the radio. */
    timestamp = __HAL_TIM_GET_COMPARE(&timer_instance, TIMESTAMP_CHANNEL);

    /*
     * Check for an overflow event. This can happen if the GPIO pin rises twice
     * before we get a chance to load the value. This should never happen,
     * because we have an interrupt handler on the GPIO rising edge where we
     * process the saved value.
     */
    if (__HAL_TIM_GET_FLAG(&timer_instance, TIMESTAMP_OVERFLOW_FLAG))
    {
        __HAL_TIM_CLEAR_FLAG(&timer_instance, TIMESTAMP_OVERFLOW_FLAG);
        DEBUG_RADIO("Input CC overflow!\n\r");
    }

    /* We need to remember to clear the flag ourselves, because we'll get an
     * overflow event if we forget! */
    __HAL_TIM_CLEAR_FLAG(&timer_instance, TIMESTAMP_COMPARE_FLAG);

    return timestamp;
}


/**
 * Passes the next packet over to the radio. If no packet is left to send, this
 * function will disable our transmission timer.
 */
static void
queue_next_packet(void)
{
    uint32_t next_time;

    /* Make sure our output compare doesn't generate any more interrupts. */
    __HAL_TIM_DISABLE_IT(&timer_instance, TRANSMIT_COMPARE_IT_FLAG);

    if (app_data.packets_remaining == 0)
    {
        DEBUG_RADIO("Transmit complete.\r\n");
        radio.Sleep();
        radio.Rx(0);
        state = APP_STATE_RX;
        return;
    }

    state = APP_STATE_TX;

    if (app_data.packets_remaining > 1)
    {
    	// have to do this before we actually send, sometimes the send can
		// happen fast enough that the txdone interrupt triggers before we decrement packets_remaining!
		app_data.packets_remaining--;
        app_data.buffer[0] = app_data.packets_remaining;

        /* Send the next timing packet.
             payload consists of a downcounting counter + 6 more bytes of the payload
             in order to uniquely identify the timing packet
        */
        radio.Sleep();
        radio.Send(app_data.buffer, uint8_t(std::min(app_data.tx_length, uint16_t(7))));

#ifdef DEBUG_ENABLE
        int i;
        DEBUG_RADIO("Sending Timing Packet %u: ",
                app_data.packets_remaining);
        for (i = 0; i < std::min(app_data.tx_length, uint16_t(7)); i++)
            DEBUG_RADIO("%02x", app_data.buffer[i]);
        DEBUG_RADIO("\r\n");
#endif
    }
    else if (app_data.packets_remaining == 1)
    {
    	// have to do this before we actually send, sometimes the send can
    	// happen fast enough that the txdone interrupt triggers before we decrement packets_remaining!
        app_data.packets_remaining--;
    	app_data.buffer[0] = app_data.packets_remaining; // =0

        /* Send our data packet. */
        if (app_data.was_attx) {
        	uint8_t* actual_buffer = &(app_data.buffer[1]);
        	uint8_t actual_length = app_data.tx_length - 1;
        	/* Don't send the countdown byte if we used AT+TX */
        	radio.Sleep();
//        	radio.Send(actual_buffer, actual_length);
        	radio.Send(app_data.buffer, uint8_t(app_data.tx_length));
        } else {
        	radio.Sleep();
        	radio.Send(app_data.buffer, uint8_t(app_data.tx_length));
        }

#ifdef DEBUG_ENABLE
		int i;
		DEBUG_RADIO("Sending Data Packet: ");

		for (i = app_data.was_attx ? 1 : 0; i < app_data.tx_length; i++)
			DEBUG_RADIO("%02x", app_data.buffer[i]);
		DEBUG_RADIO("\r\n");
#endif
    }
#ifdef DEBUG_ENABLE
    else {
    	DEBUG_RADIO("Somehow reached queue_next_packet with zero packets remaining ????");
    }
#endif

    /* Calculate the time of the next transmit - remembering to wrap with the
     * configured period! */
    next_time = ((__HAL_TIM_GET_COUNTER(&timer_instance) + app_data.interval) %
                 app_data.timer_params.period);

//    if (app_data.packets_remaining != 0) {
        /* Configure our timer to fire an interrupt for the next packet. */
        app_data.tx_timeout = false;
        __HAL_TIM_SET_COMPARE(&timer_instance, TRANSMIT_CHANNEL, next_time);
        __HAL_TIM_CLEAR_IT(&timer_instance, TRANSMIT_COMPARE_IT_FLAG);
        __HAL_TIM_ENABLE_IT(&timer_instance, TRANSMIT_COMPARE_IT_FLAG);
//    }
}


/**
 * Function to be executed on Radio Tx Done event.
 */
static void
handle_tx_done(void)
{
    /* Save the time we have stashed in our timer register. */
    app_data.timestamps[app_data.timestamp_index++] = get_radio_timestamp();

    /* Manually queue the next packet if we had a transmit timeout. This can
     * happen if the interval is smaller than the minimum time between
     * packets. */
    if (state == APP_STATE_TX && app_data.tx_timeout && app_data.packets_remaining != 0)
    {
    	DEBUG_RADIO("\r\n> OnTxDone ## Q_N_P\r\n");
        queue_next_packet();
        return;
    }

    DEBUG_RADIO("\r\n> OnTxDone: %d\r\n", radio.GetStatus());

    radio.Sleep();
    state = APP_STATE_LOWPOWER;

    if (app_data.packets_remaining == 0)
    {
        at_interface.packet_tx_complete(AT_ERROR_CODE_NO_ERROR,
                                        app_data.timestamps,
                                        app_data.timestamp_index,
										app_data.was_attx);
    }

    state = APP_STATE_RX;
    radio.Sleep();
    radio.Rx(0);
}


/**
 * Function executed on Radio Tx Timeout event.
 */
static void
handle_tx_timeout(void)
{
    radio.Sleep();
    state = APP_STATE_TX_TIMEOUT;
    DEBUG_RADIO("> OnTxTimeout\n\r");
    at_interface.packet_tx_complete(AT_ERROR_CODE_TIMEOUT, NULL, 0, app_data.was_attx);
    radio.Rx(0);
}


/**
 * Function to be executed on Radio Rx Done event.
 */
static void
handle_rx_done(uint8_t *payload, uint16_t size, int16_t rssi, int8_t snr)
{
    /* If we don't sleep the radio then the receive buffer doesn't get cleared
     * for some reason. */
    radio.Sleep();

    /* Save the time we have stashed in our timer register. */
    app_data.rx_time = get_radio_timestamp();

    if (size <= UINT8_MAX)
    {
        app_data.rssi_value = rssi;
        app_data.snr_value = snr;
        at_interface.packet_rx(payload, size, app_data.rx_time);
    }

    if (state != APP_STATE_TX)
        state = APP_STATE_RX;
    radio.Rx(0);
    DEBUG_RADIO("> OnRxDone snr:%d rssi:%d\n\r", app_data.snr_value, app_data.rssi_value);
}


/**
 * Function executed on Radio Rx Timeout event.
 */
static void
handle_rx_timeout(void)
{
    radio.Sleep();
    radio.Rx(0);
    state = APP_STATE_RX_TIMEOUT;
    DEBUG_RADIO("> OnRxTimeout\n\r");
}


/**
 * Function executed on Radio Rx Error event.
 */
static void
handle_rx_error(void)
{
    radio.Sleep();
    radio.Rx(0);
    state = APP_STATE_RX_ERROR;
    DEBUG_RADIO("> OnRxError\n\r");
}


/**
 * Configure the radio based on settings in \ref default_app_data.
 *
 * \returns \c true if the radio was configured successfully, else \c false.
 */
bool
configure_radio(radio_params_t *radio_params)
{
    DEBUG_RADIO("Configuring radio:\r\n"
                "    Frequency = %f\r\n"
                "    TX_Power = %d\r\n"
                "    LORA_BANDWIDTH = %u\r\n"
                "    LORA_DATARATE = %u\r\n"
                "    LORA_CODINGRATE = %u\r\n"
                "    LORA_PREAMBLE_LENGTH = %u\r\n"
                "    LORA_CRC_ENABLED = %d\r\n"
                "    TX_TIMEOUT_VALUE = %u\r\n",
                static_cast<float>(radio_params->frequency) / (1000 * 1000),
                radio_params->tx_power,
                radio_params->bandwidth,
                radio_params->datarate,
                radio_params->coding_rate,
                radio_params->preamble_length,
                radio_params->crc_enabled,
                TX_TIMEOUT_VALUE);

    if (!radio.CheckRfFrequency(radio_params->frequency))
    {
        DEBUG_RADIO("Frequency %f unsupported by radio\r\n",
                    static_cast<float>(radio_params->frequency) /
                        (1000 * 1000));
        return false;
    }

    radio.Sleep();
    /* Record the new radio parameters */
    memcpy(&app_data.radio_params, radio_params, sizeof(*radio_params));

    radio.SetChannel(radio_params->frequency);

    radio.SetTxConfig(MODEM_LORA, /* Modem type LoRa */
                      radio_params->tx_power,
                      0, /* Frequency deviation is FSK only  - set to 0 */
                      radio_params->bandwidth,
                      radio_params->datarate,
                      radio_params->coding_rate,
                      radio_params->preamble_length,
                      false, /* We don't want fixed length payloads */
                      radio_params->crc_enabled,
                      false, /* Frequency hopping disabled */
                      0, /* Number of symbols between each hop (ignored) */
                      false, /* Invert IQ symbols disabled */
                      TX_TIMEOUT_VALUE);

    radio.SetRxConfig(MODEM_LORA, /* Modem type LoRa */
                      radio_params->bandwidth,
                      radio_params->datarate,
                      radio_params->coding_rate,
                      0, /* AFC Bandwidth is FSK only  - set to 0 */
                      radio_params->preamble_length,
                      LORA_SYMBOL_TIMEOUT, /* Symbol timeout (in symbols) */
                      false, /* We don't want fixed length payloads */
                      0, /* Fixed length payloads aren't used - set to 0 */
                      radio_params->crc_enabled,
                      false, /* Frequency hopping disabled */
                      0, /* Number of symbols between each hop (ignored) */
                      false, /* Invert IQ symbols disabled */
                      true /* Set reception in continuous mode */);

    radio.SetMaxPayloadLength(MODEM_LORA, 255);

    radio.Sleep();
    radio.Rx(0);

    return true;
}


bool
send_packets(uint8_t timing_packets, uint32_t interval, uint8_t *data,
             unsigned length, bool was_attx)
{
#ifdef DEBUG_ENABLE
    unsigned int i;
    DEBUG_RADIO("send_packets(%d, %d, %p, %d, %s): ", timing_packets, interval, data, length, was_attx ? "AT+TX" : "AT+TXDATA");
    for (i = 0; i < length; i++) {
        DEBUG_RADIO("%02X", data[i]);
    }
    DEBUG_RADIO("\r\n");
#endif
    uint32_t ticks_per_ms;

    if (state == APP_STATE_TX)
        return false;

    if (length == 0) { return false; }

    /*
     * Round our interval up to at least 2 milliseconds. This gets around the
     * problem where the interval is shorter than the time it takes to write the
     * data to the radio.
     */
    ticks_per_ms = (TIMER_CLOCK_FREQUENCY /
                    (app_data.timer_params.prescaler + 1)) / 1000;

    if (interval < ticks_per_ms * 2)
        interval = ticks_per_ms * 2;

    app_data.interval = interval;

    /* Record number of timing packets */
    app_data.packets_remaining = timing_packets + 1;
    app_data.timestamp_index = 0;

    /* Store Data Packet - it starts with an octet with 0 in it. */
    memset(app_data.buffer, 0, sizeof(app_data.buffer));
    memcpy(&(app_data.buffer[1]), data, length);
    app_data.tx_length = length + 1;

    app_data.was_attx = was_attx;

    queue_next_packet();

    return true;
}


static void
cmd_callback(void)
{
    cmd_for_processing = true;
}


static void
char_recv_callback(void)
{
    at_interface.rx_handler();
}


extern "C"
void
SysTick_Handler(void)
{
    at_interface.send_internal_status();
    HAL_IncTick();
    HAL_SYSTICK_IRQHandler();
}

radio_params_t get_radio_configuration(void) {
    return app_data.radio_params;
}

timer_params_t get_timer_configuration(void) {
    return app_data.timer_params;
}

void
configure_timer(timer_params_t *timer_params)
{
    GPIO_InitTypeDef gpio;
    TIM_IC_InitTypeDef ic_config;
    TIM_OC_InitTypeDef oc_config;

    DEBUG_RADIO("Configuring timer:\r\n"
                "    Prescaler = %u\r\n"
                "    Period = %u\r\n",
                timer_params->prescaler,
                timer_params->period);

    memcpy(&app_data.timer_params, timer_params, sizeof(*timer_params));

    /* Prepare our input capture pin. */
    gpio.Pin = TIMESTAMP_PIN;
    gpio.Mode = GPIO_MODE_AF_PP;
    gpio.Pull = GPIO_NOPULL;
    gpio.Speed = GPIO_SPEED_FREQ_HIGH;
    gpio.Alternate = TIMESTAMP_PIN_TIMER_ALTERNATE_FUNCTION;
    HAL_GPIO_Init(TIMESTAMP_GPIO, &gpio);

    /* Initialise timer */
    timer_instance.Instance = TIM2;
    timer_instance.Init.Prescaler = timer_params->prescaler;
    timer_instance.Init.CounterMode = TIM_COUNTERMODE_UP;
    timer_instance.Init.Period = timer_params->period;
    timer_instance.Init.ClockDivision = TIM_CLOCKDIVISION_DIV1;
    timer_instance.Init.RepetitionCounter = 0;
    HAL_TIM_IC_Init(&timer_instance);

    /* Prepare our input capture channel */
    memset(&ic_config, 0, sizeof(ic_config));
    ic_config.ICPolarity = TIM_INPUTCHANNELPOLARITY_RISING;
    ic_config.ICSelection = TIM_ICSELECTION_DIRECTTI;
    ic_config.ICPrescaler = TIM_ICPSC_DIV1;
    ic_config.ICFilter = 0;
    HAL_TIM_IC_ConfigChannel(&timer_instance, &ic_config, TIMESTAMP_CHANNEL);

    /* Prepare the output capture peripheral to generate an interrupt when our
     * interval has passed. */
    oc_config.OCMode = TIM_OCMODE_TIMING;
    oc_config.Pulse = 0;
    oc_config.OCPolarity = TIM_OCPOLARITY_HIGH;
    oc_config.OCFastMode = TIM_OCFAST_DISABLE;
    HAL_TIM_OC_ConfigChannel(&timer_instance, &oc_config, TRANSMIT_CHANNEL);

    HAL_NVIC_SetPriority(TIM2_IRQn, 0, 0);
    HAL_NVIC_EnableIRQ(TIM2_IRQn);

    HAL_TIM_IC_Start(&timer_instance, TIMESTAMP_CHANNEL);
    HAL_TIM_OC_Start(&timer_instance, TRANSMIT_CHANNEL);
    DEBUG("Timer configured!\r\n");
}

int
main(void)
{
    //at_interface = ATInterface(AT_INTERFACE_ARGS);
    timer_params_t timer_params = TIMER_PARAMS_DEFAULTS;
    radio_params_t radio_params = RADIO_PARAMS_DEFAULTS;

    memset(&app_data, 0, sizeof(app_data));

    HAL_Init();

#if defined ( NEOPIXEL_ENABLED )
    neopixel.global_scale = 0.25f;
    neopixel.normalize = false;
    neopixel_chain[0].hex = 0x111111;
    neopixel.send(neopixel_chain, 1);
#endif

    DEBUG("\r\nDEBUG started\r\n");
    DEBUG("\n\n\r     %s\n\n\r", PROJECT_NAME);

#if defined ( TARGET_FEATHER_F405 )
    at_interface.set_echoback(true);
#else
    // the pin has an internal pull-up, and we want tying it to GND to enable echoback, hence negation
    bool read_echoback_pin = !((bool)at_interface_echoback_input.read());
    DEBUG("\r\nATInterface echoback is %s\r\n", read_echoback_pin ? "enabled" : "disabled");
    at_interface.set_echoback(read_echoback_pin);
#endif

    /* Initialise and configure timer */
    __HAL_RCC_TIM2_CLK_ENABLE();
    RCC_TIMESTAMP_GPIO_CLK_ENABLE();
    configure_timer(&timer_params);

    /* Initialise Radio driver */
    radio_events.TxDone = handle_tx_done;
    radio_events.RxDone = handle_rx_done;
    radio_events.RxError = handle_rx_error;
    radio_events.TxTimeout = handle_tx_timeout;
    radio_events.RxTimeout = handle_rx_timeout;
    radio.Init(&radio_events);

    DEBUG_AT_LINENUMBER("About to detect radio....\n\r");
    /* Verify the connection with the board */
    uint8_t radio_ver = radio.Read(REG_VERSION);
    if (radio_ver == 0x00)
    {
        DEBUG_RADIO("Radio could not be detected!\n\r");
        at_interface.send_reset_msg(VERSION_MAJOR, VERSION_MINOR, VERSION_PATCHLEVEL,
                                    AT_ERROR_CODE_HARDWARE_FAILURE);
        while (1)
        {
        }
    }
    DEBUG("Got radio version 0x%x", radio_ver);

    DEBUG_IF((DEBUG_RADIO_ENABLED & (radio.DetectBoardType() == SX1276MB1LAS)),
             "\n\r > Board Type: SX1276MB1LAS < \n\r");
    DEBUG_IF((DEBUG_RADIO_ENABLED & (radio.DetectBoardType() == SX1276MB1MAS)),
             "\n\r > Board Type: SX1276MB1MAS < \n\r");

    if (!configure_radio(&radio_params))
    {
        DEBUG("Radio could not be configured!\n\r");
        at_interface.send_reset_msg(VERSION_MAJOR, VERSION_MINOR, VERSION_PATCHLEVEL,
                                    AT_ERROR_CODE_HARDWARE_FAILURE);
        while (1)
        {
        }
    }

#if defined ( NEOPIXEL_ENABLED )
    neopixel_chain[0].hex = 0x0000FF;
    neopixel.send(neopixel_chain, 1);
#endif

    DEBUG("secp context size: %d\r\n", Blockchain::get_preallocated_context_size());

    at_interface.send_reset_msg(VERSION_MAJOR, VERSION_MINOR, VERSION_PATCHLEVEL,
                                AT_ERROR_CODE_NO_ERROR);

    DEBUG_AT("AT Parser started\r\n");
    while (1)
    {
        if (cmd_for_processing)
        {
#if defined ( NEOPIXEL_ENABLED )
    neopixel_chain[0].g += 30;
    neopixel_chain[0].r -= 45;
    neopixel.send(neopixel_chain, 1);
#endif
            at_interface.process_cmd_buffer();
            cmd_for_processing = false;
        } else {
#if defined ( NEOPIXEL_ENABLED )
    neopixel_chain[0].r += 1;
    neopixel_chain[0].b -= 1;
    if (neopixel_chain[0].r % 15 == 0)
    neopixel.send(neopixel_chain, 1);
#endif
        }
    }
}


/**
 * It's important the C++ compiler exports this symbol without name-mangling
 * otherwise our interrupt handler wont be linked in!
 */
extern "C"
{
void
TIM2_IRQHandler(void)
{
    if (__HAL_TIM_GET_FLAG(&timer_instance, TRANSMIT_COMPARE_FLAG))
    {
        __HAL_TIM_CLEAR_IT(&timer_instance, TRANSMIT_COMPARE_IT_FLAG);

        if (state == APP_STATE_TX)
        {
            app_data.tx_timeout = true;
        }
        else
        {
            queue_next_packet();
        }
    }
}
}
