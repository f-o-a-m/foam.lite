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


#ifndef ATINTERFACE_H__
#define ATINTERFACE_H__


#include "ltsp.h"


#include "mbed.h"
#include "sx1276-hal.h"
#include "drivers/RawSerial.h"

#include "Blockchain.h"
#include "RNGInterface.h"

#include "util.h"


/** AT parser buffer size. */
#define AT_BUFFER_SIZE          (1024)
/** Command parse buffer size. */
#define CMD_PARSE_BUFFERSIZE    (AT_BUFFER_SIZE)

/**
* converts an ASCII hex digit into a nibble by doing ASCII math
* strtol(char, NULL, 16) appears to be buggy??? (sometimes itll return 0x<digit>B instead of 0x<digit>)
* this is also probably way more optimized for a single nibble than strtol anyway...
*
* \param c          Buffer containing the parameter string.
*
* \returns the value of the ASCII hex digit its the actual value, or 0 if invalid
*/
uint8_t hex_to_nibble(char c);

extern RNGInterface rng;


/** Enumerate AT error codes. See Transceiver AT Command Interface
 *  Specification (VS-302847-SP-2) Section 4.2. */
typedef enum at_error_code_t
{
    /** Hardware failure. */
    AT_ERROR_CODE_HARDWARE_FAILURE      = 0,
    /** Invalid command parameters. */
    AT_ERROR_CODE_INVALID_PARAMETERS    = 1,
    /** Transmit error. */
    AT_ERROR_CODE_TRANSMIT_ERROR        = 2,
    /** Timeout. */
    AT_ERROR_CODE_TIMEOUT               = 3,

	/** at error codes caused by blockchain subsystem errors are added to this error code. */
	AT_ERROR_CODE_BLOCKCHAIN_ERROR_BASE = 100,

	/** Unimplemented/unsupported. */
	AT_ERROR_CODE_UNSUPPORTED           = 0xF0,
    /** No error. */
    AT_ERROR_CODE_NO_ERROR              = 0xff,
} at_error_code_t;

/**
 * Parsing the params for the foam.lite related commands is very similar,
 * so we have a common handler function -- this flag is passed to the respective
 * methods to determine what we do with the message that we parse from the params...
 */
typedef enum at_foam_lite_op_t
{
	FOAM_LITE_HASH_ONLY,
	FOAM_LITE_SIGN_ONLY,
	FOAM_LITE_SIGN_AND_TX,
} at_foam_lite_op_t;


/** AT interface callback prototype. */
typedef void (*at_interface_callback_t)(void);


/**
 * Class defining an AT command interface.
 */
class ATInterface
{
    public:
        /**
         * AT interface constructor.
         *
         * \param tx                    Serial port transmit pin.
         * \param rx                    Serial port receive pin.
         * \param baudrate              Serial port baudrate.
         * \param cmd_ready_callback    Command ready callback. This is invoked
         *                              from an interrupt context to indicate
         *                              that a command is ready to be processed.
         *                              The application should invoke \ref
         *                              ATInterface::process_cmd_buffer() from
         *                              the application thread to process the
         *                              command.
         * \param char_recv_callback    Character receive callback. This is
         *                              invoked from an interrupt context to
         *                              indicate that a character has been
         *                              received and is ready to be processed.
         *                              The application should invoke \ref
         *                              ATInterface::rx_handler() from this
         *                              callback.
         * \param echo                  Set to \c true to enable echo, else \c
         *                              false.
         * \param blockchain            Pointer to an instance of Blockchain
         * \param rng                   Pointer to an instance of RNGInterface
         */
        ATInterface(PinName tx, PinName rx, int baudrate,
                    at_interface_callback_t cmd_ready_callback,
                    at_interface_callback_t char_recv_callback,
                    bool echo, Blockchain* blockchain);

        /**
         * Handle received data.
         *
         * \note This should be invoked from an interrupt context when a
         *       character is received via the AT serial port. I.e. this
         *       should be invoked from the char_recv_callback since C++/mbed
         *       does not allow the member function of a object to be used
         *       as the callback from a RawSerial object.
         */
        void rx_handler(void);

        /**
         * Process any pending commands.
         *
         * \note Must not be invoked from an interrupt context.
         */
        void process_cmd_buffer(void);

        /**
         * Send a RESET message to the host.
         *
         * \param major     Major version number.
         * \param minor     Minor version number.
         * \param patch     Patch level number
         * \param error     Relevant error code, or \ref AT_ERROR_CODE_NO_ERROR
         *                  if the reset was successful.
         */
        void send_reset_msg(uint8_t major,
                            uint8_t minor,
                            uint8_t patch,
                            at_error_code_t error);

        /**
         * Called by the application to indicate completion of packet
         * transmission.
         *
         * \param error         Relevant error code, or
         *                      \ref AT_ERROR_CODE_NO_ERROR if packet
         *                      transmission was successful.
         * \param timestamps    Pointer to an array containing timestamps of the
         *                      sent Data Packets.
         * \param length        Length of the timestamp array.
         * \param was_attx    true if the packet tx was triggered by AT+TX, false if by AT+TXDATA
         */
        void packet_tx_complete(at_error_code_t error, uint32_t *timestamps,
                                uint16_t length, bool was_attx);

        /**
         * Called by the application to indicate reception of a valid packet.
         *
         * \note Valid packets are either Timing Packets with a length of 1, or
         *       Data Packets with a variable length (up to \ref
         *       MAX_TRANSMIT_DATA_LENGTH) but with a leading 0x00 octet.
         *       The AT interface will discard invalid packets.
         *
         * \param data      Pointer to raw packet data.
         * \param length    Length of packet data.
         * \param timestamp Timestamp of the received Packet.
         */
        void packet_rx(uint8_t *data, unsigned length, uint32_t timestamp);

        void set_echoback(bool new_echo);

        void send_internal_status();

        /** Serial port handle. */
        RawSerial serial;

    private:
        /** Pointer to Blockchain interface */
        Blockchain* blockchain;

        /** Tracks whether echo is enabled. */
        bool echo;

        /** Receive buffer. */
        char in_buffer[AT_BUFFER_SIZE];

        /** Command parse buffer. */
        char parse_buffer[CMD_PARSE_BUFFERSIZE];

        /** Index to start of data. */
        uint16_t start_index;

        /** Index to end of the first command. */
        uint16_t cmd_end_index;

        /** Index to end of data. */
        uint16_t end_index;

        /** Tracks how many commands are waiting to be processed. */
        uint16_t cmd_ready;

        /** Tracks the application command ready callback. */
        at_interface_callback_t cmd_ready_callback;

        /** Handle AT+RESET command. */
        void reset(void);

        /** Handle AT+RADIOCFG= command. */
        void radio_cfg(void);

        /** Handle AT+RADIOCFG command. */
        void get_radiocfg(void);

        /** Handle AT+TIMERCFG= command. */
        void timer_cfg(void);

        /** Handle AT+TIMERCFG command. */
        void get_timercfg(void);


        /** Handle AT+TX and AT+TXDATA command. */
        void tx_data(bool data_only);

        /** Handle AT+VERSION command */
        void version(void);

        /** Handle AT+ECHOBACK= command */
        void echoback(void);

        /** Handle AT+ECHOBACK command */
        void get_echoback(void);

        /** Handle AT+PRVKEY command or print the new private key after AT+GENKEY */
        void get_private_key(const char* op_name);

        /** Handle AT+PRVKEY= command */
		void set_private_key(void);

        /** Handle AT+PUBKEY command */
        void get_public_key(void);

        /** Handle AT+ETHADDR command */
        void get_eth_address(void);

        /** Handle AT+RNG command */
        void get_rng(void);

        /** Handle AT+KECCAK256 command */
		void calculate_keccak256(void);

		/** Handle AT+ETHRECOVER and AT+ECRECOVER commands */
		void run_ecrecover(bool eth_recover);

		/** Handle AT+FLMINT family of commands */
		void run_foam_lite_mint(at_foam_lite_op_t foam_lite_op);

		/** Handle AT+FLXFER family of commands */
		void run_foam_lite_xfer(at_foam_lite_op_t foam_lite_op);

        /**
         * Perform a strncmp on the receive buffer.
         *
         * \param data_start_index  Index of the start of valid data in the buffer.
         * \param data_end_index    Index of the end of valid data in the buffer.
         * \param cmp_str           Pointer to the string to compare.
         * \param cmp_str_length    Length of the string to compare.
         *
         * \returns \c true if matched, else \c false.
         */
        bool wrapped_strncmp(uint16_t data_start_index,
                             uint16_t data_end_index,
                             const char *cmp_str, uint16_t cmp_str_length);

        /**
         * Increment an index into a wrapping buffer of size AT_BUFFER_SIZE.
         *
         * \param index         The index to increment.
         * \param increment     The amount to increment the index.
         */
        void increment_index(/*@out@*/ uint16_t *index, uint16_t increment);

        /** Copy received data to command parse buffer. */
        void cpy_to_parse_buffer(void);

        /**
         * Gets a parameter in a comma separated list.
         *
         * \param buffer    Buffer containing start of string, or NULL to get
         *                  the next parameter in the list on subsequent
         *                  invocations (analogous to strtok()).
         * \param length    Buffer to receive length of the parameter string.
         *
         * \returns a pointer to the parameter string.
         */
        char * get_parameter(/*@null@*/ char *buffer,
                             /*@out@*/ unsigned *length);

        /**
         * Check if a parameter string is an unsigned decimal number.
         *
         * \param buffer    Buffer containing the parameter string.
         * \param length    Length of the parameter string.
         *
         * \returns \c true if the string is an unsigned decimal number, else
         *          \c false.
         */
        bool parameter_is_unsigned(char *buffer, unsigned length);

        /**
         * Check if a parameter string is a hex number.
         *
         * \param buffer    Buffer containing the parameter string.
         * \param length    Length of the parameter string.
         *
         * \returns \c true if the string is a hex number, else \c false.
         */
        bool parameter_is_hex(char *buffer, unsigned length);
};


#endif /* ATINTERFACE_H__ */
