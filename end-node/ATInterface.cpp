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


#include "ltsp.h"
#include "ATInterface.h"
#include "PinNames.h"
#include "unique_id.h"
#include "FoamLite.h"


#include "stdlib.h"
#include "string.h"
#include "ctype.h"

#define USE_KECCCAK 1
#include "trezor-crypto/sha3.h"


/** "AT" string. */
static const char * const at_string = "AT";
/** "+RESET" string. */
static const char * const reset_string = "+RESET";
/** "+VERSION" string. */
static const char * const version_string = "+VERSION";
/** "+INTERNAL" string */
static const char * const internal_string   = "+INTERNAL";
/** "+RADIOCFG=" string. */
static const char * const radiocfg_string   = "+RADIOCFG=";
/** "+RADIOCFG" string. */
static const char * const get_radiocfg_string   = "+RADIOCFG";
/** "+TIMERCFG=" string. */
static const char * const timercfg_string   = "+TIMERCFG=";
/** "+TIMERCFG" string. */
static const char * const get_timercfg_string   = "+TIMERCFG";
/** "+TXDATA" string. */
static const char * const txdata_string   = "+TXDATA=";
/** "+TX" string. */
static const char * const txdata_simple_string   = "+TX=";
/** "+ECHOBACK=" string. */
static const char * const echoback_string   = "+ECHOBACK=";
/** "+ECHOBACK" string. */
static const char * const get_echoback_string   = "+ECHOBACK";

/** "+PRVKEY" string. */
static const char * const get_private_key_string       = "+PRVKEY";
/** "+PRVKEY=" string. */
static const char * const set_private_key_string       = "+PRVKEY=";
/** "+GENKEY" string. */
static const char * const generate_private_key_string  = "+GENKEY";
/** "+PUBKEY" string. */
static const char * const get_public_key_string        = "+PUBKEY";
/** "+ETHADDR" string. */
static const char * const get_eth_address_string       = "+ETHADDR";
/** "+ECRECOVER=" string. */
static const char * const run_ecrecover_string         = "+ECRECOVER=";
/** "+ETHRECOVER=" string. */
static const char * const run_ethrecover_string         = "+ETHRECOVER=";

/** "+RNG" string. */
static const char * const get_rng_string   = "+RNG";

/** "+KECCAK256=" string. */
static const char * const calculate_keccak256_string   = "+KECCAK256=";

/** "+FLMINTHASH=" string. */
static const char * const fl_mint_hash_string   = "+FLMINTHASH=";
/** "+FLXFERHASH=" string. */
static const char * const fl_xfer_hash_string   = "+FLXFERHASH=";
/** "+FLMINTMSG=" string. */
static const char * const fl_mint_msg_string   = "+FLMINTMSG=";
/** "+FLXFERMSG=" string. */
static const char * const fl_xfer_msg_string   = "+FLXFERMSG=";
/** "+FLMINT+TX=" string. */
static const char * const fl_mint_tx_string   = "+FLMINT+TX=";
/** "+FLXFER+TX=" string. */
static const char * const fl_xfer_tx_string   = "+FLXFER+TX=";

static const char * all_command_strings[] = {
		reset_string, version_string, internal_string,
		txdata_simple_string, txdata_string,
		radiocfg_string, get_radiocfg_string,
		timercfg_string, get_timercfg_string,
		echoback_string, get_echoback_string,
		get_private_key_string, set_private_key_string,
		generate_private_key_string,
		get_public_key_string, get_eth_address_string,
		get_rng_string,
		calculate_keccak256_string,
		run_ecrecover_string, run_ethrecover_string,
		fl_mint_hash_string, fl_mint_msg_string, fl_mint_tx_string,
		fl_xfer_hash_string, fl_xfer_msg_string, fl_xfer_tx_string,
};

inline void
byte_to_hex(uint8_t b, char* dst)
{
	snprintf(dst, 3, "%02X", b);
}

inline bool parse_hex(const char *hex, const size_t hex_length, uint8_t *dest, const size_t dest_length)
{
	if (hex == NULL || hex_length % 2 != 0 || hex_length != dest_length * 2)
	{
		return false;
	}

	for (size_t ii = 0; ii < hex_length; ii++)
	{
		if (!isxdigit(hex[ii])) {
			return false;
		}
		uint8_t asU8 = hex_to_nibble(hex[ii]);
		if ((ii % 2) == 0) {
			dest[ii / 2] = (asU8 << 4);
		} else {
			dest[ii / 2] |= asU8;
		}
	}
	return true;
}

inline bool parse_hex_variable_length(const char *hex, const size_t hex_length, uint8_t *dest, const size_t max_dest_length)
{
	if (hex == NULL || hex_length == 0 || hex_length % 2 != 0 || hex_length > max_dest_length * 2)
	{
		return false;
	}
	for (size_t ii = 0; ii < hex_length; ii++)
	{
		if (!isxdigit(hex[ii])) {
			return false;
		}
		uint8_t asU8 = hex_to_nibble(hex[ii]);
		if ((ii % 2) == 0) {
			dest[ii / 2] = (asU8 << 4);
		} else {
			dest[ii / 2] |= asU8;
		}
	}
	return true;
}

/**
 * printf()s to wherever a response is being sent, without modifying the input
 */
#define SEND_PARTIAL_RESPONSE(fmt, ...)         \
    serial.printf(fmt, ##__VA_ARGS__)

/**
 * Sends a response - bookends what is passed to it with '\r\n'.
 */
#define SEND_RESPONSE(fmt, ...)                 \
    serial.printf("\r\n"fmt"\r\n", ##__VA_ARGS__)


ATInterface::ATInterface(PinName tx, PinName rx, int baudrate,
                         at_interface_callback_t cmd_ready_callback,
                         at_interface_callback_t char_recv_callback,
                         bool echo, Blockchain* b) : serial(tx, rx, baudrate), blockchain(b)
{
    this->cmd_ready_callback = cmd_ready_callback;
    this->start_index = 0;
    this->cmd_end_index = 0;
    this->end_index = 0;
    this->cmd_ready = 0;
    this->echo = echo;
    memset(this->in_buffer, 0, AT_BUFFER_SIZE);
    memset(this->parse_buffer, 0, CMD_PARSE_BUFFERSIZE);

    this->serial.attach(char_recv_callback, RawSerial::RxIrq);
}


void
ATInterface::rx_handler(void)
{
    uint16_t old_end_index;

    if (in_buffer == NULL) {
        return;
    }

    while (serial.readable())
    {
        old_end_index = end_index;
        char the_char = serial.getc();
        if (the_char == '\n') {
            // we don't process \n, but lets echo them back if we care about them..
            if (echo) {
                serial.putc(the_char);
            }
            // and skip over it, for the sake of parsing, etc.
            continue;
        } else {
            // otherwise, put it in the buffer and continue the flow as usual...
            in_buffer[old_end_index] = the_char;
        }

        /* Convert to uppercase */
        if (in_buffer[old_end_index] > 96 && in_buffer[old_end_index] < 123)
            in_buffer[old_end_index] = in_buffer[old_end_index] - 32;

        if (echo)
            serial.putc(in_buffer[old_end_index]);
        end_index++;

        if (end_index >= AT_BUFFER_SIZE)
            end_index = 0;

        /* If we circle around and run into the start index overwrite the
         * oldest character. Same situation with the command start index.*/
        if (start_index == end_index)
            start_index++;

        if (start_index >= AT_BUFFER_SIZE)
            start_index = 0;

        /* When we receive a carriage return we need to process the command. */
        if (in_buffer[old_end_index] == '\r')
        {
            /*
             * If we don't have any commands processing then the end of the
             * first command must also be the buffer end index. Otherwise
             * processing the commands will update the command end index.
             */
            DEBUG_AT("Command ready\r\n");
            cmd_ready++;

            if (cmd_ready >= 1)
            {
                if (cmd_ready_callback != NULL)
                    cmd_ready_callback();
            }
        }
    }
}


bool
ATInterface::wrapped_strncmp(uint16_t data_start_index,
                             uint16_t data_end_index,
                             const char *cmp_str, uint16_t cmp_str_length)
{
    uint16_t buffer_contents_size;

    DEBUG_AT("(data_start_index, data_end_index, cmp_str_length): "
             "(%u, %u, %u)\r\n",
             data_start_index, data_end_index, cmp_str_length);

    if (data_start_index < data_end_index)
    {
        DEBUG_AT("Buffer doesn't wrap\r\n");
        buffer_contents_size = data_end_index - data_start_index;
    }
    else
    {
        DEBUG_AT("Buffer wraps\r\n");
        buffer_contents_size = AT_BUFFER_SIZE - data_start_index
                               + data_end_index;
    }

    DEBUG_AT("Buffer contents size: %u\r\n", buffer_contents_size);

    if (buffer_contents_size < cmp_str_length || cmp_str_length == 0)
    {
        DEBUG_AT("Incorrect compare size: %u\r\n", cmp_str_length);
        return false;
    }

    if (data_start_index + cmp_str_length <= AT_BUFFER_SIZE)
    {
        DEBUG_AT("Compare doesn't wrap\r\n");
        if (!strncmp(&in_buffer[data_start_index], cmp_str, cmp_str_length))
        {
            DEBUG_AT("Compare success\r\n");
            return true;
        }
        else
        {
            return false;
        }
    }
    else
    {
        DEBUG_AT("Compare wraps");
        if (!strncmp(&in_buffer[data_start_index], cmp_str,
                     AT_BUFFER_SIZE - data_start_index))
        {
            DEBUG_AT("Prewrap compare success\r\n");
            if (!strncmp(in_buffer,
                         &cmp_str[AT_BUFFER_SIZE - data_start_index],
                         AT_BUFFER_SIZE - data_start_index))
            {
                DEBUG_AT("Postwrap compare success\r\n");
                return true;
            }
        }
    }

    return false;
}


void
ATInterface::cpy_to_parse_buffer(void)
{
    if (start_index < end_index)
    {
        DEBUG_AT("Copying data from index %u to %u\r\n", start_index,
                 end_index);

        memcpy(parse_buffer, &in_buffer[start_index], end_index - start_index);

        /* Null terminate the command string to parse. */
        parse_buffer[end_index - start_index] = '\0';
    }
    else
    {
        DEBUG_AT("Copying data from index %u to %u\r\n", start_index,
                 AT_BUFFER_SIZE);

        memcpy(parse_buffer, &in_buffer[start_index],
               AT_BUFFER_SIZE - start_index);
        DEBUG_AT("Copying data from index 0 to %u\r\n", end_index);
        memcpy(&parse_buffer[AT_BUFFER_SIZE - start_index], in_buffer,
               end_index);

        /* Null terminate the command string to parse. */
        parse_buffer[end_index + (AT_BUFFER_SIZE - start_index)] = '\0';
    }
}


void
ATInterface::increment_index(/*@out@*/ uint16_t *index, uint16_t increment)
{
    uint32_t new_index = *index + increment;

    if (new_index >= AT_BUFFER_SIZE)
        *index = new_index - AT_BUFFER_SIZE;
    else
        *index = new_index;
}


/* Don't call from an interrupt context. */
void
ATInterface::process_cmd_buffer(void)
{
#define MATCHED_COMMAND(__CMD_STRING) \
	(\
			wrapped_strncmp(start_index, \
							cmd_end_index, \
							__CMD_STRING, \
							strlen(__CMD_STRING) \
			) \
	)
#define PREPARE_PARSE_COMMAND(__CMD_STRING) \
    { \
	  DEBUG_AT("Matched command AT+%s\r\n", __CMD_STRING); \
	  increment_index(&start_index, strlen(__CMD_STRING)); \
	  cpy_to_parse_buffer(); \
    }
    uint16_t ii;
    while (cmd_ready > 0)
    {
        DEBUG_AT("Processing buffer start, (start, end): (%u, %u)\r\n",
                 start_index, end_index);

        /* Find the end of the first command in the buffer */
        for (ii = start_index; ii < AT_BUFFER_SIZE; ii++)
        {
            if (ii == end_index)
            {
                DEBUG_AT("No commands in buffer\r\n");
                goto cleanup;
            }

            if (in_buffer[ii] == '\r')
            {
                DEBUG_AT("\\r found at index: %u\r\n", ii);
                cmd_end_index = ii;
                goto start_processing;
            }
        }
        for (ii = 0; ii < end_index; ii++)
        {
            if (in_buffer[ii] == '\r')
            {
                DEBUG_AT("\\r found at index: %u\r\n", ii);
                cmd_end_index = ii;
                goto start_processing;
            }
        }

start_processing:
        /* For the wrapped string compare we want the end index to be after the
         * last character rather than being the last character. */
        increment_index(&cmd_end_index, 1);

        for (ii = 0; ii < AT_BUFFER_SIZE; ii++)
        {
            DEBUG_AT("%02x", in_buffer[ii]);
        }
        DEBUG_AT("\r\n");

        DEBUG_AT("Processing buffer post AT, (start, end): (%u, %u)\r\n",
                 start_index, end_index);

        DEBUG_AT("at_string strlen: %u\r\n", strlen(at_string));

        /* All commands start with AT so if it doesn't start with AT remove
         * the 'garbage' from the buffer. */
        if (!wrapped_strncmp(start_index, cmd_end_index,
                             at_string, strlen(at_string)))
        {
            DEBUG_AT("command didn't start with AT\r\n");
            start_index = cmd_end_index;
            cmd_ready--;
            return;
        }

        DEBUG_AT("Matched AT\r\n");
        increment_index(&start_index, 2);

        DEBUG_AT("First post AT char: 0x%02x\r\n", in_buffer[start_index]);

        if (wrapped_strncmp(start_index, cmd_end_index, "\r", strlen("\r")))
        {
            DEBUG_AT("Matched command AT\r\n");
            SEND_RESPONSE("OK");
        }

        else if (MATCHED_COMMAND(reset_string))
        {
            DEBUG_AT("Matched command AT+RESET\r\n");
            reset();
        }

        else if (MATCHED_COMMAND(version_string))
        {
            DEBUG_AT("Matched command AT+VERSION\r\n");
            version();
        }

        else if (MATCHED_COMMAND(internal_string))
        {
            DEBUG_AT("Matched command AT+INTERNAL\r\n");
            send_internal_status();
        }

        else if (MATCHED_COMMAND(echoback_string))
        {
            PREPARE_PARSE_COMMAND(echoback_string);
            echoback();
        }

        else if (MATCHED_COMMAND(get_echoback_string))
        {
            DEBUG_AT("Matched command AT+ECHOBACK\r\n");
            get_echoback();
        }

        else if (MATCHED_COMMAND(radiocfg_string))
        {
            PREPARE_PARSE_COMMAND(radiocfg_string);
            radio_cfg();
        }

        else if (MATCHED_COMMAND(get_radiocfg_string))
        {
            DEBUG_AT("Matched command AT+RADIOCFG\r\n");
            get_radiocfg();
        }

        else if (MATCHED_COMMAND(timercfg_string))
        {
        	PREPARE_PARSE_COMMAND(timercfg_string);
            timer_cfg();
        }

        else if (MATCHED_COMMAND(get_timercfg_string))
        {
            DEBUG_AT("Matched command AT+TIMERCFG\r\n");
            get_timercfg();
        }

        else if (MATCHED_COMMAND(txdata_simple_string))
		{
			PREPARE_PARSE_COMMAND(txdata_simple_string);
			tx_data(true);
		}

        else if (MATCHED_COMMAND(txdata_string))
        {
            PREPARE_PARSE_COMMAND(txdata_string);
            tx_data(false);
        }

        else if (MATCHED_COMMAND(generate_private_key_string))
		{
			DEBUG_AT("Matched command AT+GENKEY\r\n");
			if (this->blockchain != NULL)
			{
				this->blockchain->generate_private_key();
			}
			get_private_key("GENKEY");
		}

        else if (MATCHED_COMMAND(set_private_key_string))
		{
			PREPARE_PARSE_COMMAND(set_private_key_string);
			set_private_key();
		}


        else if (MATCHED_COMMAND(get_private_key_string))
        {
            DEBUG_AT("Matched command AT+PRVKEY\r\n");
            get_private_key("PRVKEY");
        }


        else if (MATCHED_COMMAND(get_public_key_string))
        {
            DEBUG_AT("Matched command AT+PUBKEY\r\n");
            get_public_key();
        }


        else if (MATCHED_COMMAND(get_eth_address_string))
        {
            DEBUG_AT("Matched command AT+ETHADDR\r\n");
            get_eth_address();
        }

        else if (MATCHED_COMMAND(get_rng_string))
        {
            DEBUG_AT("Matched command AT+RNG\r\n");
            get_rng();
        }

        else if (MATCHED_COMMAND(calculate_keccak256_string))
		{
        	PREPARE_PARSE_COMMAND(calculate_keccak256_string);
			calculate_keccak256();
		}

        else if (MATCHED_COMMAND(run_ecrecover_string))
		{
        	PREPARE_PARSE_COMMAND(run_ecrecover_string);
			run_ecrecover(false);
		}

        else if (MATCHED_COMMAND(run_ethrecover_string))
		{
        	PREPARE_PARSE_COMMAND(run_ethrecover_string);
			run_ecrecover(true);
		}

        else if (MATCHED_COMMAND(fl_mint_hash_string))
		{
			PREPARE_PARSE_COMMAND(fl_mint_hash_string);
			run_foam_lite_mint(FOAM_LITE_HASH_ONLY);
		}

        else if (MATCHED_COMMAND(fl_mint_msg_string))
		{
			PREPARE_PARSE_COMMAND(fl_mint_msg_string);
			run_foam_lite_mint(FOAM_LITE_SIGN_ONLY);
		}

        else if (MATCHED_COMMAND(fl_mint_tx_string))
		{
			PREPARE_PARSE_COMMAND(fl_mint_tx_string);
			run_foam_lite_mint(FOAM_LITE_SIGN_AND_TX);
		}

        else if (MATCHED_COMMAND(fl_xfer_hash_string))
		{
			PREPARE_PARSE_COMMAND(fl_xfer_hash_string);
			run_foam_lite_xfer(FOAM_LITE_HASH_ONLY);
		}

        else if (MATCHED_COMMAND(fl_xfer_msg_string))
		{
			PREPARE_PARSE_COMMAND(fl_xfer_msg_string);
			run_foam_lite_xfer(FOAM_LITE_SIGN_ONLY);
		}

        else if (MATCHED_COMMAND(fl_xfer_tx_string))
		{
			PREPARE_PARSE_COMMAND(fl_xfer_tx_string);
			run_foam_lite_xfer(FOAM_LITE_SIGN_AND_TX);
		}

        else if (MATCHED_COMMAND("+X="))
		{
			PREPARE_PARSE_COMMAND("+X=");
			char* parameter;
			size_t len;
			Bignum128 bn;
			parameter = get_parameter(parse_buffer, &len);
			if (parameter == NULL || len == 0 || !Bignum128::parse_hex_var_len(parameter, len, &bn))
			{
				SEND_RESPONSE("X=ERROR,%u", AT_ERROR_CODE_INVALID_PARAMETERS);
			} else {
				uint8_t buf[16] = {0};
				char hexBuf[33] = "";
				bn.write_be(&buf[0]);
				for(int i = 0; i < 16; i++) {
					byte_to_hex(buf[i], &hexBuf[i*2]);
				}
				SEND_RESPONSE("X=%s,OK", hexBuf);
			}
		}


        else
        {
            DEBUG_AT("No matching AT command\r\n");
        }

        start_index = cmd_end_index;
        cmd_ready--;
    }

    return;

cleanup:
    DEBUG_AT("cleanup\r\n");

#undef MATCHED_COMMAND
#undef PREPARE_PARSE_COMMAND
}


void
ATInterface::reset(void)
{
    NVIC_SystemReset();
}

void
ATInterface::version(void)
{
    device_uid_t uid = get_device_uid();
    SEND_PARTIAL_RESPONSE("\r\nVERSION=v%u.%u.%u,0x%08x%08x%08x,",
        VERSION_MAJOR, VERSION_MINOR, VERSION_PATCHLEVEL,
        uid.uid0, uid.uid1, uid.uid2);
    for (size_t i = 0; i < sizeof(all_command_strings) / sizeof(char*); i++) {
    	SEND_PARTIAL_RESPONSE("%s,", all_command_strings[i]);
    }
    SEND_PARTIAL_RESPONSE("OK\r\n");
}

void
ATInterface::echoback(void)
{
    char *parameter;
    unsigned length;
    uint32_t tmp;

#ifdef DEBUG_ENABLE
    uint16_t ii;

    DEBUG_AT("Parsing echoback data: ");
    for (ii = 0; ii < CMD_PARSE_BUFFERSIZE; ii++)
        DEBUG_AT("%02x", parse_buffer[ii]);

    DEBUG_AT("\r\n");
#endif

    parameter = get_parameter(parse_buffer, &length);
    if (parameter == NULL || length == 0
            || !parameter_is_unsigned(parameter, length))
    {
        goto echoback_invalid_params;
    }

    tmp = strtoul(parameter, NULL, 10);
    if (tmp == 0) {
        set_echoback(false);
    } else if (tmp == 1) {
        set_echoback(true);
    } else {
        goto echoback_invalid_params;
    }

    DEBUG_AT("AT+ECHOBACK successful\r\n");
    SEND_RESPONSE("ECHOBACK=OK");
    return;

echoback_invalid_params:
    DEBUG_AT("Invalid AT+ECHOBACK parameters\r\n");
    SEND_RESPONSE("ECHOBACK=ERROR,%u", AT_ERROR_CODE_INVALID_PARAMETERS);
}

void ATInterface::get_echoback(void)
{
    if (this->echo) {
        SEND_RESPONSE("ECHOBACK=1,OK");
    } else {
        SEND_RESPONSE("ECHOBACK=0,OK");
    }
}

void
ATInterface::send_reset_msg(uint8_t major, uint8_t minor, uint8_t patch, at_error_code_t error)
{
    if (error == AT_ERROR_CODE_NO_ERROR)
        SEND_RESPONSE("RESET=v%u.%u.%u,OK", major, minor, patch);
    else
        SEND_RESPONSE("RESET=v%u.%u.%u,ERROR,%u", major, minor, patch, error);
}


char *
ATInterface::get_parameter(/*@null@*/ char *buffer,
                           /*@out@*/ unsigned *length)
{
    char *tok = strtok(buffer, ","); // NOLINT

    /*
     * NOTE: This function is not multi-thread safe, largely because of the use
     * of strtok instead of strtok_r. The NOLINT annotation is to get around
     * splint warnings.
     *
     * This has been done for expediency and may want to be revisited in future.
     */

    if (tok != NULL)
        *length = strlen(tok);
    else
        *length = 0;

    /*
     * If the parameter is delimited by a \r\n, or \r then we exclude this from
     * the length.
     */
    if (*length > 0 && (tok[*length - 1] == '\n'))
        (*length)--;

    if (*length > 0 && (tok[*length - 1] == '\r'))
        (*length)--;

    return tok;
}


bool
ATInterface::parameter_is_unsigned(char *buffer, unsigned length)
{
    unsigned ii;

    for (ii = 0; ii < length; ii++)
    {
        if (!isdigit(buffer[ii]))
            return false;
    }

    return true;
}


bool
ATInterface::parameter_is_hex(char *buffer, unsigned length)
{
    unsigned ii;

    for (ii = 0; ii < length; ii++)
    {
        if (!isxdigit(buffer[ii]))
            return false;
    }

    return true;
}


void
ATInterface::radio_cfg(void)
{
    char *parameter;
    unsigned length;
    uint32_t tmp;

    /* Radio parameters */
    radio_params_t radio_params = RADIO_PARAMS_DEFAULTS;
#ifdef DEBUG_ENABLE
    uint16_t ii;

    DEBUG_AT("Parsing radio config data: ");
    for (ii = 0; ii < CMD_PARSE_BUFFERSIZE; ii++)
        DEBUG_AT("%02x", parse_buffer[ii]);

    DEBUG_AT("\r\n");
#endif

    /* RF frequency. */
    parameter = get_parameter(parse_buffer, &length);
    if (parameter == NULL || length == 0 ||
            !parameter_is_unsigned(parameter, length))
    {
        goto radiocfg_invalid_params;
    }

    radio_params.frequency = strtoul(parameter, NULL, 10);

    /* TX power. */
    parameter = get_parameter(NULL, &length);
    if (parameter == NULL || length == 0 ||
            !parameter_is_unsigned(parameter, length))
    {
        goto radiocfg_invalid_params;
    }

    tmp = strtoul(parameter, NULL, 10);
    if (tmp > MAX_TX_OUTPUT_POWER)
        goto radiocfg_invalid_params;
    radio_params.tx_power = static_cast<int8_t>(tmp);

    /* LoRa bandwidth. */
    parameter = get_parameter(NULL, &length);
    if (parameter == NULL || length == 0 ||
            !parameter_is_unsigned(parameter, length))
    {
        goto radiocfg_invalid_params;
    }

    tmp = strtoul(parameter, NULL, 10);
    if (tmp > static_cast<uint32_t>(LORA_BANDWIDTH_500_KHZ))
        goto radiocfg_invalid_params;

    radio_params.bandwidth =
            static_cast<lora_bandwidth_t>(tmp);

    /* LoRa datarate. */
    parameter = get_parameter(NULL, &length);
    if (parameter == NULL || length == 0 ||
            !parameter_is_unsigned(parameter, length))
    {
        goto radiocfg_invalid_params;
    }

    tmp = strtoul(parameter, NULL, 10);
    if (tmp > static_cast<uint32_t>(LORA_DATARATE_4096 - LORA_DATARATE_64))
    {
        goto radiocfg_invalid_params;
    }

    radio_params.datarate =
            static_cast<lora_datarate_t>(tmp + LORA_DATARATE_64);

    /* LoRa coding rate. */
    parameter = get_parameter(NULL, &length);
    if (parameter == NULL || length == 0 ||
            !parameter_is_unsigned(parameter, length))
    {
        goto radiocfg_invalid_params;
    }

    tmp = strtoul(parameter, NULL, 10);
    if (tmp >
            static_cast<uint32_t>(LORA_CODING_RATE_4_8 - LORA_CODING_RATE_4_5))
    {
        goto radiocfg_invalid_params;
    }

    radio_params.coding_rate =
            static_cast<lora_coding_rate_t>(tmp + LORA_CODING_RATE_4_5);

    /* Preamble length. */
    parameter = get_parameter(NULL, &length);
    if (parameter == NULL || length == 0 ||
            !parameter_is_unsigned(parameter, length))
    {
        goto radiocfg_invalid_params;
    }

    radio_params.preamble_length =
            static_cast<uint16_t>(strtoul(parameter, NULL, 10));

    /* CRC enabled. */
    parameter = get_parameter(NULL, &length);
    if (parameter == NULL || length == 0 ||
            !parameter_is_unsigned(parameter, length))
    {
        goto radiocfg_invalid_params;
    }

    radio_params.crc_enabled =
            static_cast<uint8_t>(strtoul(parameter, NULL, 10)) ? true : false;

    /* Here we need to configure the radio */
    if (!configure_radio(&radio_params))
    {
        DEBUG_AT("Failed to configure radio\r\n");
        SEND_RESPONSE("RADIOCFG=ERROR,%u", AT_ERROR_CODE_HARDWARE_FAILURE);
        return;
    }

    DEBUG_AT("AT+RADIOCFG successful\r\n");
    SEND_RESPONSE("RADIOCFG=OK");
    return;

radiocfg_invalid_params:
    DEBUG_AT("Invalid AT+RADIOCFG parameters\r\n");
    SEND_RESPONSE("RADIOCFG=ERROR,%u", AT_ERROR_CODE_INVALID_PARAMETERS);
}

void
ATInterface::get_radiocfg(void) {
    radio_params_t p = get_radio_configuration();
    uint32_t corrected_datarate = static_cast<uint32_t>(p.datarate) - LORA_DATARATE_64;
    uint32_t corrected_coderate = static_cast<uint32_t>(p.coding_rate) - LORA_CODING_RATE_4_5;

    SEND_RESPONSE("RADIOCFG=%u,%u,%u,%u,%u,%u,%u,OK", p.frequency, p.tx_power,
        p.bandwidth, corrected_datarate, corrected_coderate, p.preamble_length,
        p.crc_enabled);
}


void
ATInterface::timer_cfg(void)
{
    char *parameter;
    unsigned length;
    timer_params_t timer_params = TIMER_PARAMS_DEFAULTS;


#ifdef DEBUG_ENABLE
    unsigned ii;
    DEBUG_AT("Parsing TIMERCFG data: ");
    for (ii = 0; ii < CMD_PARSE_BUFFERSIZE; ii++)
        DEBUG_AT("%02x", parse_buffer[ii]);

    DEBUG_AT("\r\n");
#endif

    /* Prescaler. */
    parameter = get_parameter(parse_buffer, &length);
    if (parameter == NULL || length == 0 ||
            !parameter_is_unsigned(parameter, length))
    {
        goto timercfg_invalid_params;
    }

    timer_params.prescaler = strtoul(parameter, NULL, 10);

    /* Period. */
    parameter = get_parameter(NULL, &length);
    if (parameter == NULL || length == 0 ||
            !parameter_is_unsigned(parameter, length))
    {
        goto timercfg_invalid_params;
    }

    timer_params.period = strtoul(parameter, NULL, 10);

    DEBUG_AT("AT+TIMERCFG prescaler %u period %u\r\n",
             timer_params.prescaler, timer_params.period);

    configure_timer(&timer_params);

    DEBUG_AT("AT+TIMERCFG successful\r\n");
    SEND_RESPONSE("TIMERCFG=OK");

    return;

timercfg_invalid_params:
    DEBUG_AT("Invalid AT+TIMERCFG parameters\r\n");
    SEND_RESPONSE("TIMERCFG=ERROR,%u", AT_ERROR_CODE_INVALID_PARAMETERS);
}

void
ATInterface::get_timercfg(void) {
    timer_params_t p = get_timer_configuration();
    SEND_RESPONSE("TIMERCFG=%u,%u,OK", p.prescaler, p.period);
}

void
ATInterface::packet_rx(uint8_t *data, unsigned length, uint32_t timestamp)
{
    unsigned ii;
    char buffer[MAX_RXDATA_LENGTH] = { 0 };

    if (length > MAX_TRANSMIT_DATA_LENGTH)
    {
        DEBUG_AT("Message too long, dropping\r\n");
        return;
    }

    if ((length == 1) && (data[0] == 0x00))
    {
        DEBUG_AT("Invalid Timing Packet\r\n");
        return;
    }

    /* timing packets can be up to 7 bytes long */
    if ((length > 7) && (data[0] != 0x00))
    {
        DEBUG_AT("Invalid Data Packet\r\n");
        return;
    }

    /* Convert hex to string */
    for (ii = 0; ii < length; ii++)
    {
        snprintf(&buffer[ii * 2], sizeof(&buffer[ii * 2]),
                 "%x", (data[ii] >> 4) & 0x0f);
        snprintf(&buffer[ii * 2 + 1], sizeof(&buffer[ii * 2 + 1]),
                 "%x", data[ii] & 0x0f);
    }
    SEND_RESPONSE("RXDATA=%u,%s,OK", timestamp, buffer);
}


void
ATInterface::packet_tx_complete(at_error_code_t error, uint32_t *timestamps,
                                uint16_t length, bool was_attx)
{
    uint16_t i;

    if (error != AT_ERROR_CODE_NO_ERROR || length == 0)
    {
        SEND_RESPONSE("%s=ERROR,%u", was_attx ? "TX" : "TXDATA", error);
        return;
    }

    serial.printf("\r\n%s=", was_attx ? "TX" : "TXDATA");
    for (i = 0; i < length; i++)
    {
        serial.printf("%u,", timestamps[i]);
    }
    serial.printf("OK\r\n");
}


void
ATInterface::tx_data(bool is_attx)
{
    char *parameter;
    uint8_t timing_packets;
    uint32_t interval;
    unsigned int length;

    /* Buffer for the encoded data */
    uint8_t buffer[MAX_AT_TXDATA_LENGTH / 2];

    if (is_attx) {
    	timing_packets = 0;
    	interval = 0;
    	goto txdata_parse_data;
    }

#ifdef DEBUG_ENABLE
    // DEBUG_AT("Parsing TXDATA data: ");
    // for (ii = 0; ii < CMD_PARSE_BUFFERSIZE; ii++)
        // DEBUG_AT("%02x", parse_buffer[ii]);

    // DEBUG_AT("\r\n");
#endif

    /* Timing packets. */
    parameter = get_parameter(parse_buffer, &length);
    if (parameter == NULL || length == 0 ||
            !parameter_is_unsigned(parameter, length))
    {
        goto txdata_invalid_params;
    }

    timing_packets = static_cast<int8_t>(strtol(parameter, NULL, 10));

    /* Interval. */
    parameter = get_parameter(NULL, &length);
    if (parameter == NULL || length == 0 ||
        !parameter_is_unsigned(parameter, length))
    {
        goto txdata_invalid_params;
    }

    interval = strtoul(parameter, NULL, 10);

txdata_parse_data:

    /* Data. */
    parameter = get_parameter(is_attx ? parse_buffer : NULL, &length);
    if (!parse_hex_variable_length(parameter, length, buffer, MAX_AT_TXDATA_LENGTH))
    {
        goto txdata_invalid_params;
    }

    DEBUG_AT("AT+TXDATA (<timing packets>=%u, <interval>=%u, length=%u)\r\n",
             timing_packets, interval, length);
    DEBUG_AT("Data to send: %s\r\n", parameter);


    if (!send_packets(timing_packets, interval, buffer, length / 2, is_attx))
    {
        DEBUG_AT("Can't initiate sending at this time\r\n");
        SEND_RESPONSE("%s=ERROR,%u", is_attx ? "TX" : "TXDATA", AT_ERROR_CODE_TRANSMIT_ERROR);
    }

    return;

txdata_invalid_params:
    DEBUG_AT("Invalid AT+TXDATA parameters\r\n");
    SEND_RESPONSE("%s=ERROR,%u", is_attx ? "TX" : "TXDATA", AT_ERROR_CODE_INVALID_PARAMETERS);
}

void
ATInterface::set_echoback(bool new_echo) {
    this->echo = new_echo;
}

void
ATInterface::send_internal_status() {
    SEND_RESPONSE("FREQ={SYSCLK=%u,HCLK=%u,PCLK1=%u,PCLK2=%u}", HAL_RCC_GetSysClockFreq(), HAL_RCC_GetHCLKFreq(), HAL_RCC_GetPCLK1Freq(), HAL_RCC_GetPCLK2Freq());
}

void ATInterface::get_private_key(const char* cmd_name)
{
	if (this->blockchain == NULL) {
		SEND_RESPONSE("%s=unimplemented,ERROR,1", cmd_name);
		return;
	}

	char buf[PRIVATE_KEY_SIZE*2 + 1] = { 0 };
	uint8_t *prv = this->blockchain->get_private_key();

	for (int i = 0; i < PRIVATE_KEY_SIZE; i++) {
		byte_to_hex(prv[i], &buf[i*2]);
	}

	SEND_RESPONSE("%s=%s,OK", cmd_name, buf);
}

void ATInterface::set_private_key(void)
{
	if (this->blockchain == NULL) {
		SEND_RESPONSE("PRVKEY=ERROR,%u", AT_ERROR_CODE_UNSUPPORTED);
		return;
	}
	char *parameter;
	size_t length;
	/* Buffer for the encoded data */
	uint8_t prv[PRIVATE_KEY_SIZE];

	parameter = get_parameter(parse_buffer, &length);
	if (!parse_hex(parameter, length, prv, PRIVATE_KEY_SIZE))
	{
		SEND_RESPONSE("PRVKEY=ERROR,%u", AT_ERROR_CODE_INVALID_PARAMETERS);
		return;
	}

	blockchain_error_code_t res = this->blockchain->set_private_key(prv, PRIVATE_KEY_SIZE);
	if (res == BLOCKCHAIN_ERROR_CODE_NO_ERROR) {
		SEND_RESPONSE("PRVKEY=OK");
	} else {
		SEND_RESPONSE("PRVKEY=ERROR,%u", AT_ERROR_CODE_INVALID_PARAMETERS);
	}

}

void ATInterface::get_public_key(void)
{
	if (this->blockchain == NULL) {
		SEND_RESPONSE("PRVKEY=ERROR,%u", AT_ERROR_CODE_UNSUPPORTED);
		return;
	}

	char buf[PUBLIC_KEY_UNCOMPRESSED_SIZE*2 + 1] = { 0 };
	uint8_t *prv = this->blockchain->get_uncompressed_public_key();

	for (int i = 0; i < PUBLIC_KEY_UNCOMPRESSED_SIZE; i++) {
		byte_to_hex(prv[i], &buf[i*2]);
	}

	SEND_RESPONSE("PUBKEY=%s,OK", buf);
}

void ATInterface::get_eth_address(void)
{
	if (this->blockchain == NULL) {
		SEND_RESPONSE("ETHADDR=ERROR,%u", AT_ERROR_CODE_UNSUPPORTED);
		return;
	}

	char buf[ETH_ADDRESS_SIZE*2 + 1] = { 0 };
	eth_address_t addr = this->blockchain->get_eth_address();

	for (int i = 0; i < ETH_ADDRESS_SIZE; i++) {
		byte_to_hex(addr.addr[i], &buf[i*2]);
	}

	SEND_RESPONSE("ETHADDR=%s,OK", buf);
}

void ATInterface::get_rng(void)
{
    SEND_RESPONSE("RNG=%u,OK",rng.get());
}

void ATInterface::calculate_keccak256(void)
{
	char *parameter = NULL;
	size_t length = 0;
	uint8_t data_buf[MAX_AT_KECCAK_LENGTH] = { 0 };
	uint8_t digest_buf[SHA3_256_DIGEST_LENGTH] = { 0 };
	char digest_hex_buf[SHA3_256_DIGEST_LENGTH * 2] = { 0 };

	parameter = get_parameter(parse_buffer, &length);
	if (!parse_hex_variable_length(parameter, length, data_buf, MAX_AT_KECCAK_LENGTH))
	{
		SEND_RESPONSE("KECCAK256=ERROR,%u", AT_ERROR_CODE_INVALID_PARAMETERS);
		return;
	}
	keccak_256(data_buf, length / 2, digest_buf);

	for (int i = 0; i < SHA3_256_DIGEST_LENGTH; i++) {
		byte_to_hex(digest_buf[i], &digest_hex_buf[i*2]);
	}
    SEND_RESPONSE("KECCAK256=%s,OK", digest_hex_buf);
}

void ATInterface::run_ecrecover(bool eth_recover)
{
	if (this->blockchain == NULL) {
		SEND_RESPONSE("%s=ERROR,%u", eth_recover ? "ETHRECOVER" : "ECRECOVER", AT_ERROR_CODE_UNSUPPORTED);
		return;
	}

	char *parameter = NULL;
	size_t length;
	uint8_t digest_buf[SHA3_256_DIGEST_LENGTH] = { 0 };
	uint8_t signature_buf[SIGNATURE_SIZE] = { 0 };
	char hex_buf[129] = { 0 }; // we'll either have 40 hex chars if its an eth addr, or 128 bytes if its a pubkey, +1 NULL terminator.

	parameter = get_parameter(parse_buffer, &length);
	if (!parse_hex(parameter, length, digest_buf, SHA3_256_DIGEST_LENGTH)) {
		goto ecrecover_invalid_params;
	}

	parameter = get_parameter(NULL, &length);
	if (!parse_hex(parameter, length, signature_buf, SIGNATURE_SIZE)) {
		goto ecrecover_invalid_params;
	}

	if (eth_recover) {
		eth_address_t eth_addr_buf;
		blockchain_error_code_t err = this->blockchain->eth_addr_recover(digest_buf, signature_buf, &eth_addr_buf);
		if (err != BLOCKCHAIN_ERROR_CODE_NO_ERROR) {
			SEND_RESPONSE("%s=ERROR,%u", eth_recover ? "ETHRECOVER" : "ECRECOVER",  AT_ERROR_CODE_BLOCKCHAIN_ERROR_BASE + err);
			return;
		}
		for (int i = 0; i < ETH_ADDRESS_SIZE; i++) {
			byte_to_hex(eth_addr_buf.addr[i], &hex_buf[i*2]);
		}
	} else {
		uint8_t serialized_pubkey_buf[PUBLIC_KEY_UNCOMPRESSED_SIZE + 1];
		blockchain_error_code_t err = this->blockchain->ec_recover(digest_buf, signature_buf, serialized_pubkey_buf);
		if (err != BLOCKCHAIN_ERROR_CODE_NO_ERROR) {
			SEND_RESPONSE("%s=ERROR,%u", eth_recover ? "ETHRECOVER" : "ECRECOVER",  AT_ERROR_CODE_BLOCKCHAIN_ERROR_BASE + err);
			return;
		}
		for (int i = 0; i < PUBLIC_KEY_UNCOMPRESSED_SIZE; i++) {
			byte_to_hex(serialized_pubkey_buf[i+1], &hex_buf[i*2]); // +1 to offset the 0x04 UNCOMPRESSED prefix
		}
	}

	SEND_RESPONSE("%s=%s,OK", eth_recover ? "ETHRECOVER" : "ECRECOVER",  hex_buf);
	return;

ecrecover_invalid_params:
	DEBUG_AT("Invalid AT+%s parameters\r\n", eth_recover ? "ETHRECOVER" : "ECRECOVER");
	SEND_RESPONSE("%s=ERROR,%u", eth_recover ? "ETHRECOVER" : "ECRECOVER",  AT_ERROR_CODE_INVALID_PARAMETERS);
}

void ATInterface::run_foam_lite_mint(at_foam_lite_op_t op)
{
	// if we're doing a sign+mint, we will have two events, the FLMINTMSG and a TXDATA
	const char* const respEvent = (op == FOAM_LITE_HASH_ONLY) ? &fl_mint_hash_string[1] : &fl_mint_msg_string[1];
	if (op != FOAM_LITE_HASH_ONLY && this->blockchain == NULL) {
		SEND_RESPONSE("%sERROR,%u", respEvent, AT_ERROR_CODE_UNSUPPORTED);
		return;
	}

	char *parameter;
	unsigned length;

	FoamLiteMint mint;

	// parse nonce
	parameter = get_parameter(parse_buffer, &length);
	if (parameter == NULL || length == 0 ||
		!parameter_is_unsigned(parameter, length))
	{
		goto run_foam_lite_mint_invalid_params;
	}
	mint.setNonce(strtoul(parameter, NULL, 10));

	// parse fee amount (in place)
	parameter = get_parameter(NULL, &length);
	if (parameter == NULL || length == 0 ||
		!Bignum128::parse_hex_var_len(parameter, length, mint.ptrFeeAmount()))
	{
		goto run_foam_lite_mint_invalid_params;
	}

	uint8_t mintData[MAX_MINT_DATA_SIZE];
	parameter = get_parameter(NULL, &length);
	if (parameter == NULL || length == 0 ||
		!parse_hex_variable_length(parameter, length, mintData, MAX_MINT_DATA_SIZE))
	{
		goto run_foam_lite_mint_invalid_params;
	}
	mint.setTokenData(mintData, length / 2);


	if (op == FOAM_LITE_HASH_ONLY) {
		keccak256_t signingHash = mint.signingHash();
		char hexBuf[SHA3_256_DIGEST_LENGTH * 2 + 1];
		for (int i = 0; i < SHA3_256_DIGEST_LENGTH; i++) {
			byte_to_hex(signingHash.hash[i], &hexBuf[i*2]);
		}
		SEND_RESPONSE("%s%s,OK", respEvent, hexBuf);
		return;
	} else {
		signed_message_t signedMsg = { .raw = { 0 } };
		blockchain_error_code_t signErr = mint.sign(blockchain, &signedMsg);
		if (signErr != BLOCKCHAIN_ERROR_CODE_NO_ERROR) {
			SEND_RESPONSE("%sERROR,%u", respEvent, AT_ERROR_CODE_BLOCKCHAIN_ERROR_BASE + signErr);
			return;
		}

		char msgHexBuf[MAX_RXDATA_LENGTH] = "";
		for (size_t i = 0; i < signedMsg.totalLength; i++) {
			byte_to_hex(signedMsg.raw[i], &msgHexBuf[i*2]);
		}
		SEND_RESPONSE("%s%s,OK", respEvent, msgHexBuf);

		if (op == FOAM_LITE_SIGN_AND_TX) {
			send_packets(0, 0, signedMsg.raw, signedMsg.totalLength, false);
		}

	}

	return;

run_foam_lite_mint_invalid_params:
	SEND_RESPONSE("%sERROR,%u", respEvent, AT_ERROR_CODE_INVALID_PARAMETERS);
	return;
}

void ATInterface::run_foam_lite_xfer(at_foam_lite_op_t op)
{
	// if we're doing a sign+transfer, we will have two events, the FLXFERMSG and a TXDATA
	const char* const respEvent = (op == FOAM_LITE_HASH_ONLY) ? &fl_xfer_hash_string[1] : &fl_xfer_msg_string[1];
	if (op != FOAM_LITE_HASH_ONLY && this->blockchain == NULL) {
		SEND_RESPONSE("%sERROR,%u", respEvent, AT_ERROR_CODE_UNSUPPORTED);
		return;
	}

	char *parameter;
	unsigned length;

	FoamLiteTransfer xfer;
	eth_address_t destAddr = { .addr = { 0 } };


	// parse nonce
	parameter = get_parameter(parse_buffer, &length);
	if (parameter == NULL || length == 0 ||
		!parameter_is_unsigned(parameter, length))
	{
		goto run_foam_lite_xfer_invalid_params;
	}
	xfer.setNonce(strtoul(parameter, NULL, 10));

	// parse fee amount (in place)
	parameter = get_parameter(NULL, &length);
	if (parameter == NULL || length == 0 ||
		!Bignum128::parse_hex_var_len(parameter, length, xfer.ptrFeeAmount()))
	{
		goto run_foam_lite_xfer_invalid_params;
	}

	// parse token ID to transfer
	parameter = get_parameter(NULL, &length);
	if (parameter == NULL || length == 0 ||
		!parameter_is_unsigned(parameter, length))
	{
		goto run_foam_lite_xfer_invalid_params;
	}
	xfer.setTokenID(strtoul(parameter, NULL, 10));

	parameter = get_parameter(NULL, &length);
	if (parameter == NULL || length == 0 ||
		!parse_hex(parameter, length, destAddr.addr, ETH_ADDRESS_SIZE))
	{
		goto run_foam_lite_xfer_invalid_params;
	}
	xfer.setDestinationAddress(destAddr);

	if (op == FOAM_LITE_HASH_ONLY) {
		keccak256_t signingHash = xfer.signingHash();
		char hexBuf[SHA3_256_DIGEST_LENGTH * 2 + 1];
		for (int i = 0; i < SHA3_256_DIGEST_LENGTH; i++) {
			byte_to_hex(signingHash.hash[i], &hexBuf[i*2]);
		}
		SEND_RESPONSE("%s%s,OK", respEvent, hexBuf);
		return;
	} else {
		signed_message_t signedMsg = { .raw = { 0 } };
		blockchain_error_code_t signErr = xfer.sign(blockchain, &signedMsg);
		if (signErr != BLOCKCHAIN_ERROR_CODE_NO_ERROR) {
			SEND_RESPONSE("%sERROR,%u", respEvent, AT_ERROR_CODE_BLOCKCHAIN_ERROR_BASE + signErr);
			return;
		}

		char msgHexBuf[MAX_RXDATA_LENGTH] = "";
		for (size_t i = 0; i < signedMsg.totalLength; i++) {
			byte_to_hex(signedMsg.raw[i], &msgHexBuf[i*2]);
		}
		SEND_RESPONSE("%s%s,OK", respEvent, msgHexBuf);

		if (op == FOAM_LITE_SIGN_AND_TX) {
			send_packets(0, 0, signedMsg.raw, signedMsg.totalLength, false);
		}

	}

	return;

run_foam_lite_xfer_invalid_params:
	SEND_RESPONSE("%sERROR,%u", respEvent, AT_ERROR_CODE_INVALID_PARAMETERS);
	return;
}
