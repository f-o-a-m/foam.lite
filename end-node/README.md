See the [latest documentation on the FOAM Developer Portal](https://f-o-a-m.github.io/foam.developer/foamlite/end-node.html) for more details and usage instructions.

**USE THIS FIRMWARE AT YOUR OWN RISK. BE MINDFUL OF LOCAL REGULATIONS PERTAINING TO USAGE OF THE RADIO SPECTRUM**

The firmware supplied here has been tested assuming all `make` commands are run with `TARGET_PLATFORM=FEATHER_F405`, which assumes an
Adafruit Feather STM32F405 Express and an RFM95W LoRa Radio FeatherWing.

While it is possible to compile for the `NUCLEO_F303RE` which can be used on the Nucleo-F303RE eval board with the SX1276MB1xAS mBed shield,
this version of the firmware with RelayableNFT functionality has not been tested on that device.


Shout outs to Trezor for making an awesome library of handy crypto utilities and all the hard work by the folks working on libsecp256k1 to make it microcontroller-friendly.
