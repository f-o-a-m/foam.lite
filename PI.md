# Running Foam.lite bridge on a Raspberry Pi 4 RAK Lora hat

## Hardware installation

Connect the Lora hat on the Raspberry Pi GPIO connector in the position that the hat stays over the Pi. The Hat will probably come with riser bolts, you can use them to safely lock the hat in place.

## Software installation

### Install distro

Install the latest Raspberry Pi OS Lite from https://www.raspberrypi.org/software/operating-systems/#raspberry-pi-os-32-bit 

### Install RAK drivers

Follow the instructions here: https://github.com/RAKWireless/rak_common_for_gateway#installation-procedure

When configuring the gateway with `gateway-config` select TTN and the right Lora standard.

Then edit (with sudo) `/opt/ttn-gateway/packet_forwarder/lora_pkt_fwd/global_conf.json`:
 + in `gateway_conf.server_address` make sure it is 127.0.0.1. 
 + in `gateway_conf.serv_port_up` and `gateway_conf.serv_port_down` set the port where you are going to run foam.lite bridge, for example 7000

### Compile Foam.lite

In your desktop, install node 14, purescript 0.13.8 and spago.

Clone https://github.com/f-o-a-m/foam.lite , run `npm i` and then run `npm run bundle-lora`.


### Install Foam.lite

Install npm

```
$ curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash
$ export NVM_DIR="$HOME/.nvm"
$ [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
$ [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
$ nvm install 14
$ nvm use 14
```

Copy the file `dist/lora.js` from your desktop compilation to the Pi to a convenient location, for example `/opt/lora.js`

Copy the whole project where you compiled Foam.lite on your desktop machine to the Pi. For example using `rsync -av` over ssh

## Running Foam.lite

You could run manually foam.lite lora bridge, assuming that you have copied the project in the Pi device to ~/lora

```
$ cd lora
$ export NODE_URL=http://ethnode:8545 # point it to your ethereum node
$ export PACKET_RECEIVER_PORT=7000 # use the same port that you set up in /opt/ttn-gateway/packet_forwarder/lora_pkt_fwd/global_conf.json
$ node dist/lora.js
```

In another terminal:

```
$ sudo service ttn-gateway restart
```
