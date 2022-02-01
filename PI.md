# Running Foam.lite bridge on a Raspberry Pi 4 RAK Lora hat

In this documentation we will show how to set up and operate a Foam lite gateway using a Raspberry Pi 4 and a RAK Lora hat. We will see:

 + How to connect the hardware
 + How to install the Lora RAK drivers and  Foam lite gateway software
 + How to check the installation is working

After this process you will have a usable Foam Lite lora gateway.

## Hardware installation

Our system is composed of two main components:
 + Raspberry Pi 4, an ARM based computer
 + RAK lora Hat, a lora network interface that connects with the Raspberry Pi through an interface called GPIO (https://www.hwlibre.com/en/gpio-raspberry-pi/)

Connect the Lora hat on the Raspberry Pi GPIO connector in the position that the hat stays over the Pi. The Hat will probably come with riser bolts, you can use them to safely lock the hat in place.

After that, you should connect the antenas to the RAK Lora hat

[Insert photo with result]

After this process the Pi has the capability to communicate with the Lora network.

## Software installation

### Install Linux distribution

Install the latest Raspberry Pi OS Lite from https://www.raspberrypi.org/software/operating-systems/#raspberry-pi-os-32-bit into a SD card that you can insert into the Pi.

### Install RAK drivers

RAK, the manufacturer of the Lora hat, has a driver that runs on Raspberry Pi OS and is able to communicate with the Lora network, and convert Lora packets into UDP packets that will be sent to our Foam Lite Lora gateway. First we need to install this driver.

Follow the instructions here: https://github.com/RAKWireless/rak_common_for_gateway#installation-procedure

When configuring the gateway with `gateway-config` select TTN and the right Lora standard.

Then edit (with sudo) `/opt/ttn-gateway/packet_forwarder/lora_pkt_fwd/global_conf.json`:
 + in `gateway_conf.server_address` make sure it is 127.0.0.1. 
 + in `gateway_conf.serv_port_up` and `gateway_conf.serv_port_down` set the port where you are going to run foam.lite bridge, for example 7000

### Compile Foam.lite gateway

Foam.lite gateway is written in purescript a language that compiles to node/JS, at this moment it is not possible to compile purescript directly on the raspberry Pi. Hence, we should compile it to Node/JS on a desktop computer and 

In your desktop, install node 14, purescript 0.13.8 and spago.

Clone https://github.com/f-o-a-m/foam.lite , run `npm i` and then run `npm run bundle-lora`.


### Install Foam.lite gateway into 

We should prepare the Pi to be able to run the software we have compiled on the desktop computer.

In the Pi, run:

```
$ curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash
$ export NVM_DIR="$HOME/.nvm"
$ [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
$ [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
$ nvm install 14
$ nvm use 14
```

Copy the whole project where you compiled Foam.lite on your desktop machine to the Pi. For example using `rsync -av` over ssh

## Running Foam.lite manually

You could run manually foam.lite lora bridge, assuming that you have copied the project in the Pi device to ~/lora, follow this instructions.

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

## Running Foam.lite with systemd

Raspberry Pi OS uses systemd to bootstrap different services that run on the Pi. We can set up systemd so we keep running our Foam.lite lora gateway service.

As root add the file /etc/systemd/system/foam.lite.service, with the following content:

```
[Unit]
Description=Foam Lite lora gateway

[Service]
WorkingDirectory=/home/pi/foam
ExecStart=/home/pi/.nvm/versions/node/v14.18.1/bin/node dist/lora.js
Restart=always
User=pi
Environment="NODE_URL=http://ethnode:8545"
Environment="PACKET_RECEIVER_PORT=7000"

[Install]
WantedBy=multi-user.target
```

Adapting the working directory, node binary, NODE_URL and PACKET_RECEIVER_PORT to your set-up.

Then run,

```
$ sudo systemctl daemon-reload
$ sudo systemctl enable foam.lite
```

Foam.lite lora gateway will then be running always as a systems service on your Pi. You can check it logs with `journalctl -f -u foam.lite`.

## Testing the system

### Sending a invalid message

First thing we can do to check that the gateway is listening for Lora packages, we can send any non-foam lora package and see that the gateway is try to decode it. For example, observe the logs with `journalctl -f -u foam.lite` and send from a lora device the package 0x686F6C61

You should see this on the logs:

```
[INFO] (PushDataJSON { rxpk: (Just [(ReceivedPacket { data: (Base64Encoded "AGhvbGE=") })]) })
[WARN] PUSH DATA packet payload cannot be decoded as a FOAM message
```

The gateway has received the lora package and has acknowledged that it is not a Foam package.

### Minting from a lora device

For last we should check that if we send a Foam package that mints an NFT, that is processed correctly.

Assuming we have the Ethereum node set up and the contracts deployed, we should do the following on our Foam.lite project at our desktop:

```
make bundle

# Make an account that doesn't exist on the Ethereum node
PRIVATE_KEY=$(node dist/helper.js generate-private-key)
echo "The private key is ${PRIVATE_KEY}"

# Faucet the account with some of the FungibleToken we deployed, as well
# as some Ether to let it approve the RelayableNFT contract to spend its FungibleTokens
node dist/helper.js faucet --to-private-key ${PRIVATE_KEY} --also-ether

# Approve the RelayableNFT token to spend the account's balance (and automatically execute the approve() transaction on-chain)
node dist/helper.js approve --private-key ${PRIVATE_KEY} --submit --node-url "http://localhost:8545"

# Double check our balances
node dist/helper.js balance --private-key ${PRIVATE_KEY}

# Make a minting message that creates an "arbitrary" hello-world message
# Using relay-nonce 0, and paying 0.1 FT to the relayer
node dist/helper.js sign-mint --private-key ${PRIVATE_KEY} -a 'Hello World!' --nonce $(node dist/helper.js nonce -p ${PRIVATE_KEY}) --fee-amount 1000000000000000000
```

The last command will print in the console an hexadecimal string that contains the valid Lora message to ming an NFT, for example: `195955fd323a05f96e2ef8c3ae29f811e5de87059379cf964304e7233b1b8998592a7cdecd027ee93440e2a6cec877ea4217db28e9e3fc365d73c9daea5004921c0000000000000000000000000de0b6b3a76400000e413a48656c6c6f20576f726c6421`

We should observe the gateway logs and send this from our lora device. In the logs we will see something like:

```
[DEBUG] received UDP packet from 127.0.0.1:38845
[INFO] "\2|\19\0d_\1\127~\\)Z{\"rxpk\":[{\"tmst\":2066085059,\"time\":\"2021-11-24T15:55:00.158604Z\",\"tmms\":1321804519158,\"chan\":1,\"rfch\":1,\"freq\":868.300000,\"stat\":1,\"modu\":\"LORA\",\"datr\":\"SF7BW125\",\"codr\":\"4/5\",\"lsnr\":9.0,\"rssi\":-22,\"size\":101,\"data\":\"ABlZVf0yOgX5bi74w64p+BHl3ocFk3nPlkME5yM7G4mYWSp83s0Cfuk0QOKmzsh36kIX2yjp4/w2XXPJ2upQBJIcAAAAAAAAAAAAAAAADeC2s6dkAAAOQTpIZWxsbyBXb3JsZCE=\"}]}"
[INFO] PUSH_DATA
[INFO] (PushDataJSON { rxpk: (Just [(ReceivedPacket { data: (Base64Encoded "ABlZVf0yOgX5bi74w64p+BHl3ocFk3nPlkME5yM7G4mYWSp83s0Cfuk0QOKmzsh36kIX2yjp4/w2XXPJ2upQBJIcAAAAAAAAAAAAAAAADeC2s6dkAAAOQTpIZWxsbyBXb3JsZCE=") })]) })
[DEBUG] sending packet
[INFO] relaying message (DecodedMint { feeAmount: 1000000000000000000, nonce: 0, signature: (Signature { r: 0x195955fd323a05f96e2ef8c3ae29f811e5de87059379cf964304e7233b1b8998, s: 0x592a7cdecd027ee93440e2a6cec877ea4217db28e9e3fc365d73c9daea500492, v: 28 }), tokenData: (pack [(mkQuotient 65),(mkQuotient 58),(mkQuotient 72),(mkQuotient 101),(mkQuotient 108),(mkQuotient 108),(mkQuotient 111),(mkQuotient 32),(mkQuotient 87),(mkQuotient 111),(mkQuotient 114),(mkQuotient 108),(mkQuotient 100),(mkQuotient 33)]) })
Nov 24 15:54:38 rak-gateway node[5725]: 2021-11-24T15:54:38.816Z [DEBUG] doing dry run of (DecodedMint { feeAmount: 1000000000000000000, nonce: 0, signature: (Signature { r: 0x195955fd323a05f96e2ef8c3ae29f811e5de87059379cf964304e7233b1b8998, s: 0x592a7cdecd027ee93440e2a6cec877ea4217db28e9e3fc365d73c9daea500492, v: 28 }), tokenData: (pack [(mkQuotient 65),(mkQuotient 58),(mkQuotient 72),(mkQuotient 101),(mkQuotient 108),(mkQuotient 108),(mkQuotient 111),(mkQuotient 32),(mkQuotient 87),(mkQuotient 111),(mkQuotient 114),(mkQuotient 108),(mkQuotient 100),(mkQuotient 33)]) })
[DEBUG] sent
[DEBUG] dry run result: (Right 0x)
[DEBUG] submitting to blockchain(DecodedMint { feeAmount: 1000000000000000000, nonce: 0, signature: (Signature { r: 0x195955fd323a05f96e2ef8c3ae29f811e5de87059379cf964304e7233b1b8998, s: 0x592a7cdecd027ee93440e2a6cec877ea4217db28e9e3fc365d73c9daea500492, v: 28 }), tokenData: (pack [(mkQuotient 65),(mkQuotient 58),(mkQuotient 72),(mkQuotient 101),(mkQuotient 108),(mkQuotient 108),(mkQuotient 111),(mkQuotient 32),(mkQuotient 87),(mkQuotient 111),(mkQuotient 114),(mkQuotient 108),(mkQuotient 100),(mkQuotient 33)]) })
[INFO] transaction successfully relayed with id 0xd9cc55aa569d36e30cdecddf1eb1b60a82621ccb80c132d58ef62a9c09a26990
```

**If this is so, our system is set up correctly.**
