Web Scraping Tutorial // Exercise 3 // My Own Example
================
Martin Kruger (KRGMAR043)
2017-08-21

**Exercise 3: Think of your own scraping example**

A website you think contains useful or interesting information - and put together your own tutorial.

### Netram Online Electronics Shop

<https://www.netram.co.za/>  

I will be attempting to pull a list of all of the store items available on this webpage together with the pricing and description details.

**Steps:**
1.. Visit the [Netram Technologies](https://www.netram.co.za/) webpage and use the **SelectorGadget** tool to find the CSS selectors for item categories (`#categories_block_left`).
2.. Use the **rvest** package to scrape the headings and save them as strings in R.
3.. Read in the webpage XML using `read_html`.

``` r
netram_url <- "https://www.netram.co.za/"
netram_page <- read_html(netram_url)  # Grab main page XML
# netram_page
```

4.. Extract the product categories from the menu in the document with `html_nodes`.

``` r
# Categories available in CSS element:
# '#categories_block_left a'
netram_elements <- html_nodes(x = netram_page, css = "#categories_block_left a")
# netram_elements

head(netram_elements)
```

    ## {xml_nodeset (6)}
    ## [1] <a href="https://www.netram.co.za/662-arduino-shields" title=""> Ard ...
    ## [2] <a href="https://www.netram.co.za/663-audio-video" title=""> Audio & ...
    ## [3] <a href="https://www.netram.co.za/664-audio" title=""> Audio </a>
    ## [4] <a href="https://www.netram.co.za/665-video" title=""> Video </a>
    ## [5] <a href="https://www.netram.co.za/666-books" title=""> Books </a>
    ## [6] <a href="https://www.netram.co.za/667-communication" title=""> Commu ...

5.. To get just the text inside the element nodes we use `html_text`, with `trim = TRUE` to clean up whitespace characters.

``` r
# Clean up data and save to tibble
netram_categories <- html_text(netram_elements, trim = TRUE)
netram_categories <- as.tibble(netram_categories)

category_links <- netram_elements %>% html_attr("href")
netram_categories$link <- category_links  # Add column to category tibble with URL link to page

head(netram_categories)
```

    ## # A tibble: 6 x 2
    ##             value                                         link
    ##             <chr>                                        <chr>
    ## 1 Arduino Shields https://www.netram.co.za/662-arduino-shields
    ## 2   Audio & Video     https://www.netram.co.za/663-audio-video
    ## 3           Audio           https://www.netram.co.za/664-audio
    ## 4           Video           https://www.netram.co.za/665-video
    ## 5           Books           https://www.netram.co.za/666-books
    ## 6   Communication   https://www.netram.co.za/667-communication

6.. For each of the category links, we pull the list of products (`.product-name`).

    ## $`Arduino Shields`
    ## $`Arduino Shields`$links
    ##  [1] "https://www.netram.co.za/3421-protoshield-arduino-pro-mini.html"                            
    ##  [2] "https://www.netram.co.za/2715-ethernet-shield-w5100-arduio.html"                            
    ##  [3] "https://www.netram.co.za/3380-arduino-adapter-shield-turn-nano-to-uno.html"                 
    ##  [4] "https://www.netram.co.za/3555-rpi-gpio-shield-arduino-layout-5v-logic.html"                 
    ##  [5] "https://www.netram.co.za/3119-wireless-arduino-shield-with-2-channel-relay.html"            
    ##  [6] "https://www.netram.co.za/3750-arduino-ir-shield.html"                                       
    ##  [7] "https://www.netram.co.za/2696-relay-shield-v30.html"                                        
    ##  [8] "https://www.netram.co.za/3377-monster-motor-arduino-shield-vnh2sp30.html"                   
    ##  [9] "https://www.netram.co.za/3675-arduino-cnc-shield-v3-kit.html"                               
    ## [10] "https://www.netram.co.za/1264-wixel-shield-for-arduino.html"                                
    ## [11] "https://www.netram.co.za/1958-ramps-14-shield.html"                                         
    ## [12] "https://www.netram.co.za/2018-strain-gauge-shield.html"                                     
    ## [13] "https://www.netram.co.za/1433-nfc-shield-v20.html"                                          
    ## [14] "https://www.netram.co.za/1706-motor-shield-for-arduino-2a.html"                             
    ## [15] "https://www.netram.co.za/2235-bees-shield.html"                                             
    ## [16] "https://www.netram.co.za/2568-ethernet-shield-w5500.html"                                   
    ## [17] "https://www.netram.co.za/2574-arduino-cnc-shield-v3-breakout-board.html"                    
    ## [18] "https://www.netram.co.za/2622-ethernet-with-poe-control-board-w5500-arduino-compatible.html"
    ## [19] "https://www.netram.co.za/2717-energy-monitoring-shield-v2.html"                             
    ## [20] "https://www.netram.co.za/2732-game-joystick-shield.html"                                    
    ## [21] "https://www.netram.co.za/3302-proto-screw-shield.html"                                      
    ## [22] "https://www.netram.co.za/3395-arduino-screw-shield-for-wires-and-terminal.html"             
    ## 
    ## 
    ## $`Audio & Video`
    ## $`Audio & Video`$links
    ##  [1] "https://www.netram.co.za/3496-usb-90-degree-to-video-conversion-transmitter-cable-for-gopro3.html"        
    ##  [2] "https://www.netram.co.za/2712-lm358-operationalsignal-amplifier-module.html"                              
    ##  [3] "https://www.netram.co.za/3540-bluetooth-audio-receiver-and-playback-module-bluetooth-40.html"             
    ##  [4] "https://www.netram.co.za/1675-scream-out-loud.html"                                                       
    ##  [5] "https://www.netram.co.za/3492-fpv-custom-models-ultralight-700-line-mini-camera-n-p-system.html"          
    ##  [6] "https://www.netram.co.za/3609-microphone-breakout-board-max9812-voice-module.html"                        
    ##  [7] "https://www.netram.co.za/2851-audio-bct-2-bone-conducting-transducer-exciter.html"                        
    ##  [8] "https://www.netram.co.za/3624-2x10w-dual-channel-hifi-mini-audio-amplifier-pam8610.html"                  
    ##  [9] "https://www.netram.co.za/2725-wtv020-sd-musicvoice-play-module-mp3-player.html"                           
    ## [10] "https://www.netram.co.za/3258-speakjet.html"                                                              
    ## [11] "https://www.netram.co.za/3464-brushless-gimbal-camera-w-motor-controller-for-dji-phantom-gopro-3-fpv.html"
    ## [12] "https://www.netram.co.za/3495-mobius-fpv-aerial-camera-1080p-hd.html"                                     
    ## [13] "https://www.netram.co.za/3534-hp-720p-hd-laptop-embedded-web-camera-dual-mic-support-rpi.html"            
    ## [14] "https://www.netram.co.za/1046-jpeg-camera-module.html"                                                    
    ## [15] "https://www.netram.co.za/2778-ed8635-bluetooth-40-audio-module.html"                                      
    ## [16] "https://www.netram.co.za/2957-fling-mini-joystick-for-smart-phones.html"                                  
    ## [17] "https://www.netram.co.za/2960-makey-makey-standard.html"                                                  
    ## [18] "https://www.netram.co.za/3346-dmx-shield.html"                                                            
    ## [19] "https://www.netram.co.za/3623-m8s-android-tv-box-media-player-ultra-hd-4k2k.html"                         
    ## [20] "https://www.netram.co.za/2553-pixy-cmucam5-image-sensor.html"                                             
    ## 
    ## 
    ## $Audio
    ## $Audio$links
    ## [1] "https://www.netram.co.za/2712-lm358-operationalsignal-amplifier-module.html"                 
    ## [2] "https://www.netram.co.za/3540-bluetooth-audio-receiver-and-playback-module-bluetooth-40.html"
    ## [3] "https://www.netram.co.za/1675-scream-out-loud.html"                                          
    ## [4] "https://www.netram.co.za/2851-audio-bct-2-bone-conducting-transducer-exciter.html"           
    ## [5] "https://www.netram.co.za/2725-wtv020-sd-musicvoice-play-module-mp3-player.html"              
    ## [6] "https://www.netram.co.za/3258-speakjet.html"                                                 
    ## [7] "https://www.netram.co.za/2778-ed8635-bluetooth-40-audio-module.html"                         
    ## [8] "https://www.netram.co.za/3346-dmx-shield.html"

7.. We now read each of the product pages for the detail data we want and save to a `data.frame`.

8.. Done

    ##           sku        category                                         name
    ## 1   SHL-00011 Arduino Shields                 ProtoShield Arduino Pro Mini
    ## 2   SHL-00056 Arduino Shields               Ethernet Shield (W5100) Arduio
    ## 3 SHL-00021-1 Arduino Shields      Arduino Adapter Shield Turn Nano to UNO
    ## 4   SHL-00068 Arduino Shields   RPI GPIO Shield (Arduino Layout, 5V Logic)
    ## 5   SHL-00064 Arduino Shields Wireless Arduino Shield with 2 Channel Relay
    ## 6   SHL-00065 Arduino Shields                            Arduino IR Shield
    ##    price stock
    ## 1  37.95     8
    ## 2 214.95     7
    ## 3 135.19     7
    ## 4 238.03     5
    ## 5 264.95     4
    ## 6 149.95     4
    ##                                                                              link
    ## 1                 https://www.netram.co.za/3421-protoshield-arduino-pro-mini.html
    ## 2                 https://www.netram.co.za/2715-ethernet-shield-w5100-arduio.html
    ## 3      https://www.netram.co.za/3380-arduino-adapter-shield-turn-nano-to-uno.html
    ## 4      https://www.netram.co.za/3555-rpi-gpio-shield-arduino-layout-5v-logic.html
    ## 5 https://www.netram.co.za/3119-wireless-arduino-shield-with-2-channel-relay.html
    ## 6                            https://www.netram.co.za/3750-arduino-ir-shield.html
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            description
    ## 1                                                                                                                                           This is a small prototyping shield for the Arduino Pro Mini. If your project requires just a little bit more space for your external circuitry, this might be a perfect addition. 38 unconnected PTHs are provided for your prototyping needs.The shield fits the small form factor of the Arduino Pro Mini - connecting to the 'top' four pins on each side of the Mini ('GND'-'TXO' & 'VCC'-'RAW'). There are small buses for VCC, GND and RAW. In addition to the power buses, the serial pins are also broken out on the shield. The analog and digital pins are not broken out on the shield.Note: Headers not included.FeaturesVCC, RAW and GND busesSerial (RXI & TXO) pins broken out to small busesWorks with both 5V and 3.3V Pro Mini versionsUpholds the Pro Mini's small form factor38 unconnected PTHs for prototyping*Please note that Images are for display purposes only
    ## 2 The Arduino Ethernet is a microcontroller board based on the ATmega328 (datasheet). It has 14 digital input/output pins, 6 analog inputs, a 16 MHz crystal oscillator, a RJ45 connection, a power jack, an ICSP header, and a reset button. Specifications:For the Arduino Ethernet W5100 network expansion module, you can make the Arduino a simple Web server or Arduino digital and analog interface network applications through the network control read and write. IDE, Ethernet library files can be used directly to implement a simple Web server.This version support micro SD card (TF card) to read and writeThe expansion board with a stackable design can be directly plugged into the Arduino our other expansion boards can also plug in to.This is a clone without MCU on it and POE functions, but arduino stackable, achieve most functions for ethernet use.Documentation:W5100 DatasheetSchematic  arduino-ethernet-shield-05-schematicReference designGet StartedBildr server tutorialBilder client tutorial
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     With this shield, you can turn your Arduino Nano into a normal style Arduino Uno, so you use it not only as arduino nano itself, but also as a UNO with more standard shield.* Please be aware that we solder the pins of this Arduino nano on the wrong side, so it should be upside down, the Arduino core IC atmega 328 should able to be saw on the top, not the FTDI chipOn the board you have:Power Jack, Power LEDReset ButtonFTDI 4 pins jackStandard Arduino pinsMore GVS extended pins
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                Product DescriptionIf you are an expert but just step into raspberry pi, this item will create a good bridge for your learning purpose. All the Raspberry pi pins are redefined into arduino style pins, so you can focus on python programming and higher level controlling by using RPI.Fully compatible with Arduino, including Arduino shieldsPlugs into Raspberry-Pi GPIO headerVoltage safe Bidirectional translator automatically converts between 3.3V for Raspberry-Pi and 5V for ArduinoExtended interface includes UART, SPI, IICGPIOs are mapped onto the board, only pins A0-A3 is not in used.Module design for model B
    ## 5                                                                                                                                                                                                     The Relay shield is capable of controlling 2 relays. The max switching power could be 10A/250VAC or 10A/30VDC. It could be directly controlled by Arduino/Freaduino through digital IOs with external 9v supply. With buildin xbee/BTBee type socket, it can be wirelessly controlled via Xbee/BTBee(HC-06). Make it an ideal solution for home automation and robotics purpose. Specification:2 channel RelayWith XBBee/BTBee, CC1101 and nRF24L01 wireless interface (Devices not included)Wiht 6 sensor and 3 servo interfaceContact Rating 10A AC 250V / DC 30VMax Switching Voltage AC 250V / DC 30VMax Switching Current 10AElectrical Life (Min) 100,000 OperationsMechanical Life (Min) 10,000,000 OperationsSafety Standard(relay) UL cUL TUV CQCCoil Working Voltage 5VDCPLEASE NOTE: Image is for representation only
    ## 6                                                                                                                                                                                                                                                                        A simple infrared shield including transmitting and receiving function, so the IR signal can be receive, store and replay, so this work flow is a recording. Now you can replace all the remote controller of your domestic appliance by arduino!Two programmer LED and two programmer buttons,  one extra SEND LED serial connects to the Infrared transmitter, so whenever the transmitter is working you can see this LED blink too. See all the pin definition on wiki page.Supported by arduino library IRremote.Note: all the IR signal can be read but not all can be recognized by the code (for example NEC code), some signal will only be read and identified as raw data, but may not re-presentable, this is code issue, please try it yourself.
