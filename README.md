# STM8-DMX
Dedicated low cost, low component count, DMX receiver multiple 3 channel outputs 

Ok.. Written in Assembler code.. DMX input decoded using a 'bitbashing' loop. 
Use the free ST Visual developer along with the ST Visual Programmer for an STM8S103f.
The STM8 boards are easily and cheaply available as are the Stlink usb programmers.

Pin Use .. D3 pwm (1Khz) Chan2 Out. D2 pwm Chan3 .. D4 pwm Chan1 Out

           C7 Chan2  C5 Chan3  C3 Chan 1 push pull switched Out.
           
           C6 DMX in via 4k7 and is a pullup input.
           
           C4 is a 1-2mS servo out of Chan2
           
           B4 is an open drain output of Chan1
           
           D5 is a 115200 Uart out with an FF marker followed by chan1 2 3 data divide by 2 ie 0-128
           
           A1 is an output data drive for a WS2812 RGB led
           
           A2 ia an output of the data sampling timing pulses
           
           A3 is a pullup input for the DMX address setting switch. This requires a 0.22uF to ground
           .
To program .. Open ST Vis Programmer. Set for STM8S103 device. Select the programmer as STLINK usb.
Load the S19 files into the program data and options tabs. Program all and thats it.
Alternatively.. create a new workspace in Visual developer for a STM8S103 device and select a name for it.
In the files tab open this name to see the Source and Include folders. There should already be a mapping .asm file in it.
Add main2.asm and stm8s103f.asm to the Source folder. In the Include files folder add the stm8s103f.inc file.

On the build tab select 'build' and an S19  or hex file will be created. (when creating a workspace, file names can be created)
If the programmer is connected, then running a debug session will transfer the file in the chip. Note though, that the option
and data sections will not be loaded, so best first to run the ST vis programmer first. The data memory (eeprom) hold the starting address
of the DMX channels which are sequential.

Ok.. the principle of operation. Simplicity . One switch and one rgb led. At start up, if the datamem holds a valid channel address, this will
be displayed by the rgb led.. The sequence is Green Red Blue. 
Green will blink the number of hundreds in the Address
Red will blink the number of Tens in the Adress
Blue will blink the number of Units in the Address.
No Blink indicates none . ie Address 309 .    Three Green flashes, a pause in Red, then 9 Blue blinks.
Just reset to see it again, or leave the DMX disconnected. (1-2 seconds timeout before a restart)

To set an Address. Press the switch.. This is an interrupt into the set Address mode.
First, the present address in show in blinks.  Then the green will flash... either leave for a 0 or press the switch the required times
Short delay and on to a Red Flash .. time to enter the 10's count, again leave for 0 or press the require count.
Short delay and on the Blue flash .. enter the desired units count.
Wait ... the entered address will be displayed and the unit will now start up. This address will have been stored in eeprom.

When running ... dmx channel 1 will increase the led Green brigtness, channel 2 the led Red brightness, and channel 3 the blue brightness.
Useful for checking all is well.

For interest ... Scoping the DMX and syncing using a Pulse mode to sync on a pulse of > 75uS the sampling pulse output pin (A2) will
indicate the precise time the data gets sampled (per bit) and stored.(a double pulse indicates bit0 of the byte)

This particular dmx program has been functioning for many years in commercial product that I have designed (some many thousands units) and has
proved to be extremely robust. (I am hoping I have ported everything correctly and nothing is going to bite back!).

I found when using a stm8 disco board as a programmer, there was enough power to supply the 103 board from the swim output pins. Just use Gnd, +3.3V and swim pins.
No need for the rst line.

So, here is a cheap and low count DMX receiver that can be interfaced or used standalone. One aspect is that because it is low cost and 
standalone, fault finding and debugging is relatively simple. Sure, all the decoding could be done in a more complex chip, along with
all the other software, in DMA fashion etc. This is just one way of removing overheads from the main processing device.
I know the assembly is structured like a screwed up dishcloth... but it works.
One more reason it's here .... being technically classed as an elderly gentleman, I find documentation filing becoming more tedious and very
slapdash. It's here so I can find it again .... if I can only remember I put it here!

