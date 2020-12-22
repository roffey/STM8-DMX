stm8/

; Pretty well there  now.
; port use recap for STM8S103 board.
; D3		DMX Chan2 pwm out (1Khz)
; D2		DMX Chan3 pwm out
; D1		swim connection
; C7		DMX Chan2 Push Pull out.
; C6    DMX in (is pullup. 4K7 series in.
; C5		DMX Chan 3 Push/Pull out 
; C4    DMX Chan 2 1-2mS servo out
; C3    DMX Chan 1 Push Pull out
; B4    DMX chan1  open drain pull down high >150  
; A3 		Button for DMX address change (Interrupt)
; 3V3   supply to WS2812 and pullup R's
; 5V		or rather input voltage
; GND
; A2
; A1		Data drive to WS2812
; RST
; D6
; D5	  UART at 115200 baud
; D4		DMX Chan1 pwm out, 

; could try C3 as chan 2 pp output.
; could try C7 as chan 1 pp output.
; ok.. port C5 set to pp out. switch at > 150
; Yup ... all well there with no conflicts apparent.
;PWM's working fine 1Khz period. 
; Need to check all zero's when 1 second time out occurs.
; yup, as long as dmx input is high (as it should be).
; switch and servo reset .... yes
; UART? yes all 0 then held high. and zero's sent every couple of seconds.
; Actually sends 4 zero's maybe needs the ff
;marker sent? is this in the uart setup?
; yes in the clear uart and rgb sub .. changed to add ff marker then 3 zeroes.
;Should maybe have a pp switched output for channel 3 if there is a port free?
; C5? 


;reinstated timer2 registers after using ARF1 alternate pin use.
; pwm outputs working fine on PD 2 3 and 4.
; just need to update them at the end of the dmx suss.
;also make sure they are zeroed on no DMX input.
; noticed DMX input not being pulled up? port c7 needs checking.
; Well, registers set up for it, but no pullup for some reason.
;have to add a resistor here. 47K should do it.
; remember that the 485 chip output should be high when no connection
;so only need the R if this is not used.

; looking at timer 2 now 
; ports for chan1 2 & 3 are PA3[3] aalready used.
; PD2 ( alternate? ARF1] ch2 PD3[2] PD4[ch1]
; pins 1 19 20


;Next on the list are the pwm outputs for the three channels!


; OK.. sorted the fin interrupt.
; had to change the port CR2 registers whilst within the ISR!
; OK. DMX in, Uart out, Switched chan1 and servo all working.
; now back to button interrupt!
;Nope just locks up  and changes eeprom start address to c8?

; Yup .. went pear shape for some reason.21/12/20
; start again. move DMX in to C6 and see. This means more cycles involve so timing delays need changing.
; oh well.. 
; sampling pulses coming out of A2 still
;rgb still out of A1
; serial out at 115200 still out of D5? Yup.
; swutched channel 1 still working D1? yup.
; servo out still working on C4 ? yup.
; ok.... get sampling times sorted.



; Change dmx in to port C7 to help with debugging etc.
;Problems..... locking up when going into interrupt from button press.

; Strange things happening ... comment out TIM2 stuff.
;Locks up for some reason?
;for pwm's could try using TIM2   D4 = CH1 D3 = CH2  D2 = CH3  ???

; Next ... maybe use strip of three rgb's for 100's 10's 1's flash indicator and setting dmx address?
; can any more outputs be used for high frequency pwm's?
;

; Sorted .. remember DMX input should be held high on disconnection
; or else programm will loop and hold data at last input.
; 

;Need to clear dmx data on a 1 second timeout watchdog.
;also send 0's to the UART and rgb. 

; Marker works fine Serial gets shoved out about 375uS after last DMX data sussed.  4 bytes takes 720uS
; Maybe send out 0-128 data and use a 255 as a marker for the reciever?
;ie in reciever -- if serial.available > 3 look for data = 255 then load next 3 bytes.
; try it .. ie srl A
;UART acting up .. only 7 bits of data? Need to load BRR2 before BRR1 !!
; ok Uart looks to be set up,
; try Tx data at end of dmx loop.

;So .. UART1 registers
;UART1_SR status - 7 Tx Empty 6 RxNE recieve not empty idle,overrun,Noise,Frame Error Polarity Error
;UART1_DR  data reg for data to be sent or data recieved.
;UART1_BRR1 BRR2 baudrate  0x05 and 0x07 for 115200
;UART1_CR1  76 for 9th bit of data, 5 Uart disable,4 Mbit 3 WAke 2 PCEN Polarity control 1 PS Parity selection 0 Polarity int  
;UART1_CR2 7 Tx Int 6 Tx Complete int 5 Rx Int 4 idle int 3 Tx Enable 2 Rx Enable 1 Recieve wakeup 0 Send a Break
;UART1_CR3 7 rsv 6 LIN enable 5:4 Stop bits 00= 1stop 3 Clk enable 2 Clock pol 1 Clock phase
;UART1_CR4 7 rsv 6 LIN break int en 5 LIN break len 4 LIN break detect flag 3:0 Address UART mode (slave)
;UART1_CR5 SmartCard and irDA register
;UART1_CR6 LIN Stuff again
;UART1_GTR guard time value for SmartCard mode.

; So, set up baud rate BRR1 & BRR2. Enable Tx and Rx CR2 bits 3 and 2
;that should be pretty much it really , write to UART1_DR reg and it should be transmitted?
; out of pin D6
; do in portsetup.

; want to set up uart ports.  UART1
; baud rate Brr1 = 0x05  Brr2 = 0x07 for 115200
; uart_CR1 reg Mbit =0 PCEN bit = 0 relates to 8 bit no parity.


; it does look like the pulsing sample slots need adjusting slighty
;;between 2 & 3 etc
;yes, timing improved now.

;want to try and use uart1 for transmitting data out, so
; need PD5 and PD6. PD^6 is used for the ws21812 at present.
; see if this can be put on PA1
; Pa1 now set as push pull. change define used for led drive.


; Ports on 103 
; D6 for ws2812
; A3 for button control
; Swim is DMX input via 4K7 in series (swim input)
; B4 is 1st channel switched 0 - 3V3 (On)
; C4 is the servo output. 
; now on 103 can use a port to see the sync on dmx signal.
;  Try PA2 as pushpull out for monitoring? 1694 already set!
; some ports not available on 003's should be set pushpull low.
; so, change d_pulse define for PA2


; ok, channel 1 for open drain with pull up R
; channel 2 for servo output.
; rgb is fine.
; don't think pwm chip will work with rgb chip signal, but worth a  go?

;so PB,#4 needs changing to output pullup in portsetup

;add pull up R to pin 6, and change it's port config to output open drain
;change servo output to act from DMX2 value, which means changing dmx1val word variable to dmx2val
;have dmx1 set output pin 6 high when > 60 percent. ie. 155.
;pin 7 is servo output.

; dmx in now can use swim input and unit can still be programmed.
;this means port that was used for dmx input can be used as an output as well as having servo 
;or pwm drive output.
;old dmx in was port PB,#4 


;nearly works, something not quite right though, 
;check timing. swap rgb port again, to get timing pulses out.
; something wrong with timing between frames?
; looking at wrong port bit between dmx slots, sorted, so change back monitor pulse 
;using rgbled


; defines in place, so can try using pd1,#1 as input port.
; will have to program chip first, then connect swim port to dmx input
;and see if works.
; define DMX_action as SWAP so can be replaced with SRL if required.
; replaced
;replace and A,#16 with DMX_AND

;new defines in place for DMX_in (port and bit),DMX_port(whole port), and DMX_AND,
;this is the value and'd with the port value in order to get the bit value of the dmx (1 or 0)
; at present is PB_IDR,#4. ie bit 4 value 16 to and with.
; will be PD_IDR,#1  ie bit 1 value 2 to and with.
; Swap is used when bit is 4 to get it into bit 0 for loading into 'sample'
; srl will be used when input is PD_IDR,#1 (swim input)
; better to copy all this file into a new one.





; now want to try swim input port to receive dmx signal.
;this is input port

; rgb port and led doesn't like the pulses!
; but port can be used to check timing, then set the port back
; to an unused one.
; now for checking if dmx pulse monitor affects rgb writing
; when using rgb output port as dmx sample timing monitor.
;ok, all sorted now .... brightness defined for setting rgb bright.
; clearam is called if dmx at start, so bright needs to be loaded after
; a call to clearam

;bright variable is set after a Word variable, so its possible
;that the data space for it is getting overwritten when the word variable
; is accessed or used.
;didn't work
;works if dmx is not on at start up.
;if dmx is on at start up then bright gets zeroed?
;insert an unused variable before the bright variable.

;rgbled not working for some reason? after defining d_pulse
; want to see if sample pulses can be put out on rgb port
;and not effect the rgb led.
;presently pa odr,#3 is used port, just load register for delay.
;define this as d_pulse

;							ld A,PB_IDR
							;	and A,#16
							;	nop ;srl A
							;	nop ;srl A
							;	SWAP A
							;	add A,sample
							;	ld sample,A
							;	sll sample  

;at present port B is looked at, specifically #4. Bit 4 represented by decimal 16
;so, A is and'd with 16, to clear all other bits.
;this bit has to become bit 0, so swap is used, to swap nibbles, and thus bit 4
;becomes a bit 0. then added to bit 0 of 'sample'.
; all 8 bits are sampled and left shifted into sample, so the whole dmx byte is retrieved.

;have to find a way of dealing with port D,#1 in the same way with the same timing.
; this would be and with 2, shift right, then compensate and add to sample.
;swap cycles as opposed to shift right. 
; swap A is one cycle length.
; srl  A is one cycle length.



; swim pin8 is PD1, apparently set to input pullup.
; check it to see if DMX will work, and that swim programming will also work.
; present dmx input port is PB5 

; check that PB,#4 is set to pullup (dmx input)
;The PA2, PB0, PB1, PB2, PB3, PB6, PB7, PC1, PC2, PC7, PD0, PD2, PD4, PD7, PE5 and PF4 GPIOs should be configured after ;device reset in output push-pull mode with output low-state to reduce the device’s consumption and to improve its EMC ;immunity
;like to try using swim input for dmx.
;put 4k3 in series from 485 output.
; swim still works if 4k3 taken to ground or positive.
;

;dimming sorted (reduced current consumption.

; 'clear' and 'set8' replaced with call to rgbled, using rgb1,2and3 variables now.
; lot of call set8 and clear used to set leds, so need another routine for rgb's

; In order to reduce running current, put in brightness control for rgb leds
; use 0 to 100 for percentage. so value X percentage / 100.
 ; use mul  ie X x A, where low byte put in X,
 ; then use div X,A where A X is divided by 100 to get answer in XL
;'rgbled' uses rgb1,rgb2,and rgb3 variables for green red blue.
; so use this in place of set8 and clear functions.
; put percentage brightness function in rgb led before data is sent out 


;PIN 1 = WS2812 rgb
;PIN 2 = GND
;PIN 3 = VCAP
;PIN 4 = VCC
;PIN 5 = PULLUP INPUT SWITCH TO GND
;PIN 6 = was DMX INPUT now open drain switch on channel 1
;PIN 7 = SERVO
;PIN 8 = SWIM PIN and dmx input


; Servo works fine now.

; timer prescale needs doubling to 64 
; add variable dmx1val ..... 
;need to add a byte to a word .. issues again.
; create a word variable and use it's actual address.
;store the low byte in it, then add word, using the actual address
; as in tenval example
;timer pwm ccr4 registers filled in 'dmx1'routine 1109
; remove comment out for timerset
; comment out 1069 call to ws2811
; prescale at 32 gives 4uS period out.
;Auto reload at 5000 gives 20mS period.
; then use CCR4 register for pwm width.
; ie 250 for 1ms
; need to load CCR4 L & H from 250 to 500
;so add dmx value to 250 then load these registers

; timer set up, need to disable ws2811 drive
; 127 to 255 will give 1 - 2 mS pulse width.
; so 0-255 needs adjusting.
; 
; servo on Tim1 ch4 pin 7. get it going at 50hZ?
; Aworking quit well now and puts out 3 dmx lines into pwm.
; would like to control a servo. Only needs to be 1 - 2 ms with 1.5ms centre.
; maybe program second chip and let it be driven by the same rs485 chip.
;
; So, duplicate this file and set up a timer pwm port instead of the ws2811 output.

; watchdog refreshed during dmx address reset sorts issues there.
;remember that tenval is stores as a word, and the low value is pulled
;from its address +1.
;if variables are added to the list before the tenval position, then
;its address will change.
;this must therefore be changed in the interrupt routine in order
;that the address can be properly calculated, else the tens and units
;value will not get added.

;with dmx running, watchdog resetting and stopping address 
;routine?
;try refreshing watchdog in interrupt routine?


;be good to have colour rgb led respond to dmx1,2,3 settings.
; look at ws2811
;timing sorted and dmx3 now being picked up correctly
;so, put PA #3 back to an input with interrupt.

;have to change PA_port 3 back to an output temporarily to see
;whats going on, since dmx 3 is not being correctly detected.
; line 1501 in setup remove test for high before setting interrupt
;restore when sorted.

;only display dmx when button pressed and held.
;when released flash green for hundreds
; press number of hundreds wanted.
; wait for red flash
; now press button for number of tens
;wait for blue flash
;now press number of units in address
;wait for display of address.
;now press and release button to exit.




; need cleaner way of inputting dmx value.

; in portsetup wait for button pin PA,#3 to go high before setting as an interrupt pin
; else setting dmx gets called which will clear the eeprom.



; the routine is such that a single button press will cause an interrupt.
; this will then wait for about 5 seconds, during which subsequent button presses
; determines the hundreds value.
; a next key press sets the tens value, and finally the third button press
;determines the units value.
;not greater than 508 or less than 1 will be accepted.


;Needs an absolute address in order to load the ram variable when it is set to be a word value
; this variable is at $81, and therefore a low bit needs to be sent into $82 address.
; after the whole variable has been loaded with zero.
;problem at 1837, trying to add byte value to word value form memory store
;high and low bytes getting mixed uo somehow.

;25/10/19

; remember to use rim in order for interrupts to work!
; need count button presses, and time out , so that huns,tens units can be counted.


; ok.... port use changed to pullup int.... check it. 
; port A interrupy on pin5 (at present led drive to monitor dmx sample slots.
;button to ground must be wired to it.
;dmx routine which uses command to set/reset this port should still work for timing
; but will not show as a pulse.

; set up portA EXT_CR1 no change since it is falling edge & low level.
; set up ddr as 0, cr1 as 1 and cr2 as 1 pullup interrupt.irq extIO irq no 3
; vector add 8014.
; point this to a routine end with reti.

;now need a way of setting the dmx channel?
;via push switch pulses. 
; call via interrupt on push switch pin?
; blink green and wait for pulses
; after wait for 1 second
; blink red and wait for pulses
; wait for one second
; blink blue and wait for pulses.
; wait for  1 second and display result.
; no pulse in one second = 0.

; use pin present used for dmx pulse output.
; this is pin 5 of the chip and carrier.
; this is port Port A3.*

;ok..... blinking rgb led displays dmx channel number now.
;green = hundreds
;red = tens
;blue = units.
; long blink = 0


;going to try using a ws2811 pwm led driver as pwm to 0-5v output for three channels
; seems to have slightly different timings to ws2812 rgb led, to need to try it out.
; try using led pwm timer output pin as the drive pin, so timer needs to be disabled.
; ;try using shift left through carry to determine 0 or 1 routine for ws2811 output pulses.

; first ... disable timer output drive. i.e dont call timer setup
;add first8, second8, third8 as variables to be left shifted in ws2811 driver.
 
;Yes ..... this now works...
;need to get pwm into 0 - 5V output. Just 9k1 into 2uF at the moment.

;now want to display and set dmx channel.



;using stm8s001j3 chip on demo pcb.
 ; not using pin8 the swim pin at the moment.
 ; pin7 is the timer pwm output.
 ; will disconnect the pcb button as it has capacitance connected to pin 7.
 ; will be using PB4 pin6 as dmx input
 ; will be using PA3 pin5 as led output
 ;so change these in the dmx routine
 ;in dmx routine PB_IDR,#0 was used to sample the input
 ; now is PB_IDR,#4
 ;sll is actually used, which creates a problem which needs sorting out.
 ; use SWAP ... this uses 4 machine cycles, so compensation will be required.
 ;Timing sorted and dmx working ......
 ;watchdog timeout working but at the moment the led is not blinking
 ; delay put in watchdog reset so that led has very short blink.
 
 ; need to review how dmx channel can be displayed and set?
 ; this will need a switch input pin. 
 
 ;at present .... led output pin, dmx input pin, pwm output pin.
 
 ;use PD6 pin1 as ws2812 led drive.
 ;requires .4us high .8us low for 0, .8us high .4us low for 1, greater than 50uS for reset.
 ; 3 cycles for .4 us, 6 cycles for .8uS
 
 
 
	title "main.asm"
	MOTOROLA
		
	#include "mapping.inc"
 ; #include "stm8s001j3.inc"
	#include "stm8s103f.inc"
			
	#define dmx_cnt_comp 200 		
	#define led PA_ODR,#1; changed to use pd6 as uart rx   PD_ODR,#6		; drive to rgb
	#define WS  PC_ODR,#4   ; trial for servo		
	#define DMX_in PC_IDR,#6 ;PD_IDR,#1 ;PB_IDR,#4		;dmx port and bit
	#define DMX_AND #%01000000 ;#16		      ; which bit to AN
	#define DMX_port PC_IDR ;PB_IDR    ;basic dmx input register.
	; bit 7 of portc is sampled and has to be put in bit 0 of A so need rll and rll  now
	#define DMX_action rlc ;srl ;SWAP     ;action after port has been read and and'd
	#define d_pulse PA_ODR,#2 ;PA_ODR,#2 timing pulse monitor, was 3 for 003. dummy drive to an input port for dmx timing
  #define brightness #20  ;percentage brightness for rgb
   segment 'ram0'
	
; Place RAM based variables here

;variables dmxval1,dmx2val,dmx1,dmx2 and various others to go in here.	
; weed out variables not actually used
.rub1		DS.B	1
.rub2		DS.B	1
.rub3		DS.B	1
.rgb1 DS.B 1
.rgb2 DS.B 1
.rgb3 DS.B 1
.dmxval1	DS.B	1
.dmxval2	DS.B	1
.dmx1		DS.B	1
.dmx2		DS.B	1	
.dmxcountl	DS.B	1
.dmxcounth	DS.B	1
.bin1		DS.B	1
.bin2		DS.B	1
.sample		DS.B	1
.countr1	DS.B	1
.countr2	DS.B	1
.countr3	DS.B	1
.getit		DS.B	1
.ledhun		DS.B	1		;first 7 seg  (hundreds)
.ledten		DS.B	1
.ledunit	DS.B	1
.led1to8	DS.B	1
.compH		DS.B	1
.compL		DS.B 1
.ptrH		DS.B	1
.ptrL		DS.B	1
.voted1       DS.B 1
.first8 DS.B 1
.second8 DS.B 1
.third8 DS.B 1
.med1		DS.B 1
.variable1		DS.B	1  ; these are last sampled bits of the dmx data
.variable2		DS.B	1	;including the last to be thrown out of the shifts when new data arrives. (variable0)
.variable3		DS.B	1
.variable4		DS.B	1
.variable5  	DS.B	1	
.voted1a	DS.B 1
.med1a		DS.B 1
.variable1a		DS.B	1  ; these are last sampled bits of the dmx data
.variable2a		DS.B	1	;including the last to be thrown out of the shifts when new data arrives. (variable0)
.variable3a		DS.B	1
.variable4a		DS.B	1
.variable5a  	DS.B	1	
.voted1b			DS.B 1
.med1b		DS.B 1
.variable1b		DS.B	1  ; these are last sampled bits of the dmx data
.variable2b		DS.B	1	;including the last to be thrown out of the shifts when new data arrives. (variable0)
.variable3b		DS.B	1
.variable4b		DS.B	1
.variable5b  	DS.B	1	
.stored1a			DS.B 1
.stored1b			DS.B 1
.subcounter	DS.B	1
.last_haze	DS.B	1
.last_fan	DS.B	1
.dmxl_ram	DS.B	1
.dmxh_ram	DS.B	1
.dmxh_ramlo DS.B 1  ; loading X wants 2 bytes, so this is cleared
.lastdmx1	DS.B	1
.lastdmx2	DS.B    1
.updateflag	DS.B	1
.dmx3     DS.B	1
.dmxval3 DS.B 1
.dmxramv	DS.B	1
.dmxsample0 DS.B 1
.dmxsample1 DS.B 1
.dmxsample2 DS.B 1
.dmxsample3 DS.B 1
.dmxsample4 DS.B 1
.dmxsample5 DS.B 1
.counterdiv     DS.B	1
.resultdiv      DS.B	1
.remainddiv	DS.B	1
.holdtime	DS.B	1  
.random		DS.B	1  
.ledcount	DS.B	1  
.subledcnt	DS.B	1  
.dividend	DS.L	1
.divisor	DS.W	1
.tempquot	DS.L	1
.quotient	DS.W	1
.scratchpad DS.B 1
.first_start DS.B 1
.displaycount DS.B 1
.delay1 DS.B 1			; timing loop variable
.stored1 DS.B 1
.spidat0	DS.B 1 
.spidat1	DS.B 1 
.spidat2	DS.B 1 
.haze	DS.B 1 
.fan	DS.B 1 
.ptc_ntc	DS.B 1 
.dmx_select DS.B 1
.dmx_sw	DS.B 1 	;controls haow fast address changes when selecting
.system_integ DS.B 1
.disp_now DS.B 1
.haze_h DS.B 1
.haze_l DS.B 1
.fan_h DS.B 1
.fan_l DS.B 1
.haze_timeout DS.W 1
.fan_timeout DS.W 1
.haze_t DS.B  1; used for testing, and whether to hold reading to prevent display jitter.
.fan_t DS.B   1
.hazeX2 DS.B 1
.fanX2 DS.B 1
.displaycnt DS.B 1
.prog_d   DS.B 1         ;flag for eeprom being prog_d, flash more if so, then reset
.into_eeproml DS.B 1
.into_eepromh DS.B 1
.dispmodel_flag DS.B 1

.fail_flag DS.B 1  ;
.dmxadd_first DS.B 1; flag for no adress change until fan pot centred
.ptc_reached DS.B 1;indicates that ptc has reached higher accepted temp
										;and that a lower one will be used as a cooled too much
.flo_adjust
.dmx_start_cnt DS.B 1
.blinkhun DS.B 1
.blinkten DS.B 1
.blinkunit DS.B 1

.buthun DS.B 1
.butten DS.B 1
.butunit DS.B 1
.butflag DS.B 1 ; counts up for each interrupt to set huns, tens or units.
.tenval DS.W 1
.unitval DS.B 1
.dmx2val DS.W 1  ; don't add variables before this, as the actual address is used to refer to it.
.padding DS.B 1 ; dmx1val word can overwrite next variable space, so add
								; and unused variable space
.bright DS.B 1

.memend DS.B 1  ;end of memory list marker for clearing variables
     segment 'eeprom'

.eep1		DS.B	1
.eep2		DS.B	1
.eep3 DS.B	1
.eep4     DS.B	1
.eep5       DS.B	1
.eep6       DS.B	1
.dmxl_ep        DS.B	1
.dmxh_ep        DS.B	1
.minutes_ep	DS.B 1
.hours_ep DS.B 1
.days_ep DS.B 1
.weeks_ep DS.B 1
.years_ep DS.B 1
	


	segment 'rom'

.reset

main.l
	; initialize SP
	ldw X,#stack_end
	ldw SP,X
				
;				ldw X,#50000
;settle	decw X
;				jrne settle
	      ld A,#01
				ld CLK_CKDIVR,A     ;/2     ;clock driver setup
	      
				
				clr butflag
				clr dmx1
				clr dmx2
				clr dmxval1
				clr dmxval2
	      clr dmx3
				clr dmxval3
			 ;allow time for port A button cap to charge
			 ;else will detect a low, triggering an interrupt
			 ;which will clear the dmx eeprom value.
      ; in portsetup wait until PA #3 has gone high before setting as interrupt
			;else false int at startup clears eeprom.
			 mov bright,brightness;  percentage of brightness for rgb
			 call portsetup
	     call timerset
       call clr_rgb_uart
; test loop for WS2811 drive going in here
       rim    ; allow interrupts.



      ld A,dmxh_ep
			cp A,#2
			jrc eeprom_ok
			ld A,dmxl_ep
			ld dmxl_ram,A
			ld A,#0
			ld dmxh_ram,A
			
      		
      call eeprom

eeprom_ok
      ldw X,#0
			ldw tenval,X	
			ld A,dmxl_ep
			ld dmxl_ram,A
			ld A,dmxh_ep
			ld dmxh_ram,A

st_again				
				mov fan,#255
loopy	 mov first8,fan
			 mov second8,fan
			 mov third8,fan
			 call ws2811 
			 
			 ldw x,#$ffff
nee1		decw x
				jrne nee1
			  dec fan
			  jreq st_again
			call display_dmx




			ldw x,#$0fff
nee			decw x
				jrne nee
			mov rgb1,#0
			mov rgb2,#255
			mov rgb3,#255
			call rgbled
			
			;call clear
			;call set8
			;call set8
			;jp eeprom_ok
			
			
			
			
			
			
			
			btjt RST_SR,#1,miss_model   ;watchdog reset flag
			btjt RST_SR,#0,miss_model
				
						clr dmx_start_cnt
						clr dmx1
						clr dmx2
						clr dmx3
						clr ptc_reached
						call clearam
						ld A,dmxl_ep
						ld dmxl_ram,A
						ld A,dmxh_ep
						ld dmxh_ram,A
					  clr fail_flag	
						mov bright,brightness
						
						
						
						ldw X,#0
						ld A,#0



miss_model

           mov IWDG_KR,#$CC ; enable watchdog   for dmx timeout
					 mov IWDG_KR,#$55
					 mov IWDG_PR,#6 ;prescale /256
					 mov IWDG_RLR,#$FF
					 mov IWDG_KR,#$AA    ;1 second timeout

						btjt RST_SR,#1,reset_path   ;watchdog reset flag
						btjt RST_SR,#0,reset_path
						jp normalstuff
						
reset_path	bset RST_SR,#0 ;clears the rst_SR watchdog flag
						bset RST_SR,#1 ;clears independent watchdog flag
						
						;load the reload value into the IWDG and prescale value of FF
						
						mov IWDG_KR,#$55 ;refresh watchdog
						bres d_pulse
            ldw X,#$FFFF
ledblink		decw X
						jrne ledblink

normalstuff
				
				
				   clr dmx_sw
				   call ep2ram             ;copies eeprom variables to ram
           jp maindmx						



;****************************************************************
;*		Main Program Loop  DMX routine area
;****************************************************************
               
.maindmx
	; main loop
					; convert reading to display	
		
                ld A,dmxl_ram
                ld bin1,A
                ld A,dmxh_ram
                ld bin2,A
                               
                clr sample
                clr dmxcountl
                clr dmxcounth
               
                clr dmx1
                clr dmx2
                clr dmx3
               ;check not in change address mode
							 ld A,dmx_select
							 cp A,#100
							 jrnc main2
							 clr dmxadd_first  ;prevents address being changed before fan pot centered when going into routine            
main2                
                                  
                clr dmx1
                clr dmx2
                clr dmx3
								
								ld A,#dmx_cnt_comp;50
								cp A,dmx_start_cnt
								jrnc inmode2
								
								               ;bypass dmx routine.


             
inmode2					;call dispdmx
								;call sevenseg
					
								

bypassm2			;	call exdatdmxG300 ; set up timer d/a outputs
								
		             
                
horiz           clr dmxcountl
                clr dmxcounth
 
                  ;************************************************************************************************
             
                ;first suss out a break period so that a synchronised starting
                ;point ensues.
                ;This first suss will not contain any data manipulation
                ;the second break suss in the main loop will provide
                ;the data manipulation and use an interrupt to reset
                ; the chip if the break is not correct, so that all
                ;will start again.
                                               
                clr getit 
Sstartag                ;FIRST BREAK AND SYNC SUSS
                 
                 ;port PB_ODR,#7 is now the led indicator  was PB 4
                 ;port PD_IDR,#2 is now the 485 input      was PC 0
                 
									mov IWDG_KR,#$AA   ;watchdog key reg refresh
dmxgo                 
                 clr countr1
                 clr countr2
                 clr countr3
                 bset d_pulse ;#6 ;turn indicator on
								jp Swaitchi2 ; TI chip remains in high state always


Swaitchi2       mov IWDG_KR,$AA
								
								

Swaitchi2x			btjt DMX_in,Swaitchi2x ;from A0
								mov IWDG_KR,$AA ; refresh the watchdog values
								
								;Now sense a low must  to move on
                ;it must be low for at least 88uS to be recognised
                ;as a real break period
                
                ;So next loop and keep checking for a low
                ;if high at any time during this period
                ;then it is not an actual break, so start again
                ;until it is found.
                clr countr1
                clr countr2
                clr countr3
                ld A,#61;64
Swait88         btjt DMX_in,Sstartagx ;from A0
								dec A
                jrne Swait88 ;one loop = 1.375uS
                ;if the routine reaches here then a real break has been
                ;sussed. (i.e. low for at least 88uS)
                ; A Mark After Break is the next official protocol
                ;before a Start bit is recognised
                ;This is a high period of a least 8uS
                
                ;so wait for a MAB mark after break going high
                ;Then go into main loop and data retrieval
                        
                jp notmab
Sstartagx       jp Sstartag
notmab          btjt DMX_in,notstarta ;form A0
                
;this is the only other point that will loop if the
;signal is low.
; so use timeout and jump back to main
;each loop takes 2uS and mab is at least 8uS so no real probs                
                 inc countr1
                 jrne notmess
                 clr countr1
                 inc countr2
                 jrne notmess
                 clr countr2
                 inc countr3
notmess          btjf countr3,#3,notmab
                 jp main2
                        
notstarta      							
								;now in mab period
                ;wait for low - ie start bit
notstart        btjt DMX_in,notstart ;moveon;  notstart normally btjt
																
								;
								
;This is the falling edge of the start bit, where the getit flag is checked.
;if the getit flag is high, then an immediate jump to getting the data is made.
;if the getit flag is not raised, then the 'fetch address' counters are incremented
;and checked against the set dmx address. If there is a match then the getit flag is raised.
;This means the next frame will be sussed.

;So, in the frame where the address is being checked in order to set the getit flag,
; there must be a time padding to get in the middle of the last two stop bits,
;so that the 'notstart' get be looped ready for the next frame start.
								
								
								
								
								;start edge is here
                ;detects in 1 to 1.5uS
                ;so wait 4.5uS so sample is taken at 5.5 to 6 uS
                ;in the middle of lsb.
								
;this is the start of a new packet when hit after a break period or the
;data frame prior to that which will be collected.
;it needs to be correctly timed and include up counters and
;a flag set for the next frame if it is to be captured.

;first look a getit flag variable and either run a dummy with counter and compare
;or run the real capture routine.
;moveon									
                  btjt getit,#7,doit
;false frame area
;needs to be 42uS
;Routine below takes about 7.2uS



                                 
testit          bset d_pulse     ;5
                ld A,dmxcountl   ;3
                cp A,#255        ;3     
                jreq uph         ;3
                nop              ;time equalise
                inc dmxcountl    ;5
                jp notup         ;2
uph             inc dmxcounth    ;5
                clr dmxcountl    ;5
notup           ;channel counter
                
                
                ld A,bin1	;3	;test channel set number
                cp A,dmxcountl  ;3
                jreq testnext   ;3
                jp timeeq1      ;2      
testnext       ld A,bin2        ;3      ;compares against switches
                cp A,dmxcounth  ;3
                jrne nothere    ;3
                bset getit,#7   ;5      ;next frame a real capture
                jp isset        ;2

timeeq1         nop         ;2
		nop         ;2				;watchdog reset compensation remove 3 nops
		nop         ;2
		jp isset   ;2

nothere         nop
		jp isset                                  
                        

               
isset                                                       
                bres d_pulse    ;5

                
                ld A,#104 ;40  ** 104 for 16Mhz stm8s003 chip
framedel        dec A
                jrne framedel
                bset d_pulse
                nop
                bres d_pulse
                clr A
								jp notstart        ;2

doit              ld A,#11;3;4       ; check this timing out
midlsb          dec A
                jrne midlsb
                
                bset d_pulse ;#6
                bres d_pulse ;#6
                bset d_pulse
								bres d_pulse
								;yup its here
                ld A,DMX_port
								rcf
								and A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								ld sample,A
								sll sample              ;bit0

								call dmxfill     ;timing delay

								ld A,#1
pad1						dec A
								jrne pad1
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								and A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								add A,sample
								ld sample,A
								sll sample              ;bit1
							;	nop
							;	nop
		
								call dmxfill
		 		 
		
		
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								and A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								add A,sample
								ld sample,A
								sll sample               ;bit 2
								call dmxfill
		            
								
								
								
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								and A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								 ;srl A
								 ;srl A
								add A,sample
								ld sample,A
								sll sample               ;bit 3
								call dmxfill
		
		            
								
								
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								and A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								add A,sample
								ld sample,A
								sll sample               ;bit 4
                call dmxfill ; nop
                bset d_pulse ;#6
                bres d_pulse ;#6
								Ld A,DMX_port
								rcf
								and A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								add A,sample
								ld sample,A
								sll sample               ;bit 5
								call dmxfill
		            
								
								
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								and A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								 ;srl A
								 ;srl A
								add A,sample
								ld sample,A
								sll sample               ;bit 6
     
								call dmxfill
		 
								
								
                bset d_pulse ;#6
                bres d_pulse ;#6
								ld A,DMX_port
								rcf
								and A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								add A,sample
								ld sample,A
		
                ;now there should be two stop bits
                ;ie high level                
            
								call dmxfill
								bset d_pulse ;#6          ;
                call dmxfill
								ld A,sample             ; is actual sampled value.
                ld dmxval1,A
                clr sample
                bres d_pulse
;******************************* second dmx value **********************                
notbtart        
								btjt DMX_in,notbtart ;from A0
                ;start edge is here
                ;detects in 1 to 1.5uS
                ;so wait 4.5uS so sample is taken at 5.5 to 6 uS
                ;in the middle of lsb.
                ld A,#11
midlsbb         dec A
                jrne midlsbb
                nop
                bset d_pulse
								bres d_pulse
								bset d_pulse ;#6
                bres d_pulse ;#6
                ;yup its here
                ld A,DMX_port
								rcf            ;reset carry flag
								AND A,DMX_AND
								DMX_action A   ;rotate thru to bit 0
								DMX_action A
								DMX_action A
								ld sample,A
								sll sample              ;bit0
								call dmxfill
		
								ld A,#1
pad1b						dec A
								jrne pad1b
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								AND A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								add A,sample
								ld sample,A
								sll sample              ;bit1
								
								call dmxfill
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								AND A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								add A,sample
								ld sample,A
								sll sample               ;bit 2
								call dmxfill
								
											
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								AND A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								add A,sample
								ld sample,A
								sll sample               ;bit 3
								call dmxfill
		            
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								AND A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								add A,sample
								ld sample,A
								sll sample               ;bit 4
                
								call dmxfill
								bset d_pulse ;#6
                bres d_pulse ;#6
								rcf
								ld A,DMX_port
								AND A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								add A,sample
								ld sample,A
								sll sample               ;bit 5
								call dmxfill
		
                
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								AND A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								add A,sample
								ld sample,A
								sll sample               ;bit 6
                call dmxfill
							  
								bset d_pulse ;#6
                bres d_pulse ;#6
								ld A,DMX_port           ;bit7
								rcf
								AND A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A
								add A,sample
								ld sample,A
								call dmxfill		
							 ;now there should be two stop bits
                ;ie high level                
                bset d_pulse ;#6          ;              
                
                ld A,sample
                ld dmxval2,A
                clr sample
                bres d_pulse
;************************************** THIRD DMX VALUE ******************************                
notctart        
								btjt DMX_in,notctart ;from A0
                ;start edge is here
                ;detects in 1 to 1.5uS
                ;so wait 4.5uS so sample is taken at 5.5 to 6 uS
                ;in the middle of lsb.
                ld A,#11
midlsbc         dec A
                jrne midlsbc
                nop
               							
								bset d_pulse ;#6
                bres d_pulse ;#6
                bset d_pulse ;#6
                bres d_pulse ;
								;yup its here
                ld A,DMX_port     ;port c7
								rcf
								AND A,DMX_AND     ; #7
								DMX_action A      ;rlc right thru carry
								DMX_action A			;rlc right thru carry
								DMX_action A 
								ld sample,A       ; should be into bit 0
								sll sample        ;bit0 shift left into bit 1 ready for next sample
								call dmxfill
		
								ld A,#1
pad1c						dec A
								jrne pad1c
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								AND A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A 
								add A,sample
								ld sample,A
								sll sample              ;bit1
							
								call dmxfill
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								AND A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A 
								add A,sample
								ld sample,A
								sll sample               ;bit 2
								call dmxfill
								
								
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								AND A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A 
								add A,sample
								ld sample,A
								sll sample               ;bit 3
								call dmxfill
								
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								AND A,DMX_AND
								rcf
								DMX_action A
								DMX_action A
								DMX_action A 
								add A,sample
								ld sample,A
								sll sample               ;bit 4
                call dmxfill
							  
								
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								AND A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A 
								add A,sample
								ld sample,A
								sll sample               ;bit 5
								call dmxfill
								
								
								bset d_pulse ;#6
								bres d_pulse ;#6
								ld A,DMX_port
								rcf
								AND A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A 
								add A,sample
								ld sample,A
								sll sample               ;bit 6
                call dmxfill
								
								bset d_pulse ;#6
                bres d_pulse ;#6
								ld A,DMX_port
								rcf
								AND A,DMX_AND
								DMX_action A
								DMX_action A
								DMX_action A 
								add A,sample
								ld sample,A
								call dmxfill
													 ;bit 7
                ;now there should be two stop bits
                ;ie high level                
                bset d_pulse ;#6          ;              
                ld A,sample
                ld dmxval3,A
                clr sample
                bres d_pulse
            
;                
;                
;***************************************** SORT DMX DATA OUT ***************************                


							
bypass_dmx

								
								ld A,#255
dmxstatus				dec A
								jrne dmxstatus
								
								
								call dmx1bit
                bset d_pulse
                call dmx2bit				;puts data recieved into dmx1,2,3,4
                bres d_pulse
                call dmx3bit                       
                bres d_pulse
                
                clr dmxcountl
                clr dmxcounth
                clr getit
                bres d_pulse
                
                
testloop        ;

								ld A,dmx1		 
								srl A
								XOR A,lastdmx1
								jreq next_dmxchan
								ld A,dmx1
								srl A
								ld lastdmx1,A
								bres spidat0,#3
								bset spidat0,#2
next_dmxchan                
								ld A,dmx2	 
								srl A
								XOR A,lastdmx2
								jreq noval
								ld A,dmx2
								srl A
								ld lastdmx2,A
								bres spidat0,#2
								bset spidat0,#3
noval                

									bset d_pulse     ;this next lot takes about 220uS after val sus
                 
								 ld A,spidat0
								 AND A,#%00001100 ; in dmx mode so either bit 2 or 3 should be high
																	;so that dmx values are shown
																	; at reset this may be clear
																	;so set up Haze to show the value
																	;rather than the manual haze value
									jrne no_need2set
									bset spidat0,#2
no_need2set
									

                  
;  added delay 8th Aug to see if improves glitching
									
								
								;	jp reset
           ;Load dmx values int ws2811 
					 
					 mov first8,dmx1
					 mov second8,dmx2
					 mov third8,dmx3
           ;call ws2811
           mov rgb1,dmx1
				   mov rgb2,dmx2
				   mov rgb3,dmx3
				   rcf
					 ld A,dmx1
					 cp A, #150
					 jrnc pc3on
					 bres PC_ODR,#3
           jp dmx1done
pc3on			 bset PC_ODR,#3	
dmx1done			
					 
					 rcf
					 ld A,dmx2
					 cp A, #150
					 jrnc pc7on
					 bres PC_ODR,#7
           jp dmx2done
pc7on			 bset PC_ODR,#7	
dmx2done					 
					 rcf
					 ld A,dmx3
					 cp A, #150
					 jrnc pc5on
					 bres PC_ODR,#5
           jp dpulse
pc5on			 bset PC_ODR,#5	 
dpulse		 bres d_pulse
					 call rgbled
testswitch				

;******************************  SEND OUT OF UART 115200 WITH 255 MArker and 0 -128 dmx value
markerbit   btjf UART1_SR,#7,markerbit ;ok to fill uart?
            ld A,#255
            ld UART1_DR,A
checkfrst		btjf UART1_SR,#7,checkfrst				
						ld A,dmx1
						srl A
            ld UART1_DR,A						
waittx1     btjf UART1_SR,#7,waittx1 ; check clear to put data in
            ld A,dmx2
						srl A
						ld UART1_DR,A
waittx2			btjf UART1_SR,#7,waittx2
            ld A,dmx3
						srl A
						ld UART1_DR,A

; load up TIM2 registers  with val * 4
;           
            ld A,dmx1
						ldw X,#4
						mul X,A 
						ld A,XH
						ld TIM2_CCR1H,A
						ld A,XL
						ld TIM2_CCR1L,A
            ld A,dmx2
						ldw X,#4
						mul X,A 
						ld A,XH
						ld TIM2_CCR2H,A
						ld A,XL
						ld TIM2_CCR2L,A
            ld A,dmx3
						ldw X,#4
						mul X,A 
						ld A,XH
						ld TIM2_CCR3H,A
						ld A,XL
						ld TIM2_CCR3L,A


									
									jp Sstartag
manual_start      jp maindmx          
                

			



loopit

			jp loopit
			






		
;***********************************DMX1BIT**************
dmx1bit

; a median value of the last 5 samples will now be made.
;first shift the last four samples back and enter the latest value
;then call median1. answer will be in med1

;dmx1 stored values are in variable1,2,3,4,5
;shift 5 4 3 2 into 4 3 2 1

											
                        clr A
		btjf dmxval1,#7,sit1a
		add A,#1
sit1a		btjf dmxval1,#6,sit2a
		add A,#2
sit2a		btjf dmxval1,#5,sit3a
		add A,#4				
sit3a		btjf dmxval1,#4,sit4a
		add A,#8
sit4a		btjf dmxval1,#3,sit5a				
		add A,#16
sit5a		btjf dmxval1,#2,sit6a
		add A,#32
sit6a		btjf dmxval1,#1,sit7a
		add A,#64
sit7a		btjf dmxval1,#0,sit8a
		add A,#128						
sit8a		
		ld dmx1,A
    ; now controls PB,#4 open drain output if above 153,
    bres PB_ODR,#4
		rcf
		cp A,#150
    jrc noset		
		bset PB_ODR,#4
noset		
					
					
					
					ret




;***************************************DMX2BIT*********************************

.dmx2bit											

                    		clr A
		btjf dmxval2,#7,sit1h
		add A,#1
sit1h		btjf dmxval2,#6,sit2h
		add A,#2
sit2h		btjf dmxval2,#5,sit3h
		add A,#4				
sit3h		btjf dmxval2,#4,sit4h
		add A,#8
sit4h		btjf dmxval2,#3,sit5h				
		add A,#16
sit5h		btjf dmxval2,#2,sit6h
		add A,#32
sit6h		btjf dmxval2,#1,sit7h
		add A,#64
sit7h		btjf dmxval2,#0,sit8h
		add A,#128						
sit8h		
		
		ld dmx2,A
		ldw X,#250          
		ldw dmx2val,X
		ld A,dmx2
    ld XL,A
		addw X,dmx2val;$88
		ld A,XH
		ld TIM1_CCR4H,A
		ld A,XL
		ld TIM1_CCR4L,A
		bset TIM1_BKR,#7	
                ret

;***************************** DMX3BIT********************************

dmx3bit

                clr A
		btjf dmxval3,#7,sit1i
		add A,#1
sit1i		btjf dmxval3,#6,sit2i
		add A,#2
sit2i		btjf dmxval3,#5,sit3i
		add A,#4				
sit3i		btjf dmxval3,#4,sit4i
		add A,#8
sit4i		btjf dmxval3,#3,sit5i				
		add A,#16
sit5i		btjf dmxval3,#2,sit6i
		add A,#32
sit6i		btjf dmxval3,#1,sit7i
		add A,#64
sit7i		btjf dmxval3,#0,sit8i
		add A,#128						
sit8i		
		
		ld dmx3,A
		            
                
                 ret    





;********************************  DMXFILL ***************************************************	
; since a SWAP A (nibble swap) has been added, need to pull out 4 machine cycles


dmxfill	
				ld A,#2 ; was 3 but trying to compensate for SWAP inclusion.
dmxdec	dec A
				jrne dmxdec
				nop
				nop
			;	nop
				nop
				;nop
				
				ret







delflash
									ldw Y,#20
									ldw X,#255
delboy2       	  ld A,#255
delboy       	  	 dec A
									jrne delboy
									decw X
									jrne delboy2
									decw Y
									jrne delboy2
									ret

clearam

					ldw X,#rub1  ;address of variable start
more2clr	clr (X)
					incw X
					cpw X,#memend
					JRULE more2clr
					
					ret

;********************************** ep2ram **************************************

ep2ram                  ;copy stored values to ram for program use.


						ld A,dmxl_ep
						ld dmxl_ram,A
						ld A,dmxh_ep
						ld dmxh_ram,A
							
						ret







eeprom




islockedmain		;btjt FLASH_IAPSR,#1,islocked  ;check MASS
								;mov FLASH_PUKR,#$56
								;mov FLASH_PUKR,#$AE   ;load in unlock code

islocked		   btjt FLASH_IAPSR,#3,notlocked  ;check MASS
								mov FLASH_DUKR,#$AE
								mov FLASH_DUKR,#$56   ;load in unlock code
								
notlocked 			btjt FLASH_IAPSR,#2,inuse ; check that is free to program
								
								mov dmxl_ep,dmxl_ram  ; should just go and do it, takes 3mS or so
inuse								
								btjt FLASH_IAPSR,#2,inuse2
								
								mov dmxh_ep,dmxh_ram
inuse2								
								
								ldw X,#2000
epdel								decw X
								jrne epdel

								
relock				;	mov FLASH_PUKR,#$50  ; program arear protection
							;	mov FLASH_PUKR,#$50

								bset prog_d,#7
									
									ret

























set8     ; 24 data bit for ws rgb led
	
		bset led
		nop
		nop
		nop
		nop
		nop
		bres led
		nop
		nop
		nop
		bset led
		nop
		nop
		nop
		nop
		nop
		bres led
		nop
		nop
		nop
		bset led
		nop
		nop
		nop
		nop
		nop
		bres led
		nop
		nop
		nop
		bset led
		nop
		nop
		nop
		nop
		nop
		bres led
		nop
		nop
		nop
		bset led
		nop
		nop
		nop
		nop
		nop
		bres led
		nop
		nop
		nop
		bset led
		nop
		nop
		nop
		nop
		nop
		bres led
		nop
		nop
		nop
		bset led
		nop
		nop
		nop
		nop
		nop
		bres led
		nop
		nop
		nop
		bset led
		nop
		nop
		nop
		nop
		nop
		bres led
		nop
		nop
		nop
		
  ret

clear
    bset led
		nop
		nop
		nop
    bres led
		nop
		nop
		nop
		nop
		nop
		bset led
		nop
		nop
		nop
    bres led
		nop
		nop
		nop
		nop
		nop
		bset led
		nop
		nop
		nop
    bres led
		nop
		nop
		nop
		nop
		nop
		bset led
		nop
		nop
		nop
    bres led
		nop
		nop
		nop
		nop
		nop
		bset led
		nop
		nop
		nop
    bres led
		nop
		nop
		nop
		nop
		nop
		bset led
		nop
		nop
		nop
    bres led
		nop
		nop
		nop
		nop
		nop
		bset led
		nop
		nop
		nop
    bres led
		nop
		nop
		nop
		nop
		nop
		bset led
		nop
		nop
		nop
    bres led
		nop
		nop
		nop
		nop
		nop
		
	ret	
		
		
		
		
portsetup
;The PA2, PB0, PB1, PB2, PB3, PB6, PB7, PC1, PC2, PC7, PD0, PD2, PD4, PD7, PE5 and PF4 GPIOs should be configured after ;device reset in output push-pull mode with output low-state to reduce the device’s consumption and to improve its EMC ;immunity	
;    DDR	CR1	CR2
		;	0		0		0			floating no int
		;	0		1		0			pull up no int
		;	0		0		1			floating with int
		;	0		1		1			pull up with int
		
		;	1		0		0			open drain output
		;	1		1		0			push pull output
		;	1		0		1			open drain fast output
		;	1		1		1			push pull fast output
		; 1		x		x			true open drain on specific pins on chip

	 ; port c6 in pullup for DMX input.
	ld A,#%10111110;  port C 
	ld PC_DDR,A ;     
	ld A,#%11111110;  
	ld PC_CR1,A
	ld A,#0
	ld PC_ODR,A
	
	
	ld A,#%11011111; 
	ld PB_DDR,A ;    PB4 is open drain output. 
	ld A,#%11101111; 
	ld PB_CR1,A
	ld A,#%00000000
	ld PD_ODR,A
	;set PA1 as out pp and use it for the ws2812 drive so that PD6 can be used as Uart.
	;PA2 is now used for sync pulses out
	;setting for output #3 to sort timing
	ld A,#%00000110; PA3 is now pullup interrupt For Switch......was ...PA3 is pp output for led pin 5
	ld PA_DDR,A ;    
	ld A,#%00001110; 
	ld PA_CR1,A
	ld A,#%00000000; sets as pullup up.
	ld PA_CR2,A
	ld A,#%00000000
	ld PA_ODR,A
	
waitforOne
	ld A,#255
	btjf PA_IDR,#3,waitforOne  ; check input high before setting for interrupt
	ld A,#%00001000; sets as pullup up.interrupt
	ld PA_CR2,A
	
	ld A,#%11010101; PD6 pin1 for ws2812 grb
	ld PD_DDR,A ;    
	ld A,#%11010111; 
	ld PD_CR1,A
	ld A,#%00000000
	ld PD_ODR,A
	ld PD_CR2,A
		
	;******** not in 003 chip **********************
	;ld A,#%00100000;   e5 as marker output
	;ld PE_DDR,A ;
	;ld A,#%00100000;   
	;ld PE_CR1,A
	
	;ld A,#%00000000;08
	;ld PG_DDR,A ;
	;ld A,#%11111111;   input pullups 
	;ld PG_CR1,A
	
	 ;ld A,#%00010000;08    port F only F4
	 ;ld PF_DDR,A ;
	 ;ld A,#%00010000;   input pullups 
	 ;ld PF_CR1,A
	 
	 ;UART1 setup
	 
	 ld A,#$0B
	 ld UART1_BRR2,A  ;set baud rate 115200
	 ld A,#08
	 ld UART1_BRR1,A
	 bset UART1_CR2,#3
	 bset UART1_CR2,#2   ; tx and Rx enable?
	 
	 
	 
	 
	 ret
timerset     ; channel 1 on PC6 pin 8, channel 4 on pin7
	; going to get a 50Hz pulse for servos going, 50 Hz= 20mS
	;need a 1.5mS pulse to centre servo then 1mS to 2mS for a 0 - 255 data.
	;prescale to 400Khz, so divide by 7095 loaded into auto reload will arrive at 50Hz
	;frequency.
	
	
	ld A,#$68
;	ld TIM1_CCMR1,A	; output compare 1 preload reg.pwm mode.channel is output	
	ld TIM1_CCMR4,A ; SAME AS 1 for each of up to 4 channels. upper '6' gives PWM mode 1, lower '8' enables preload register.
	

	ld A,#$00;   =   prescaler for timer. clock at 8Mhz in this ie 0.125uS
	ld TIM1_PSCRH,A		;at 65535 max count time = 8ms  = ok.
	ld A,#64; 32
	ld TIM1_PSCRL,A   ; at 16Mhz gives 4uS clock
	;need to enable output compare to pins
	ld A,#%00010000; enable channel 4
	
	ld TIM1_CCER2,A
  ; with prescale 63, count = 126.98khz  divide by 0x1EE1(7095) gives 20ms
	;ld A,#%00010000	   ; channel 4 output enable
	;ld TIM1_CCER2,A 
	;ld A,#$1E  ; 1. mS PWM count needs to be 8064 (so 255 haze/fan max times 31 gives max pwm)
	ld A,#$13           ;ARRH sets period of pwm, needs to be 20mS ie 5000, 5000 X 4uS 
	ld TIM1_ARRH,A
	;ld A,#$E1					;
	ld A,#$88
	ld TIM1_ARRL,A
	clr TIM2_IER			;no interrupts
	; 4uS resolution now
	; for 1ms, basic pulse need a count of 250,
	; then add 0 - 255 for pulse of 1mS to 2mS for servo.
	; pulse is set by CCRi registers
	;need to load output compare registers with data.
	;max count = 8192.
	;frequency here is 500uS 
	;output compares need to start of at 1.5mS so count is 191
	LD A,#$00;0f
 ; ld TIM1_CCR1H,A
	LD TIM1_CCR4H,A
	ld A,#250        ; should be a 1mS pulse every 20mS.
	;ld TIM1_CCR1L,A
	LD TIM1_CCR4L,A
	bset TIM1_EGR,#1
	ld A,#$01
	bset TIM1_BKR,#7; Main output enable, must be set
	ld TIM1_CR1,A   ;enable timer
	
	;  ret
	
;.timer2	
	; ok going to set up TIM2.
	ld A,#$68
	ld TIM2_CCMR1,A     ; output compare preload. pwm mode
	ld TIM2_CCMR2,A
	ld TIM2_CCMR3,A
	ld A,#4          ; f = clock / 2 ^ (PSCR(3:0))
	ld TIM2_PSCR,A     ;prescale  should give 1uS tick
	
	ld A,#$11
	ld TIM2_CCER1,A   ;  enable channel bit 1 and 4 enable chan 1 and chan 2
	ld A,#$01
	ld TIM2_CCER2,A   ;enable chan3 output
 	ld A,#$03
	ld TIM2_ARRH,A	; auto reload ie the period. 0x3E8 (1000) should give 1Khz.
	ld A,#$E8
	ld TIM2_ARRL,A
;	
	clr TIM2_IER  ; no ints
;	
	; for test load up with 500 which should be 50 50 square wave
	ld A,#$00 ;  01     ; load 1F4  
	ld TIM2_CCR1H,A
	ld TIM2_CCR2H,A
	ld TIM2_CCR3H,A
	ld A,#$00; F4
	ld TIM2_CCR1L,A
	ld TIM2_CCR2L,A
	ld TIM2_CCR3L,A
	
	bset TIM2_EGR,#1 ; update generartion
	
	;TIM2_BKR  TIM2 does not  have this register;
	ld A,#1
	ld TIM2_CR1,A     ; enable timer
	
	
	
	
	
	   ret
; *************** routine for filling 24bits out to the ws2811 chip

.ws2811	
        ld A,#8
loopit1	RCF   		;reset carry flag
				SLL first8	
	      jrnc zeropulse    ; if a carry then this is a 1 bit sequence else a 0 sequence
				call highseq
        jp donit
zeropulse				
        call lowseq
donit				
	      dec A
				jrne loopit1
	     ld A,#8
loopit2	RCF   		;reset carry flag
				SLL second8	
	      jrnc zeropulse2    ; if a carry then this is a 1 bit sequence else a 0 sequence
				call highseq
        jp donit2
zeropulse2				
        call lowseq
donit2				
	      dec A
				jrne loopit2
        ld A,#8
loopit3	RCF   		;reset carry flag
				SLL third8	
	      jrnc zeropulse3    ; if a carry then this is a 1 bit sequence else a 0 sequence
				call highseq
        jp donit3
zeropulse3				
        call lowseq
donit3				
	      dec A
				jrne loopit3
				ret
	
.highseq
    bset WS
		nop
		nop
		nop
		nop
		bres WS
		;nop
		;nop
		;nop
       ret
			 
.lowseq
    bset WS
		;nop
		nop
		nop
    bres WS
		;nop
		;nop
		;nop
		;nop
		nop
			ret

;****************************** reset RGB and Uart out
.clr_rgb_uart

        mov rgb1,#0
				mov rgb2,#0
				mov rgb3,#0
				call rgbled
				
markerbitb   btjf UART1_SR,#7,markerbitb ;ok to fill uart?
            ld A,#255
            ld UART1_DR,A
checkfrstb		btjf UART1_SR,#7,checkfrstb				
						ld A,#0
						ld UART1_DR,A						
waittx1b    btjf UART1_SR,#7,waittx1b ; check clear to put data in
            ld A,#0
						ld UART1_DR,A
waittx2b		btjf UART1_SR,#7,waittx2b
            ld A,#0
						ld UART1_DR,A
   ret						







;*************** DISPLAY DMX .. blinks huns, tens, units.. 0 is long blink.

.display_dmx	
				; get dmx channel value. high byte, low byte.
				; then convert into huns tens and units.
				;ld A,dmxh_ram
			 ; EXG A,XH
				;ld XH,A
	
			  call WD
				ld A,dmxh_ram
        EXG A,XL
				SWAPW X
				ld A,dmxl_ram
				EXG A,XL
				
				;ld XL,A    ;X contains word length of dmx channel.
				ldw Y,#100 ;divider
				divw X,Y
        ld A,XL
				ld blinkhun,A ;number of hundreds
				ldw X,Y   ; X has remainder of value
				ldw Y,#10
				divw X,Y ;how many 10's,Y holds units
				ld A,XL
				ld blinkten,A
				ld A,YL
				ld blinkunit,A
        ; need to blink first8 blinkten times with others clear
				; need to blink secoond8 blink ten times with first and third cleared
				;need to blink third8 blink unit times with first and second cleared
				; blank blink is all three cleared.
				ld A,blinkhun
				jrne do_blinkh
				mov rgb1,#255
				mov rgb2,#0
				mov rgb3,#0
				;call set8
				;call clear
				;call clear
				call rgbled
				call WD
				call longdel
				mov rgb1,#0
				mov rgb2,#0
				mov rgb3,#0
				call rgbled
				;call clear
				;call clear
				;call clear
				jp do_tens
do_blinkh
hun_again  
        mov rgb1,#255
				mov rgb2,#0
				mov rgb3,#0
				call rgbled
				;call set8
				;call clear
				;call clear
				call WD
				call blinkdelay
				mov rgb1,#0
				mov rgb2,#0
				mov rgb3,#0
				call rgbled
				;call clear
				;call clear
				;call clear
				call WD
				call blinkdelay
				dec A
				jrne hun_again
do_tens
				; need to blink first8 blinkten times with others clear
				; need to blink secoond8 blink ten times with first and third cleared
				;need to blink third8 blink unit times with first and second cleared
				; blank blink is all three cleared.
				call WD
				call longdel
				ld A,blinkten
				jrne do_blinkt
				mov rgb1,#0
				mov rgb2,#255
				mov rgb3,#0
				call rgbled
				;call clear
				;call set8
				;call clear
				call WD
				call longdel
				mov rgb1,#0
				mov rgb2,#0
				mov rgb3,#0
				call rgbled
				;call clear
				;call clear
				;call clear
				jp do_units
do_blinkt
ten_again
        mov rgb1,#0
				mov rgb2,#255
				mov rgb3,#0
				call rgbled
				;call clear
				;call set8
				;call clear
				call WD
				call blinkdelay
				mov rgb1,#0
				mov rgb2,#0
				mov rgb3,#0
				call rgbled
				;call clear
				;call clear
				;call clear
				call WD
				call blinkdelay
				dec A
				jrne ten_again
do_units
        call WD
				call longdel
				ld A,blinkunit
				jrne do_blinku
				mov rgb1,#0
				mov rgb2,#0
				mov rgb3,#255
				call rgbled
				;call clear
				;call clear
				;call set8
				call WD
				call longdel
				mov rgb1,#0
				mov rgb2,#0
				mov rgb3,#0
				call rgbled
				;call clear
				;call clear
				;call clear
				jp dunblinks
do_blinku
unit_again
        mov rgb1,#0
				mov rgb2,#0
				mov rgb3,#255
				call rgbled
				;call clear
				;call clear
				;call set8
				call WD
				call blinkdelay
				mov rgb1,#0
				mov rgb2,#0
				mov rgb3,#0
				call rgbled
				;call clear
				;call clear
				;call clear
				call WD
				call blinkdelay
				dec A
				jrne unit_again
dunblinks				
        call WD
				call longdel 
				 ret
				 
longdel
   			 ldw X,#40
fillY1	 ldw Y,#$ffff
dekY1	 decw Y
			 jrne dekY1
			 call WD
			 decw X
			 jrne fillY1
   ret
blinkdelay
       
			 ldw X,#10
fillY	 ldw Y,#$ffff
dekY	 decw Y
			 jrne dekY
			 decw X
			 jrne fillY
	 
	 ret
; routine for ws2812 rgb led.
; In order to reduce running current, put in brightness control for rgb leds
; use 0 to 100 for percentage. so value X percentage / 100.
 ; use mul  ie X x A, where low byte put in X,
 ; then use div X,A where A X is divided by 100 to get answer in XL
;'rgbled' uses rgb1,rgb2,and rgb3 variables for green red blue.
rgbled
  push A
	ld A,bright
	ld XL,A
	ld A,rgb1
	mul X,A
	ld A,#100
	div X,A
	ld A,XL
	ld rgb1,A
	ld A,bright
	ld XL,A
	ld A,rgb2
	mul X,A
	ld A,#100
	div X,A
	ld A,XL
	ld rgb2,A
	ld A,bright
	ld XL,A
	ld A,rgb3
	mul X,A
	ld A,#100
	div X,A
	ld A,XL
	ld rgb3,A
	ld A,#8
loopitled
  SLL rgb1
	jrnc ledpulsezero
	call ledhigh
	jp dun
ledpulsezero
  call ledzero
dun	
  dec A
	jrne loopitled
	 ld A,#8
loopitled1
  SLL rgb2
	jrnc ledpulsezero1
	call ledhigh
	jp dun1
ledpulsezero1
  call ledzero
dun1	
  dec A
	jrne loopitled1
  ld A,#8
loopitled2
  SLL rgb3
	jrnc ledpulsezero2
	call ledhigh
	jp dun2
ledpulsezero2
  call ledzero
dun2	
  dec A
	jrne loopitled2
  pop A
	ret

;*****************************************
ledhigh
 bset led
 nop
 nop
 nop
 nop
 bres led
 ret
ledzero
 bset led
 nop
 nop
 bres led
 ret
.WD
           mov IWDG_KR,#$CC ; enable watchdog   for dmx timeout
					 mov IWDG_KR,#$55
					 mov IWDG_PR,#6 ;prescale /256
					 mov IWDG_RLR,#$FF
					 mov IWDG_KR,#$AA  
	ret				 
	
	#ifdef RAM0	
	; clear RAM0
ram0_start.b EQU $ram0_segment_start
ram0_end.b EQU $ram0_segment_end
	ldw X,#ram0_start
clear_ram0.l
	clr (X)
	incw X
	cpw X,#ram0_end	
	jrule clear_ram0
	#endif

	#ifdef RAM1
	; clear RAM1
ram1_start.w EQU $ram1_segment_start
ram1_end.w EQU $ram1_segment_end	
	ldw X,#ram1_start
clear_ram1.l
	clr (X)
	incw X
	cpw X,#ram1_end	
	jrule clear_ram1
	#endif

	; clear stack
stack_start.w EQU $stack_segment_start
stack_end.w EQU $stack_segment_end
	ldw X,#stack_start
clear_stack.l
	clr (X)
	incw X
	cpw X,#stack_end	
	jrule clear_stack

infinite_loop.l
	jra infinite_loop

	interrupt SetDMXaddress
SetDMXaddress.l
  SIM
	
	
	;wait for port to go high again
	call WD   ;watchdog refresh
  ; take PA3 out of interrupt mode to prevent flags being set
	;ld A,#%00000000; PA3 is now pullup interrupt .....PA3 is pp output for led pin 5
	;ld PA_DDR,A ;    
	ld A,#%00001110; 
	ld PA_CR1,A

	ld A,#%00000000; sets as pullup up no int
	ld PA_CR2,A


  ld A,#100
pause dec A
	jrne pause
pinlow	
  call display_dmx  ;display dmx whilst pin is low
  btjf PA_IDR,#3,pinlow
; now need to count pin lows for 1 second.....
;and flash huns the same number of times.
; 
  ;flash green for short while
        mov rgb1,#255
				mov rgb2,#0
				mov rgb3,#0
				call rgbled
			;call set8
			;call clear
			;call clear
			call WD
			call blinkdelay  ;green flash
			  mov rgb1,#0
				mov rgb2,#0
				mov rgb3,#0
				call rgbled
			;call clear
			;call clear
			;call clear
	
	
	
	clr buthun
	clr butten
	clr butunit
	;ld A,butflag
	;jreq do_huns
	;jp count_tens
do_huns	
        ldw X,#$5555;ffff
loadhun	ldw Y,#60
lookhun	btjt PA_IDR,#3,nocount
				inc buthun
waithun		
				btjf PA_IDR,#3,waithun
nocount
				call WD
				decw Y
				jrne lookhun
				call WD
				decw X
				jrne loadhun
;need to display huns now.
; clear eeprom values, reload and call display_dmx
	  ld A,buthun
		cp A,#6
		jrc hungood
		mov buthun,#5
hungood
    ld A,buthun
		ld XL,A
		ld A,#100
		mul X,A ; x contains answer
		ld A,XL
		ld dmxl_ram,A
		ld A,XH
		ld dmxh_ram,A
		call WD
		call eeprom	
		inc butflag
	;	jp disp_it
count_tens
  
	      mov rgb1,#0
				mov rgb2,#255
				mov rgb3,#0
				call rgbled
			;call clear
			;call set8      ;red for tens
			;call clear
			call WD
			call blinkdelay  ;green flash
			mov rgb1,#0
			mov rgb2,#0
			mov rgb3,#0
			call rgbled
			;call clear
			;call clear
			;call clear 
	
	
	
	;ld A,butflag
	;cp A,#1
	;jreq godoten
	;jp godounits
godoten	
	ldw X,#$5555;ffff
loadten	ldw Y,#60
lookten	btjt PA_IDR,#3,nocountten
				inc butten
waitten		
				btjf PA_IDR,#3,waitten
nocountten
				call WD
				decw Y
				jrne lookten
				call WD
				decw X
				jrne loadten
;need to display huns now.
; clear eeprom values, reload and call display_dmx
	  ld A,butten
		cp A,#10
		jrc tengood
		mov buthun,#9
tengood
    ld A,butten
		ld XL,A
		ld A,#10
		mul X,A ; x contains answer 0 - 90
		;now need to get 100's value and add number of tens to it and store again
		ld A,dmxl_ep
		ld YL,A
		ld A,dmxh_ep
		ld YH,A         ;y holds present dmx address
		
		ld A,XL
    ld $86,A  ;tens value
		ldw X,tenval
		ldw tenval,X
		Addw Y,tenval        ; new address val
		ld A,YL
		ld dmxl_ram,A
		ld A,YH
		ld dmxh_ram,A
		call WD
		call eeprom	
		inc butflag
		;jp disp_it
godounits                 ; count and add units	
	    mov rgb1,#0
				mov rgb2,#0
				mov rgb3,#255
				call rgbled
      ;call clear
			;call clear
			;call set8
			call WD
			call blinkdelay  ;green flash
			mov rgb1,#0
			mov rgb2,#0
			mov rgb3,#0
			call rgbled
			;call clear
			;call clear
			;call clear		
		
		
		ldw X,#$5555;ffff
loadunits	ldw Y,#60
lookunits	btjt PA_IDR,#3,nocountunits
				inc butunit
waitunit		
				btjf PA_IDR,#3,waitunit
nocountunits
				call WD
				decw Y
				jrne lookunits
				call WD
				decw X
				jrne loadunits
;need to display huns now.
; clear eeprom values, reload and call display_dmx
	  ld A,butunit
		cp A,#10
		jrc unitgood
		mov butunit,#9
unitgood
    ld A,butunit
		ld XL,A
		ld A,#1
		mul X,A ; x contains answer 0 - 90
		;now need to get 100's value and add number of tens to it and store again
		ld A,dmxl_ep
		ld YL,A
		ld A,dmxh_ep
		ld YH,A         ;y holds present dmx address
		ld A,XL
    ld $86,A  ;units value
		ldw X,tenval   ;units into tenval variable
		ldw tenval,X
		Addw Y,tenval        ; new address val
		;check that Y is not greater than 508, if so set it at 508
		cpw Y,#509
		jrc add_good
    ldw Y,#508
    
add_good
		cpw Y,#1
		jrnc addnotzero
		ldw Y,#1
addnotzero		
		ld A,YL
		ld dmxl_ram,A
		ld A,YH
		ld dmxh_ram,A
		call WD
		call eeprom	
		mov butflag,#0
		jp disp_it
	
	
disp_it
	
	call display_dmx
	ld A,dmxl_ram
  ld bin1,A
  ld A,dmxh_ram
  ld bin2,A
	ld A,#%00001000; sets as pullup up  int
	ld PA_CR2,A
	
	rim
	iret
	;jp reset
	interrupt NonHandledInterrupt
NonHandledInterrupt.l
	iret

	segment 'vectit'
	dc.l {$82000000+main}									; reset
	dc.l {$82000000+NonHandledInterrupt}	; trap
	dc.l {$82000000+NonHandledInterrupt}	; irq0
	dc.l {$82000000+NonHandledInterrupt}	; irq1
	dc.l {$82000000+NonHandledInterrupt}	; irq2
	dc.l {$82000000+SetDMXaddress}	      ; irq3 external io port A
	dc.l {$82000000+NonHandledInterrupt}	; irq4
	dc.l {$82000000+NonHandledInterrupt}	; irq5
	dc.l {$82000000+NonHandledInterrupt}	; irq6
	dc.l {$82000000+NonHandledInterrupt}	; irq7
	dc.l {$82000000+NonHandledInterrupt}	; irq8
	dc.l {$82000000+NonHandledInterrupt}	; irq9
	dc.l {$82000000+NonHandledInterrupt}	; irq10
	dc.l {$82000000+NonHandledInterrupt}	; irq11
	dc.l {$82000000+NonHandledInterrupt}	; irq12
	dc.l {$82000000+NonHandledInterrupt}	; irq13
	dc.l {$82000000+NonHandledInterrupt}	; irq14
	dc.l {$82000000+NonHandledInterrupt}	; irq15
	dc.l {$82000000+NonHandledInterrupt}	; irq16
	dc.l {$82000000+NonHandledInterrupt}	; irq17
	dc.l {$82000000+NonHandledInterrupt}	; irq18
	dc.l {$82000000+NonHandledInterrupt}	; irq19
	dc.l {$82000000+NonHandledInterrupt}	; irq20
	dc.l {$82000000+NonHandledInterrupt}	; irq21
	dc.l {$82000000+NonHandledInterrupt}	; irq22
	dc.l {$82000000+NonHandledInterrupt}	; irq23
	dc.l {$82000000+NonHandledInterrupt}	; irq24
	dc.l {$82000000+NonHandledInterrupt}	; irq25
	dc.l {$82000000+NonHandledInterrupt}	; irq26
	dc.l {$82000000+NonHandledInterrupt}	; irq27
	dc.l {$82000000+NonHandledInterrupt}	; irq28
	dc.l {$82000000+NonHandledInterrupt}	; irq29

	end
