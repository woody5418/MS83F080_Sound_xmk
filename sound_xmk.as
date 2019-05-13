opt subtitle "HI-TECH Software Omniscient Code Generator (Lite mode) build 10920"

opt pagewidth 120

	opt lm

	processor	16F685
clrc	macro
	bcf	3,0
	endm
clrz	macro
	bcf	3,2
	endm
setc	macro
	bsf	3,0
	endm
setz	macro
	bsf	3,2
	endm
skipc	macro
	btfss	3,0
	endm
skipz	macro
	btfss	3,2
	endm
skipnc	macro
	btfsc	3,0
	endm
skipnz	macro
	btfsc	3,2
	endm
indf	equ	0
indf0	equ	0
pc	equ	2
pcl	equ	2
status	equ	3
fsr	equ	4
fsr0	equ	4
c	equ	1
z	equ	0
pclath	equ	10
	FNCALL	_main,_System_init
	FNCALL	_main,_GPIO_Init
	FNCALL	_main,_ADC_Init
	FNCALL	_main,_EEPROM_Init
	FNCALL	_main,_Read_EEPROM_u16
	FNCALL	_main,_LED_Study_End
	FNCALL	_main,_ExchChannel
	FNCALL	_main,_GetADCValue
	FNCALL	_main,_isKeyPressed
	FNCALL	_main,_Delay_xms
	FNCALL	_main,_Write_EEPROM_u16
	FNCALL	_LED_Study_End,_Delay_xms
	FNCALL	_ADC_Init,_Delay_xms
	FNCALL	_Read_EEPROM_u16,_Read_EEPROM_u8
	FNCALL	_Write_EEPROM_u16,_Write_EEPROM_u8
	FNCALL	_EEPROM_Init,_Write_EEPROM_u8
	FNROOT	_main
	FNCALL	intlevel1,_ISR
	global	intlevel1
	FNROOT	intlevel1
	global	_adc_Study
	global	_adc_input
	global	_adc_value
	global	_times
	global	_volume1
	global	_flag_sc
	global	_PSTRCON
psect	text371,local,class=CODE,delta=2
global __ptext371
__ptext371:
_PSTRCON	set	413
	DABS	1,413,1	;_PSTRCON

	global	_SRCON
_SRCON	set	414
	DABS	1,414,1	;_SRCON

	global	_C1SEN
_C1SEN	set	3317
	DABS	1,414,1	;_C1SEN

	global	_C2REN
_C2REN	set	3316
	DABS	1,414,1	;_C2REN

	global	_EEPGD
_EEPGD	set	3175
	DABS	1,396,1	;_EEPGD

	global	_PULSR
_PULSR	set	3314
	DABS	1,414,1	;_PULSR

	global	_PULSS
_PULSS	set	3315
	DABS	1,414,1	;_PULSS

	global	_SR0
_SR0	set	3318
	DABS	1,414,1	;_SR0

	global	_SR1
_SR1	set	3319
	DABS	1,414,1	;_SR1

	global	_STRA
_STRA	set	3304
	DABS	1,413,1	;_STRA

	global	_STRB
_STRB	set	3305
	DABS	1,413,1	;_STRB

	global	_STRC
_STRC	set	3306
	DABS	1,413,1	;_STRC

	global	_STRD
_STRD	set	3307
	DABS	1,413,1	;_STRD

	global	_STRSYNC
_STRSYNC	set	3308
	DABS	1,413,1	;_STRSYNC

	global	_WREN
_WREN	set	3170
	DABS	1,396,1	;_WREN

	global	_ADCON0
_ADCON0	set	31
	global	_ADRESH
_ADRESH	set	30
	global	_TMR2
_TMR2	set	17
	global	_WDTCON
_WDTCON	set	24
	global	_ADFM
_ADFM	set	255
	global	_ADON
_ADON	set	248
	global	_CARRY
_CARRY	set	24
	global	_CHS0
_CHS0	set	250
	global	_CHS1
_CHS1	set	251
	global	_CHS2
_CHS2	set	252
	global	_CM0
_CM0	set	200
	global	_CM1
_CM1	set	201
	global	_CM2
_CM2	set	202
	global	_EEIF
_EEIF	set	103
	global	_GIE
_GIE	set	95
	global	_GO_DONE
_GO_DONE	set	249
	global	_PA6
_PA6	set	46
	global	_PC2
_PC2	set	58
	global	_PC3
_PC3	set	59
	global	_PEIE
_PEIE	set	94
	global	_T2CKPS0
_T2CKPS0	set	144
	global	_T2CKPS1
_T2CKPS1	set	145
	global	_TMR2IF
_TMR2IF	set	97
	global	_TMR2ON
_TMR2ON	set	146
	global	_TOUTPS0
_TOUTPS0	set	147
	global	_TOUTPS1
_TOUTPS1	set	148
	global	_TOUTPS2
_TOUTPS2	set	149
	global	_TOUTPS3
_TOUTPS3	set	150
	global	_VCFG0
_VCFG0	set	253
	global	_ADRESL
_ADRESL	set	158
	global	_EEADR
_EEADR	set	155
	global	_EECON1
_EECON1	set	156
	global	_EECON2
_EECON2	set	157
	global	_EEDAT
_EEDAT	set	154
	global	_EEDATA
_EEDATA	set	154
	global	_OPTION
_OPTION	set	129
	global	_OSCCON
_OSCCON	set	143
	global	_PR2
_PR2	set	146
	global	_ADCS0
_ADCS0	set	1276
	global	_ADCS1
_ADCS1	set	1277
	global	_ADCS2
_ADCS2	set	1278
	global	_ANSEL0
_ANSEL0	set	1160
	global	_ANSEL1
_ANSEL1	set	1161
	global	_ANSEL2
_ANSEL2	set	1162
	global	_ANSEL3
_ANSEL3	set	1163
	global	_ANSEL4
_ANSEL4	set	1164
	global	_ANSEL5
_ANSEL5	set	1165
	global	_ANSEL6
_ANSEL6	set	1166
	global	_ANSEL7
_ANSEL7	set	1167
	global	_DIVS
_DIVS	set	1279
	global	_RD
_RD	set	1248
	global	_TMR2IE
_TMR2IE	set	1121
	global	_TRISA0
_TRISA0	set	1064
	global	_TRISA1
_TRISA1	set	1065
	global	_TRISA6
_TRISA6	set	1070
	global	_TRISC1
_TRISC1	set	1081
	global	_TRISC2
_TRISC2	set	1082
	global	_TRISC3
_TRISC3	set	1083
	global	_WPDA4
_WPDA4	set	1100
	global	_WPDC1
_WPDC1	set	1099
	global	_WPDC2
_WPDC2	set	1098
	global	_WPDC3
_WPDC3	set	1097
	global	_WPUA0
_WPUA0	set	1192
	global	_WPUA1
_WPUA1	set	1193
	global	_WPUA6
_WPUA6	set	1198
	global	_WPUC1
_WPUC1	set	1089
	global	_WPUC2
_WPUC2	set	1090
	global	_WPUC3
_WPUC3	set	1091
	global	_WR
_WR	set	1256
	file	"sound_xmk.as"
	line	#
psect cinit,class=CODE,delta=2
global start_initialization
start_initialization:

psect	bssBANK0,class=BANK0,space=1
global __pbssBANK0
__pbssBANK0:
_adc_Study:
       ds      2

_adc_input:
       ds      2

_adc_value:
       ds      2

_times:
       ds      2

_volume1:
       ds      2

_flag_sc:
       ds      1

; Clear objects allocated to BANK0
psect cinit,class=CODE,delta=2
	clrf	((__pbssBANK0)+0)&07Fh
	clrf	((__pbssBANK0)+1)&07Fh
	clrf	((__pbssBANK0)+2)&07Fh
	clrf	((__pbssBANK0)+3)&07Fh
	clrf	((__pbssBANK0)+4)&07Fh
	clrf	((__pbssBANK0)+5)&07Fh
	clrf	((__pbssBANK0)+6)&07Fh
	clrf	((__pbssBANK0)+7)&07Fh
	clrf	((__pbssBANK0)+8)&07Fh
	clrf	((__pbssBANK0)+9)&07Fh
	clrf	((__pbssBANK0)+10)&07Fh
psect cinit,class=CODE,delta=2
global end_of_initialization

;End of C runtime variable initialization code

end_of_initialization:
clrf status
ljmp _main	;jump to C main() function
psect	cstackCOMMON,class=COMMON,space=1
global __pcstackCOMMON
__pcstackCOMMON:
	global	?_System_init
?_System_init:	; 0 bytes @ 0x0
	global	?_GPIO_Init
?_GPIO_Init:	; 0 bytes @ 0x0
	global	?_LED_Study_End
?_LED_Study_End:	; 0 bytes @ 0x0
	global	?_ADC_Init
?_ADC_Init:	; 0 bytes @ 0x0
	global	?_ExchChannel
?_ExchChannel:	; 0 bytes @ 0x0
	global	?_EEPROM_Init
?_EEPROM_Init:	; 0 bytes @ 0x0
	global	?_main
?_main:	; 0 bytes @ 0x0
	global	?_ISR
?_ISR:	; 0 bytes @ 0x0
	global	??_ISR
??_ISR:	; 0 bytes @ 0x0
	global	?_Read_EEPROM_u8
?_Read_EEPROM_u8:	; 1 bytes @ 0x0
	global	?_isKeyPressed
?_isKeyPressed:	; 1 bytes @ 0x0
	ds	4
	global	?_Delay_xms
?_Delay_xms:	; 0 bytes @ 0x4
	global	?_Write_EEPROM_u8
?_Write_EEPROM_u8:	; 0 bytes @ 0x4
	global	??_Read_EEPROM_u8
??_Read_EEPROM_u8:	; 0 bytes @ 0x4
	global	??_System_init
??_System_init:	; 0 bytes @ 0x4
	global	??_GPIO_Init
??_GPIO_Init:	; 0 bytes @ 0x4
	global	??_isKeyPressed
??_isKeyPressed:	; 0 bytes @ 0x4
	global	??_ExchChannel
??_ExchChannel:	; 0 bytes @ 0x4
	global	?_GetADCValue
?_GetADCValue:	; 2 bytes @ 0x4
	global	Write_EEPROM_u8@EEDatas
Write_EEPROM_u8@EEDatas:	; 1 bytes @ 0x4
	global	Delay_xms@x
Delay_xms@x:	; 2 bytes @ 0x4
	ds	1
	global	??_Write_EEPROM_u8
??_Write_EEPROM_u8:	; 0 bytes @ 0x5
	global	Write_EEPROM_u8@EEAddress
Write_EEPROM_u8@EEAddress:	; 1 bytes @ 0x5
	global	Read_EEPROM_u8@EEAddress
Read_EEPROM_u8@EEAddress:	; 1 bytes @ 0x5
	ds	1
	global	??_Delay_xms
??_Delay_xms:	; 0 bytes @ 0x6
	global	??_GetADCValue
??_GetADCValue:	; 0 bytes @ 0x6
	global	??_EEPROM_Init
??_EEPROM_Init:	; 0 bytes @ 0x6
	global	?_Write_EEPROM_u16
?_Write_EEPROM_u16:	; 0 bytes @ 0x6
	global	ExchChannel@ch_temp
ExchChannel@ch_temp:	; 1 bytes @ 0x6
	global	Read_EEPROM_u8@ReEepromData
Read_EEPROM_u8@ReEepromData:	; 1 bytes @ 0x6
	global	Write_EEPROM_u16@EEDatas
Write_EEPROM_u16@EEDatas:	; 2 bytes @ 0x6
	ds	1
	global	?_Read_EEPROM_u16
?_Read_EEPROM_u16:	; 2 bytes @ 0x7
	global	ExchChannel@adc_ch_temp
ExchChannel@adc_ch_temp:	; 1 bytes @ 0x7
	ds	1
	global	??_Write_EEPROM_u16
??_Write_EEPROM_u16:	; 0 bytes @ 0x8
	global	GetADCValue@ADC_num
GetADCValue@ADC_num:	; 2 bytes @ 0x8
	global	Delay_xms@i
Delay_xms@i:	; 2 bytes @ 0x8
	ds	2
	global	??_LED_Study_End
??_LED_Study_End:	; 0 bytes @ 0xA
	global	??_ADC_Init
??_ADC_Init:	; 0 bytes @ 0xA
psect	cstackBANK0,class=BANK0,space=1
global __pcstackBANK0
__pcstackBANK0:
	global	??_Read_EEPROM_u16
??_Read_EEPROM_u16:	; 0 bytes @ 0x0
	global	Write_EEPROM_u16@DATA_H
Write_EEPROM_u16@DATA_H:	; 1 bytes @ 0x0
	ds	1
	global	Write_EEPROM_u16@DATA_L
Write_EEPROM_u16@DATA_L:	; 1 bytes @ 0x1
	ds	1
	global	Write_EEPROM_u16@EEAddress
Write_EEPROM_u16@EEAddress:	; 1 bytes @ 0x2
	global	Read_EEPROM_u16@EEAddress
Read_EEPROM_u16@EEAddress:	; 1 bytes @ 0x2
	ds	1
	global	Read_EEPROM_u16@EepromData
Read_EEPROM_u16@EepromData:	; 2 bytes @ 0x3
	ds	2
	global	??_main
??_main:	; 0 bytes @ 0x5
	ds	1
	global	main@i
main@i:	; 1 bytes @ 0x6
	ds	1
;;Data sizes: Strings 0, constant 0, data 0, bss 11, persistent 0 stack 0
;;Auto spaces:   Size  Autos    Used
;; COMMON          14     10      10
;; BANK0           80      7      18
;; BANK1           32      0       0

;;
;; Pointer list with targets:

;; ?_Read_EEPROM_u16	unsigned int  size(1) Largest target is 0
;;
;; ?_GetADCValue	unsigned int  size(1) Largest target is 0
;;


;;
;; Critical Paths under _main in COMMON
;;
;;   _main->_GetADCValue
;;   _main->_Delay_xms
;;   _LED_Study_End->_Delay_xms
;;   _ADC_Init->_Delay_xms
;;   _Read_EEPROM_u16->_Read_EEPROM_u8
;;   _Write_EEPROM_u16->_Write_EEPROM_u8
;;   _EEPROM_Init->_Write_EEPROM_u8
;;
;; Critical Paths under _ISR in COMMON
;;
;;   None.
;;
;; Critical Paths under _main in BANK0
;;
;;   _main->_Read_EEPROM_u16
;;
;; Critical Paths under _ISR in BANK0
;;
;;   None.
;;
;; Critical Paths under _main in BANK1
;;
;;   None.
;;
;; Critical Paths under _ISR in BANK1
;;
;;   None.

;;
;;Main: autosize = 0, tempsize = 1, incstack = 0, save=0
;;

;;
;;Call Graph Tables:
;;
;; ---------------------------------------------------------------------------------
;; (Depth) Function   	        Calls       Base Space   Used Autos Params    Refs
;; ---------------------------------------------------------------------------------
;; (0) _main                                                 2     2      0     684
;;                                              5 BANK0      2     2      0
;;                        _System_init
;;                          _GPIO_Init
;;                           _ADC_Init
;;                        _EEPROM_Init
;;                    _Read_EEPROM_u16
;;                      _LED_Study_End
;;                        _ExchChannel
;;                        _GetADCValue
;;                       _isKeyPressed
;;                          _Delay_xms
;;                   _Write_EEPROM_u16
;; ---------------------------------------------------------------------------------
;; (1) _LED_Study_End                                        0     0      0      46
;;                          _Delay_xms
;; ---------------------------------------------------------------------------------
;; (1) _ADC_Init                                             0     0      0      46
;;                          _Delay_xms
;; ---------------------------------------------------------------------------------
;; (1) _Read_EEPROM_u16                                      7     5      2     137
;;                                              7 COMMON     2     0      2
;;                                              0 BANK0      5     5      0
;;                     _Read_EEPROM_u8
;; ---------------------------------------------------------------------------------
;; (1) _Write_EEPROM_u16                                     6     4      2     180
;;                                              6 COMMON     3     1      2
;;                                              0 BANK0      3     3      0
;;                    _Write_EEPROM_u8
;; ---------------------------------------------------------------------------------
;; (1) _EEPROM_Init                                          1     1      0      44
;;                                              6 COMMON     1     1      0
;;                    _Write_EEPROM_u8
;; ---------------------------------------------------------------------------------
;; (1) _ExchChannel                                          4     4      0      68
;;                                              4 COMMON     4     4      0
;; ---------------------------------------------------------------------------------
;; (1) _Delay_xms                                            6     4      2      46
;;                                              4 COMMON     6     4      2
;; ---------------------------------------------------------------------------------
;; (1) _isKeyPressed                                         0     0      0       0
;; ---------------------------------------------------------------------------------
;; (1) _GPIO_Init                                            0     0      0       0
;; ---------------------------------------------------------------------------------
;; (1) _System_init                                          0     0      0       0
;; ---------------------------------------------------------------------------------
;; (2) _Read_EEPROM_u8                                       3     3      0      45
;;                                              4 COMMON     3     3      0
;; ---------------------------------------------------------------------------------
;; (2) _Write_EEPROM_u8                                      2     1      1      44
;;                                              4 COMMON     2     1      1
;; ---------------------------------------------------------------------------------
;; (1) _GetADCValue                                          6     4      2      70
;;                                              4 COMMON     6     4      2
;; ---------------------------------------------------------------------------------
;; Estimated maximum stack depth 2
;; ---------------------------------------------------------------------------------
;; (Depth) Function   	        Calls       Base Space   Used Autos Params    Refs
;; ---------------------------------------------------------------------------------
;; (3) _ISR                                                  4     4      0       0
;;                                              0 COMMON     4     4      0
;; ---------------------------------------------------------------------------------
;; Estimated maximum stack depth 3
;; ---------------------------------------------------------------------------------

;; Call Graph Graphs:

;; _main (ROOT)
;;   _System_init
;;   _GPIO_Init
;;   _ADC_Init
;;     _Delay_xms
;;   _EEPROM_Init
;;     _Write_EEPROM_u8
;;   _Read_EEPROM_u16
;;     _Read_EEPROM_u8
;;   _LED_Study_End
;;     _Delay_xms
;;   _ExchChannel
;;   _GetADCValue
;;   _isKeyPressed
;;   _Delay_xms
;;   _Write_EEPROM_u16
;;     _Write_EEPROM_u8
;;
;; _ISR (ROOT)
;;

;; Address spaces:

;;Name               Size   Autos  Total    Cost      Usage
;;BITCOMMON            E      0       0       0        0.0%
;;EEDATA             100      0       0       0        0.0%
;;NULL                 0      0       0       0        0.0%
;;CODE                 0      0       0       0        0.0%
;;COMMON               E      A       A       1       71.4%
;;BITSFR0              0      0       0       1        0.0%
;;SFR0                 0      0       0       1        0.0%
;;BITSFR1              0      0       0       2        0.0%
;;SFR1                 0      0       0       2        0.0%
;;STACK                0      0       2       2        0.0%
;;BITBANK0            50      0       0       3        0.0%
;;BANK0               50      7      12       4       22.5%
;;BITBANK1            20      0       0       5        0.0%
;;BITSFR2              0      0       0       5        0.0%
;;SFR2                 0      0       0       5        0.0%
;;BANK1               20      0       0       6        0.0%
;;ABS                  0      0      1C       7        0.0%
;;DATA                 0      0      1E       8        0.0%

	global	_main
psect	maintext,global,class=CODE,delta=2
global __pmaintext
__pmaintext:

;; *************** function _main *****************
;; Defined at:
;;		line 41 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;  i               1    6[BANK0 ] unsigned char 
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2, status,0, pclath, cstack
;; Tracked objects:
;;		On entry : 17F/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       1       0
;;      Temps:          0       1       0
;;      Totals:         0       2       0
;;Total ram usage:        2 bytes
;; Hardware stack levels required when called:    3
;; This function calls:
;;		_System_init
;;		_GPIO_Init
;;		_ADC_Init
;;		_EEPROM_Init
;;		_Read_EEPROM_u16
;;		_LED_Study_End
;;		_ExchChannel
;;		_GetADCValue
;;		_isKeyPressed
;;		_Delay_xms
;;		_Write_EEPROM_u16
;; This function is called by:
;;		Startup code after reset
;; This function uses a non-reentrant model
;;
psect	maintext
	file	"main.c"
	line	41
	global	__size_of_main
	__size_of_main	equ	__end_of_main-_main
	
_main:	
	opt	stack 5
; Regs used in _main: [wreg+status,2+status,0+pclath+cstack]
	line	42
	
l7650:	
;main.c: 42: unsigned char i=0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(main@i)
	line	44
	
l7652:	
;main.c: 44: System_init();
	fcall	_System_init
	line	45
	
l7654:	
;main.c: 45: GPIO_Init();
	fcall	_GPIO_Init
	line	46
	
l7656:	
;main.c: 46: PC2=0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bcf	(58/8),(58)&7
	line	47
	
l7658:	
;main.c: 47: ADC_Init();
	fcall	_ADC_Init
	line	48
	
l7660:	
;main.c: 48: EEPROM_Init();
	fcall	_EEPROM_Init
	line	50
	
l7662:	
;main.c: 50: adc_Study = Read_EEPROM_u16 ( 0x00 );
	movlw	(0)
	fcall	_Read_EEPROM_u16
	movf	(1+(?_Read_EEPROM_u16)),w
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(_adc_Study+1)
	addwf	(_adc_Study+1)
	movf	(0+(?_Read_EEPROM_u16)),w
	clrf	(_adc_Study)
	addwf	(_adc_Study)

	line	51
	
l7664:	
;main.c: 51: if ( adc_Study == 0XFFFF ) {
	movlw	high(0FFFFh)
	xorwf	(_adc_Study+1),w
	skipz
	goto	u2445
	movlw	low(0FFFFh)
	xorwf	(_adc_Study),w
u2445:

	skipz
	goto	u2441
	goto	u2440
u2441:
	goto	l7668
u2440:
	line	52
	
l7666:	
;main.c: 52: adc_Study = 600;
	movlw	low(0258h)
	movwf	(_adc_Study)
	movlw	high(0258h)
	movwf	((_adc_Study))+1
	goto	l7668
	line	53
	
l3266:	
	line	54
	
l7668:	
;main.c: 53: }
;main.c: 54: LED_Study_End();
	fcall	_LED_Study_End
	line	55
	
l7670:	
;main.c: 55: ExchChannel ( 5 );
	movlw	(05h)
	fcall	_ExchChannel
	goto	l7672
	line	56
;main.c: 56: while ( 1 ) {
	
l3267:	
	line	57
	
l7672:	
;main.c: 57: flag_sc=0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(_flag_sc)
	line	60
	
l7674:	
;main.c: 60: adc_value = GetADCValue();
	fcall	_GetADCValue
	movf	(1+(?_GetADCValue)),w
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(_adc_value+1)
	addwf	(_adc_value+1)
	movf	(0+(?_GetADCValue)),w
	clrf	(_adc_value)
	addwf	(_adc_value)

	line	61
	
l7676:	
;main.c: 61: if ( isKeyPressed() ) {
	fcall	_isKeyPressed
	xorlw	0
	skipnz
	goto	u2451
	goto	u2450
u2451:
	goto	l7718
u2450:
	line	62
	
l7678:	
;main.c: 62: volume1=0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(_volume1)
	clrf	(_volume1+1)
	line	63
	
l7680:	
;main.c: 63: Delay_xms ( 20 );
	movlw	low(014h)
	movwf	(?_Delay_xms)
	movlw	high(014h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	64
	
l7682:	
;main.c: 64: times=0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(_times)
	clrf	(_times+1)
	line	66
;main.c: 66: while ( !isKeyPressed() ) {
	goto	l7716
	
l3270:	
	line	67
;main.c: 67: PA6=1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(46/8),(46)&7
	line	68
;main.c: 68: PC2=0;
	bcf	(58/8),(58)&7
	line	69
	
l7684:	
;main.c: 69: if ( flag_sc == 0 ) {
	movf	(_flag_sc),f
	skipz
	goto	u2461
	goto	u2460
u2461:
	goto	l7690
u2460:
	line	70
	
l7686:	
;main.c: 70: flag_sc=1;
	clrf	(_flag_sc)
	bsf	status,0
	rlf	(_flag_sc),f
	line	71
	
l7688:	
;main.c: 71: Delay_xms ( 300 );
	movlw	low(012Ch)
	movwf	(?_Delay_xms)
	movlw	high(012Ch)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	goto	l7690
	line	72
	
l3271:	
	line	73
	
l7690:	
;main.c: 72: }
;main.c: 73: times++;
	movlw	low(01h)
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	addwf	(_times),f
	skipnc
	incf	(_times+1),f
	movlw	high(01h)
	addwf	(_times+1),f
	line	74
	
l7692:	
;main.c: 74: if ( times>=2000 ) {
	movlw	high(07D0h)
	subwf	(_times+1),w
	movlw	low(07D0h)
	skipnz
	subwf	(_times),w
	skipc
	goto	u2471
	goto	u2470
u2471:
	goto	l7702
u2470:
	line	75
	
l7694:	
;main.c: 75: adc_Study = volume1;
	movf	(_volume1+1),w
	clrf	(_adc_Study+1)
	addwf	(_adc_Study+1)
	movf	(_volume1),w
	clrf	(_adc_Study)
	addwf	(_adc_Study)

	line	76
	
l7696:	
;main.c: 76: Write_EEPROM_u16 ( 0x00,adc_Study );
	movf	(_adc_Study+1),w
	clrf	(?_Write_EEPROM_u16+1)
	addwf	(?_Write_EEPROM_u16+1)
	movf	(_adc_Study),w
	clrf	(?_Write_EEPROM_u16)
	addwf	(?_Write_EEPROM_u16)

	movlw	(0)
	fcall	_Write_EEPROM_u16
	line	77
	
l7698:	
;main.c: 77: PA6=0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bcf	(46/8),(46)&7
	goto	l3273
	line	78
	
l7700:	
;main.c: 78: return;
	goto	l3273
	line	79
	
l3272:	
	line	80
	
l7702:	
;main.c: 79: }
;main.c: 80: adc_value = GetADCValue();
	fcall	_GetADCValue
	movf	(1+(?_GetADCValue)),w
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(_adc_value+1)
	addwf	(_adc_value+1)
	movf	(0+(?_GetADCValue)),w
	clrf	(_adc_value)
	addwf	(_adc_value)

	line	81
	
l7704:	
;main.c: 81: if ( volume1 < adc_value ) {
	movf	(_adc_value+1),w
	subwf	(_volume1+1),w
	skipz
	goto	u2485
	movf	(_adc_value),w
	subwf	(_volume1),w
u2485:
	skipnc
	goto	u2481
	goto	u2480
u2481:
	goto	l7714
u2480:
	line	82
	
l7706:	
;main.c: 82: if ( adc_value >= 700 ) {
	movlw	high(02BCh)
	subwf	(_adc_value+1),w
	movlw	low(02BCh)
	skipnz
	subwf	(_adc_value),w
	skipc
	goto	u2491
	goto	u2490
u2491:
	goto	l3275
u2490:
	line	83
	
l7708:	
;main.c: 83: adc_value = 700;
	movlw	low(02BCh)
	movwf	(_adc_value)
	movlw	high(02BCh)
	movwf	((_adc_value))+1
	line	84
	
l3275:	
	line	85
;main.c: 84: }
;main.c: 85: if ( adc_value<=540 ) {
	movlw	high(021Dh)
	subwf	(_adc_value+1),w
	movlw	low(021Dh)
	skipnz
	subwf	(_adc_value),w
	skipnc
	goto	u2501
	goto	u2500
u2501:
	goto	l7712
u2500:
	line	86
	
l7710:	
;main.c: 86: adc_value=540;
	movlw	low(021Ch)
	movwf	(_adc_value)
	movlw	high(021Ch)
	movwf	((_adc_value))+1
	goto	l7712
	line	87
	
l3276:	
	line	88
	
l7712:	
;main.c: 87: }
;main.c: 88: volume1 = adc_value;
	movf	(_adc_value+1),w
	clrf	(_volume1+1)
	addwf	(_volume1+1)
	movf	(_adc_value),w
	clrf	(_volume1)
	addwf	(_volume1)

	goto	l7714
	line	89
	
l3274:	
	line	90
	
l7714:	
;main.c: 89: }
;main.c: 90: Delay_xms ( 1 );
	movlw	low(01h)
	movwf	(?_Delay_xms)
	movlw	high(01h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	goto	l7716
	line	91
	
l3269:	
	line	66
	
l7716:	
	fcall	_isKeyPressed
	xorlw	0
	skipnz
	goto	u2511
	goto	u2510
u2511:
	goto	l3270
u2510:
	goto	l7718
	
l3277:	
	goto	l7718
	line	92
	
l3268:	
	line	93
	
l7718:	
;main.c: 91: }
;main.c: 92: }
;main.c: 93: if ( adc_value >= adc_Study ) {
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	(_adc_Study+1),w
	subwf	(_adc_value+1),w
	skipz
	goto	u2525
	movf	(_adc_Study),w
	subwf	(_adc_value),w
u2525:
	skipc
	goto	u2521
	goto	u2520
u2521:
	goto	l3278
u2520:
	line	96
	
l7720:	
;main.c: 96: ExchChannel ( 0 );
	movlw	(0)
	fcall	_ExchChannel
	line	97
	
l7722:	
;main.c: 97: for ( i=0; i<5; i++ ) {
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(main@i)
	
l7724:	
	movlw	(05h)
	subwf	(main@i),w
	skipc
	goto	u2531
	goto	u2530
u2531:
	goto	l7728
u2530:
	goto	l7734
	
l7726:	
	goto	l7734
	
l3279:	
	line	98
	
l7728:	
;main.c: 98: adc_input = GetADCValue();
	fcall	_GetADCValue
	movf	(1+(?_GetADCValue)),w
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(_adc_input+1)
	addwf	(_adc_input+1)
	movf	(0+(?_GetADCValue)),w
	clrf	(_adc_input)
	addwf	(_adc_input)

	line	97
	
l7730:	
	movlw	(01h)
	movwf	(??_main+0)+0
	movf	(??_main+0)+0,w
	addwf	(main@i),f
	
l7732:	
	movlw	(05h)
	subwf	(main@i),w
	skipc
	goto	u2541
	goto	u2540
u2541:
	goto	l7728
u2540:
	goto	l7734
	
l3280:	
	line	100
	
l7734:	
;main.c: 99: }
;main.c: 100: if ( adc_input>=500 ) {
	movlw	high(01F4h)
	subwf	(_adc_input+1),w
	movlw	low(01F4h)
	skipnz
	subwf	(_adc_input),w
	skipc
	goto	u2551
	goto	u2550
u2551:
	goto	l3281
u2550:
	line	101
	
l7736:	
;main.c: 101: ExchChannel ( 5 );
	movlw	(05h)
	fcall	_ExchChannel
	line	102
	
l7738:	
;main.c: 102: PC2=1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(58/8),(58)&7
	line	103
;main.c: 103: Delay_xms ( 100 );
	movlw	low(064h)
	movwf	(?_Delay_xms)
	movlw	high(064h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	104
	
l7740:	
;main.c: 104: PC2=0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bcf	(58/8),(58)&7
	line	105
;main.c: 105: Delay_xms ( 400 );
	movlw	low(0190h)
	movwf	(?_Delay_xms)
	movlw	high(0190h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	106
;main.c: 106: } else {
	goto	l3282
	
l3281:	
	line	107
;main.c: 107: PC2=0;
	bcf	(58/8),(58)&7
	line	108
	
l7742:	
;main.c: 108: Delay_xms ( 10 );
	movlw	low(0Ah)
	movwf	(?_Delay_xms)
	movlw	high(0Ah)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	110
	
l3282:	
	line	111
;main.c: 110: }
;main.c: 111: ExchChannel ( 5 );
	movlw	(05h)
	fcall	_ExchChannel
	line	112
;main.c: 112: } else {
	goto	l7672
	
l3278:	
	line	113
;main.c: 113: PC2=0;
	bcf	(58/8),(58)&7
	goto	l7672
	line	114
	
l3283:	
	goto	l7672
	line	115
	
l3284:	
	line	56
	goto	l7672
	
l3285:	
	line	116
	
l3273:	
	global	start
	ljmp	start
	opt stack 0
GLOBAL	__end_of_main
	__end_of_main:
;; =============== function _main ends ============

	signat	_main,88
	global	_LED_Study_End
psect	text372,local,class=CODE,delta=2
global __ptext372
__ptext372:

;; *************** function _LED_Study_End *****************
;; Defined at:
;;		line 204 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2, status,0, pclath, cstack
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          0       0       0
;;      Totals:         0       0       0
;;Total ram usage:        0 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    2
;; This function calls:
;;		_Delay_xms
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text372
	file	"main.c"
	line	204
	global	__size_of_LED_Study_End
	__size_of_LED_Study_End	equ	__end_of_LED_Study_End-_LED_Study_End
	
_LED_Study_End:	
	opt	stack 5
; Regs used in _LED_Study_End: [wreg+status,2+status,0+pclath+cstack]
	line	205
	
l7636:	
;main.c: 205: PA6 = 1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(46/8),(46)&7
	line	206
	
l7638:	
;main.c: 206: Delay_xms ( 40 );
	movlw	low(028h)
	movwf	(?_Delay_xms)
	movlw	high(028h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	207
	
l7640:	
;main.c: 207: PA6 = 0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bcf	(46/8),(46)&7
	line	208
;main.c: 208: Delay_xms ( 40 );
	movlw	low(028h)
	movwf	(?_Delay_xms)
	movlw	high(028h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	209
	
l7642:	
;main.c: 209: PA6 = 1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(46/8),(46)&7
	line	210
;main.c: 210: Delay_xms ( 40 );
	movlw	low(028h)
	movwf	(?_Delay_xms)
	movlw	high(028h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	211
	
l7644:	
;main.c: 211: PA6 = 0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bcf	(46/8),(46)&7
	line	212
;main.c: 212: Delay_xms ( 40 );
	movlw	low(028h)
	movwf	(?_Delay_xms)
	movlw	high(028h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	213
	
l7646:	
;main.c: 213: PA6 = 1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(46/8),(46)&7
	line	214
;main.c: 214: Delay_xms ( 40 );
	movlw	low(028h)
	movwf	(?_Delay_xms)
	movlw	high(028h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	215
	
l7648:	
;main.c: 215: PA6 = 0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bcf	(46/8),(46)&7
	line	216
;main.c: 216: Delay_xms ( 40 );
	movlw	low(028h)
	movwf	(?_Delay_xms)
	movlw	high(028h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	217
	
l3303:	
	return
	opt stack 0
GLOBAL	__end_of_LED_Study_End
	__end_of_LED_Study_End:
;; =============== function _LED_Study_End ends ============

	signat	_LED_Study_End,88
	global	_ADC_Init
psect	text373,local,class=CODE,delta=2
global __ptext373
__ptext373:

;; *************** function _ADC_Init *****************
;; Defined at:
;;		line 17 in file "adc.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2, status,0, pclath, cstack
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          0       0       0
;;      Totals:         0       0       0
;;Total ram usage:        0 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    2
;; This function calls:
;;		_Delay_xms
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text373
	file	"adc.c"
	line	17
	global	__size_of_ADC_Init
	__size_of_ADC_Init	equ	__end_of_ADC_Init-_ADC_Init
	
_ADC_Init:	
	opt	stack 5
; Regs used in _ADC_Init: [wreg+status,2+status,0+pclath+cstack]
	line	19
	
l7616:	
;adc.c: 19: TRISA0=1;
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	bsf	(1064/8)^080h,(1064)&7
	line	20
;adc.c: 20: TRISC1=1;
	bsf	(1081/8)^080h,(1081)&7
	line	21
;adc.c: 21: ANSEL0=1;
	bsf	(1160/8)^080h,(1160)&7
	line	22
;adc.c: 22: ANSEL5=1;
	bsf	(1165/8)^080h,(1165)&7
	line	25
;adc.c: 25: DIVS=0;
	bcf	(1279/8)^080h,(1279)&7
	line	26
;adc.c: 26: ADCS0=0;
	bcf	(1276/8)^080h,(1276)&7
	line	27
;adc.c: 27: ADCS1=0;
	bcf	(1277/8)^080h,(1277)&7
	line	28
;adc.c: 28: ADCS2=0;
	bcf	(1278/8)^080h,(1278)&7
	line	30
	
l7618:	
;adc.c: 30: ADCON0=0B10000001;
	movlw	(081h)
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movwf	(31)	;volatile
	line	31
	
l7620:	
;adc.c: 31: VCFG0=0;
	bcf	(253/8),(253)&7
	line	32
	
l7622:	
;adc.c: 32: CHS0=1;
	bsf	(250/8),(250)&7
	line	33
	
l7624:	
;adc.c: 33: CHS1=0;
	bcf	(251/8),(251)&7
	line	34
	
l7626:	
;adc.c: 34: CHS2=1;
	bsf	(252/8),(252)&7
	line	35
	
l7628:	
;adc.c: 35: ADFM=1;
	bsf	(255/8),(255)&7
	line	36
	
l7630:	
;adc.c: 36: ADON=1;
	bsf	(248/8),(248)&7
	line	38
	
l7632:	
;adc.c: 38: Delay_xms(1);
	movlw	low(01h)
	movwf	(?_Delay_xms)
	movlw	high(01h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	39
	
l7634:	
;adc.c: 39: GO_DONE=1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(249/8),(249)&7
	line	40
	
l1069:	
	return
	opt stack 0
GLOBAL	__end_of_ADC_Init
	__end_of_ADC_Init:
;; =============== function _ADC_Init ends ============

	signat	_ADC_Init,88
	global	_Read_EEPROM_u16
psect	text374,local,class=CODE,delta=2
global __ptext374
__ptext374:

;; *************** function _Read_EEPROM_u16 *****************
;; Defined at:
;;		line 39 in file "eeprom.c"
;; Parameters:    Size  Location     Type
;;  EEAddress       1    wreg     unsigned char 
;; Auto vars:     Size  Location     Type
;;  EEAddress       1    2[BANK0 ] unsigned char 
;;  EepromData      2    3[BANK0 ] unsigned int 
;; Return value:  Size  Location     Type
;;                  2    7[COMMON] unsigned int 
;; Registers used:
;;		wreg, status,2, status,0, pclath, cstack
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         2       0       0
;;      Locals:         0       3       0
;;      Temps:          0       2       0
;;      Totals:         2       5       0
;;Total ram usage:        7 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    2
;; This function calls:
;;		_Read_EEPROM_u8
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text374
	file	"eeprom.c"
	line	39
	global	__size_of_Read_EEPROM_u16
	__size_of_Read_EEPROM_u16	equ	__end_of_Read_EEPROM_u16-_Read_EEPROM_u16
	
_Read_EEPROM_u16:	
	opt	stack 5
; Regs used in _Read_EEPROM_u16: [wreg+status,2+status,0+pclath+cstack]
;Read_EEPROM_u16@EEAddress stored from wreg
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movwf	(Read_EEPROM_u16@EEAddress)
	line	40
	
l7604:	
;eeprom.c: 40: unsigned int EepromData=0;
	clrf	(Read_EEPROM_u16@EepromData)
	clrf	(Read_EEPROM_u16@EepromData+1)
	line	42
	
l7606:	
;eeprom.c: 42: EepromData = Read_EEPROM_u8(EEAddress);
	movf	(Read_EEPROM_u16@EEAddress),w
	fcall	_Read_EEPROM_u8
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movwf	(??_Read_EEPROM_u16+0)+0
	clrf	(??_Read_EEPROM_u16+0)+0+1
	movf	0+(??_Read_EEPROM_u16+0)+0,w
	movwf	(Read_EEPROM_u16@EepromData)
	movf	1+(??_Read_EEPROM_u16+0)+0,w
	movwf	(Read_EEPROM_u16@EepromData+1)
	line	43
	
l7608:	
;eeprom.c: 43: EepromData = EepromData<<8;
	movf	(Read_EEPROM_u16@EepromData),w
	movwf	(??_Read_EEPROM_u16+0)+0+1
	clrf	(??_Read_EEPROM_u16+0)+0
	movf	0+(??_Read_EEPROM_u16+0)+0,w
	movwf	(Read_EEPROM_u16@EepromData)
	movf	1+(??_Read_EEPROM_u16+0)+0,w
	movwf	(Read_EEPROM_u16@EepromData+1)
	line	44
	
l7610:	
;eeprom.c: 44: EepromData |= Read_EEPROM_u8(EEAddress+1);
	movf	(Read_EEPROM_u16@EEAddress),w
	addlw	01h
	fcall	_Read_EEPROM_u8
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movwf	(??_Read_EEPROM_u16+0)+0
	clrf	(??_Read_EEPROM_u16+0)+0+1
	movf	0+(??_Read_EEPROM_u16+0)+0,w
	iorwf	(Read_EEPROM_u16@EepromData),f
	movf	1+(??_Read_EEPROM_u16+0)+0,w
	iorwf	(Read_EEPROM_u16@EepromData+1),f
	line	45
	
l7612:	
;eeprom.c: 45: return EepromData;
	movf	(Read_EEPROM_u16@EepromData+1),w
	clrf	(?_Read_EEPROM_u16+1)
	addwf	(?_Read_EEPROM_u16+1)
	movf	(Read_EEPROM_u16@EepromData),w
	clrf	(?_Read_EEPROM_u16)
	addwf	(?_Read_EEPROM_u16)

	goto	l2153
	
l7614:	
	line	46
	
l2153:	
	return
	opt stack 0
GLOBAL	__end_of_Read_EEPROM_u16
	__end_of_Read_EEPROM_u16:
;; =============== function _Read_EEPROM_u16 ends ============

	signat	_Read_EEPROM_u16,4218
	global	_Write_EEPROM_u16
psect	text375,local,class=CODE,delta=2
global __ptext375
__ptext375:

;; *************** function _Write_EEPROM_u16 *****************
;; Defined at:
;;		line 27 in file "eeprom.c"
;; Parameters:    Size  Location     Type
;;  EEAddress       1    wreg     unsigned char 
;;  EEDatas         2    6[COMMON] unsigned int 
;; Auto vars:     Size  Location     Type
;;  EEAddress       1    2[BANK0 ] unsigned char 
;;  DATA_L          1    1[BANK0 ] unsigned char 
;;  DATA_H          1    0[BANK0 ] unsigned char 
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2, status,0, pclath, cstack
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         2       0       0
;;      Locals:         0       3       0
;;      Temps:          1       0       0
;;      Totals:         3       3       0
;;Total ram usage:        6 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    2
;; This function calls:
;;		_Write_EEPROM_u8
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text375
	file	"eeprom.c"
	line	27
	global	__size_of_Write_EEPROM_u16
	__size_of_Write_EEPROM_u16	equ	__end_of_Write_EEPROM_u16-_Write_EEPROM_u16
	
_Write_EEPROM_u16:	
	opt	stack 5
; Regs used in _Write_EEPROM_u16: [wreg+status,2+status,0+pclath+cstack]
;Write_EEPROM_u16@EEAddress stored from wreg
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movwf	(Write_EEPROM_u16@EEAddress)
	line	28
	
l7594:	
;eeprom.c: 28: unsigned char DATA_H=0;
	clrf	(Write_EEPROM_u16@DATA_H)
	line	29
;eeprom.c: 29: unsigned char DATA_L=0;
	clrf	(Write_EEPROM_u16@DATA_L)
	line	31
	
l7596:	
;eeprom.c: 31: DATA_H = (EEDatas>>8)&0xff;
	movf	(Write_EEPROM_u16@EEDatas+1),w
	movwf	(??_Write_EEPROM_u16+0)+0
	movf	(??_Write_EEPROM_u16+0)+0,w
	movwf	(Write_EEPROM_u16@DATA_H)
	line	32
	
l7598:	
;eeprom.c: 32: DATA_L = EEDatas&0xff;
	movf	(Write_EEPROM_u16@EEDatas),w
	movwf	(??_Write_EEPROM_u16+0)+0
	movf	(??_Write_EEPROM_u16+0)+0,w
	movwf	(Write_EEPROM_u16@DATA_L)
	line	33
	
l7600:	
;eeprom.c: 33: Write_EEPROM_u8(EEAddress,DATA_H);
	movf	(Write_EEPROM_u16@DATA_H),w
	movwf	(??_Write_EEPROM_u16+0)+0
	movf	(??_Write_EEPROM_u16+0)+0,w
	movwf	(?_Write_EEPROM_u8)
	movf	(Write_EEPROM_u16@EEAddress),w
	fcall	_Write_EEPROM_u8
	line	34
	
l7602:	
;eeprom.c: 34: Write_EEPROM_u8(EEAddress+1,DATA_L);
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	(Write_EEPROM_u16@DATA_L),w
	movwf	(??_Write_EEPROM_u16+0)+0
	movf	(??_Write_EEPROM_u16+0)+0,w
	movwf	(?_Write_EEPROM_u8)
	movf	(Write_EEPROM_u16@EEAddress),w
	addlw	01h
	fcall	_Write_EEPROM_u8
	line	36
	
l2150:	
	return
	opt stack 0
GLOBAL	__end_of_Write_EEPROM_u16
	__end_of_Write_EEPROM_u16:
;; =============== function _Write_EEPROM_u16 ends ============

	signat	_Write_EEPROM_u16,8312
	global	_EEPROM_Init
psect	text376,local,class=CODE,delta=2
global __ptext376
__ptext376:

;; *************** function _EEPROM_Init *****************
;; Defined at:
;;		line 13 in file "eeprom.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2, status,0, pclath, cstack
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          1       0       0
;;      Totals:         1       0       0
;;Total ram usage:        1 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    2
;; This function calls:
;;		_Write_EEPROM_u8
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text376
	file	"eeprom.c"
	line	13
	global	__size_of_EEPROM_Init
	__size_of_EEPROM_Init	equ	__end_of_EEPROM_Init-_EEPROM_Init
	
_EEPROM_Init:	
	opt	stack 5
; Regs used in _EEPROM_Init: [wreg+status,2+status,0+pclath+cstack]
	line	14
	
l7592:	
;eeprom.c: 14: Write_EEPROM_u8(0xFF,0xAA);
	movlw	(0AAh)
	movwf	(??_EEPROM_Init+0)+0
	movf	(??_EEPROM_Init+0)+0,w
	movwf	(?_Write_EEPROM_u8)
	movlw	(0FFh)
	fcall	_Write_EEPROM_u8
	line	15
;eeprom.c: 15: Write_EEPROM_u8(0xFF,0xAA);
	movlw	(0AAh)
	movwf	(??_EEPROM_Init+0)+0
	movf	(??_EEPROM_Init+0)+0,w
	movwf	(?_Write_EEPROM_u8)
	movlw	(0FFh)
	fcall	_Write_EEPROM_u8
	line	16
	
l2147:	
	return
	opt stack 0
GLOBAL	__end_of_EEPROM_Init
	__end_of_EEPROM_Init:
;; =============== function _EEPROM_Init ends ============

	signat	_EEPROM_Init,88
	global	_ExchChannel
psect	text377,local,class=CODE,delta=2
global __ptext377
__ptext377:

;; *************** function _ExchChannel *****************
;; Defined at:
;;		line 66 in file "adc.c"
;; Parameters:    Size  Location     Type
;;  ch_temp         1    wreg     unsigned char 
;; Auto vars:     Size  Location     Type
;;  ch_temp         1    6[COMMON] unsigned char 
;;  adc_ch_temp     1    7[COMMON] unsigned char 
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2, status,0
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         2       0       0
;;      Temps:          2       0       0
;;      Totals:         4       0       0
;;Total ram usage:        4 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    1
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text377
	file	"adc.c"
	line	66
	global	__size_of_ExchChannel
	__size_of_ExchChannel	equ	__end_of_ExchChannel-_ExchChannel
	
_ExchChannel:	
	opt	stack 6
; Regs used in _ExchChannel: [wreg+status,2+status,0]
;ExchChannel@ch_temp stored from wreg
	line	69
	movwf	(ExchChannel@ch_temp)
	
l7584:	
;adc.c: 67: unsigned char adc_ch_temp;
;adc.c: 69: adc_ch_temp = ch_temp;
	movf	(ExchChannel@ch_temp),w
	movwf	(??_ExchChannel+0)+0
	movf	(??_ExchChannel+0)+0,w
	movwf	(ExchChannel@adc_ch_temp)
	line	70
	
l7586:	
;adc.c: 70: adc_ch_temp = adc_ch_temp<<2;
	movf	(ExchChannel@adc_ch_temp),w
	movwf	(??_ExchChannel+0)+0
	movlw	(02h)-1
u2435:
	clrc
	rlf	(??_ExchChannel+0)+0,f
	addlw	-1
	skipz
	goto	u2435
	clrc
	rlf	(??_ExchChannel+0)+0,w
	movwf	(??_ExchChannel+1)+0
	movf	(??_ExchChannel+1)+0,w
	movwf	(ExchChannel@adc_ch_temp)
	line	71
	
l7588:	
;adc.c: 71: ADCON0 = (ADCON0&0xe3)|adc_ch_temp;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	(31),w
	andlw	0E3h
	iorwf	(ExchChannel@adc_ch_temp),w
	movwf	(31)	;volatile
	line	72
;adc.c: 72: _delay((unsigned long)((1)*(16000000/4000.0)));
	opt asmopt_off
movlw	6
movwf	((??_ExchChannel+0)+0+1),f
	movlw	48
movwf	((??_ExchChannel+0)+0),f
u2567:
	decfsz	((??_ExchChannel+0)+0),f
	goto	u2567
	decfsz	((??_ExchChannel+0)+0+1),f
	goto	u2567
	clrwdt
opt asmopt_on

	line	73
	
l7590:	
;adc.c: 73: GO_DONE=1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(249/8),(249)&7
	line	74
	
l1078:	
	return
	opt stack 0
GLOBAL	__end_of_ExchChannel
	__end_of_ExchChannel:
;; =============== function _ExchChannel ends ============

	signat	_ExchChannel,4216
	global	_Delay_xms
psect	text378,local,class=CODE,delta=2
global __ptext378
__ptext378:

;; *************** function _Delay_xms *****************
;; Defined at:
;;		line 189 in file "main.c"
;; Parameters:    Size  Location     Type
;;  x               2    4[COMMON] unsigned int 
;; Auto vars:     Size  Location     Type
;;  i               2    8[COMMON] unsigned int 
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         2       0       0
;;      Locals:         2       0       0
;;      Temps:          2       0       0
;;      Totals:         6       0       0
;;Total ram usage:        6 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    1
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_ADC_Init
;;		_main
;;		_LED_Study_End
;; This function uses a non-reentrant model
;;
psect	text378
	file	"main.c"
	line	189
	global	__size_of_Delay_xms
	__size_of_Delay_xms	equ	__end_of_Delay_xms-_Delay_xms
	
_Delay_xms:	
	opt	stack 6
; Regs used in _Delay_xms: [wreg+status,2]
	line	191
	
l7580:	
;main.c: 190: unsigned int i;
;main.c: 191: for ( i=0; i<x; i++ ) {
	clrf	(Delay_xms@i)
	clrf	(Delay_xms@i+1)
	goto	l3297
	
l3298:	
	line	192
	
l7582:	
;main.c: 192: _delay((unsigned long)((1)*(16000000/4000.0)));
	opt asmopt_off
movlw	6
movwf	((??_Delay_xms+0)+0+1),f
	movlw	48
movwf	((??_Delay_xms+0)+0),f
u2577:
	decfsz	((??_Delay_xms+0)+0),f
	goto	u2577
	decfsz	((??_Delay_xms+0)+0+1),f
	goto	u2577
	clrwdt
opt asmopt_on

	line	191
	movlw	low(01h)
	addwf	(Delay_xms@i),f
	skipnc
	incf	(Delay_xms@i+1),f
	movlw	high(01h)
	addwf	(Delay_xms@i+1),f
	
l3297:	
	movf	(Delay_xms@x+1),w
	subwf	(Delay_xms@i+1),w
	skipz
	goto	u2425
	movf	(Delay_xms@x),w
	subwf	(Delay_xms@i),w
u2425:
	skipc
	goto	u2421
	goto	u2420
u2421:
	goto	l7582
u2420:
	goto	l3300
	
l3299:	
	line	195
	
l3300:	
	return
	opt stack 0
GLOBAL	__end_of_Delay_xms
	__end_of_Delay_xms:
;; =============== function _Delay_xms ends ============

	signat	_Delay_xms,4216
	global	_isKeyPressed
psect	text379,local,class=CODE,delta=2
global __ptext379
__ptext379:

;; *************** function _isKeyPressed *****************
;; Defined at:
;;		line 18 in file "key.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;                  1    wreg      unsigned char 
;; Registers used:
;;		wreg, status,2, status,0
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          0       0       0
;;      Totals:         0       0       0
;;Total ram usage:        0 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    1
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text379
	file	"key.c"
	line	18
	global	__size_of_isKeyPressed
	__size_of_isKeyPressed	equ	__end_of_isKeyPressed-_isKeyPressed
	
_isKeyPressed:	
	opt	stack 6
; Regs used in _isKeyPressed: [wreg+status,2+status,0]
	line	19
	
l7576:	
;key.c: 19: return PC3 ? 0:1;
	clrc
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	btfss	(59/8),(59)&7
	setc
	movlw	0
	skipnc
	movlw	1

	goto	l4368
	
l7578:	
	line	20
	
l4368:	
	return
	opt stack 0
GLOBAL	__end_of_isKeyPressed
	__end_of_isKeyPressed:
;; =============== function _isKeyPressed ends ============

	signat	_isKeyPressed,89
	global	_GPIO_Init
psect	text380,local,class=CODE,delta=2
global __ptext380
__ptext380:

;; *************** function _GPIO_Init *****************
;; Defined at:
;;		line 143 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		None
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          0       0       0
;;      Totals:         0       0       0
;;Total ram usage:        0 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    1
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text380
	file	"main.c"
	line	143
	global	__size_of_GPIO_Init
	__size_of_GPIO_Init	equ	__end_of_GPIO_Init-_GPIO_Init
	
_GPIO_Init:	
	opt	stack 6
; Regs used in _GPIO_Init: []
	line	145
	
l7574:	
;main.c: 145: TRISA0=0;
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	bcf	(1064/8)^080h,(1064)&7
	line	146
;main.c: 146: TRISA1=0;
	bcf	(1065/8)^080h,(1065)&7
	line	147
;main.c: 147: TRISA6=0;
	bcf	(1070/8)^080h,(1070)&7
	line	149
;main.c: 149: TRISC1=0;
	bcf	(1081/8)^080h,(1081)&7
	line	150
;main.c: 150: TRISC2=0;
	bcf	(1082/8)^080h,(1082)&7
	line	151
;main.c: 151: TRISC3=1;
	bsf	(1083/8)^080h,(1083)&7
	line	153
;main.c: 153: CM0=1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(200/8),(200)&7
	line	154
;main.c: 154: CM1=1;
	bsf	(201/8),(201)&7
	line	155
;main.c: 155: CM2=1;
	bsf	(202/8),(202)&7
	line	159
;main.c: 159: ANSEL0=0;
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	bcf	(1160/8)^080h,(1160)&7
	line	160
;main.c: 160: ANSEL1=0;
	bcf	(1161/8)^080h,(1161)&7
	line	161
;main.c: 161: ANSEL2=0;
	bcf	(1162/8)^080h,(1162)&7
	line	162
;main.c: 162: ANSEL3=0;
	bcf	(1163/8)^080h,(1163)&7
	line	163
;main.c: 163: ANSEL4=0;
	bcf	(1164/8)^080h,(1164)&7
	line	164
;main.c: 164: ANSEL5=0;
	bcf	(1165/8)^080h,(1165)&7
	line	165
;main.c: 165: ANSEL6=0;
	bcf	(1166/8)^080h,(1166)&7
	line	166
;main.c: 166: ANSEL7=0;
	bcf	(1167/8)^080h,(1167)&7
	line	168
;main.c: 168: WPUA0=1;
	bsf	(1192/8)^080h,(1192)&7
	line	169
;main.c: 169: WPUA1=1;
	bsf	(1193/8)^080h,(1193)&7
	line	170
;main.c: 170: WPUA6=1;
	bsf	(1198/8)^080h,(1198)&7
	line	172
;main.c: 172: WPUC1=1;
	bsf	(1089/8)^080h,(1089)&7
	line	173
;main.c: 173: WPUC2=1;
	bsf	(1090/8)^080h,(1090)&7
	line	174
;main.c: 174: WPUC3=1;
	bsf	(1091/8)^080h,(1091)&7
	line	176
;main.c: 176: WPDA4=0;
	bcf	(1100/8)^080h,(1100)&7
	line	177
;main.c: 177: WPDC1=0;
	bcf	(1099/8)^080h,(1099)&7
	line	178
;main.c: 178: WPDC2=0;
	bcf	(1098/8)^080h,(1098)&7
	line	179
;main.c: 179: WPDC3=0;
	bcf	(1097/8)^080h,(1097)&7
	line	180
	
l3294:	
	return
	opt stack 0
GLOBAL	__end_of_GPIO_Init
	__end_of_GPIO_Init:
;; =============== function _GPIO_Init ends ============

	signat	_GPIO_Init,88
	global	_System_init
psect	text381,local,class=CODE,delta=2
global __ptext381
__ptext381:

;; *************** function _System_init *****************
;; Defined at:
;;		line 130 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          0       0       0
;;      Totals:         0       0       0
;;Total ram usage:        0 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    1
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text381
	file	"main.c"
	line	130
	global	__size_of_System_init
	__size_of_System_init	equ	__end_of_System_init-_System_init
	
_System_init:	
	opt	stack 6
; Regs used in _System_init: [wreg+status,2]
	line	131
	
l7568:	
;main.c: 131: OPTION = 0x00;
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	clrf	(129)^080h	;volatile
	line	132
	
l7570:	
;main.c: 132: OSCCON = 0x70;
	movlw	(070h)
	movwf	(143)^080h	;volatile
	line	133
	
l7572:	
;main.c: 133: WDTCON = 0x00;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(24)	;volatile
	line	134
	
l3291:	
	return
	opt stack 0
GLOBAL	__end_of_System_init
	__end_of_System_init:
;; =============== function _System_init ends ============

	signat	_System_init,88
	global	_Read_EEPROM_u8
psect	text382,local,class=CODE,delta=2
global __ptext382
__ptext382:

;; *************** function _Read_EEPROM_u8 *****************
;; Defined at:
;;		line 64 in file "eeprom.c"
;; Parameters:    Size  Location     Type
;;  EEAddress       1    wreg     unsigned char 
;; Auto vars:     Size  Location     Type
;;  EEAddress       1    5[COMMON] unsigned char 
;;  ReEepromData    1    6[COMMON] unsigned char 
;; Return value:  Size  Location     Type
;;                  1    wreg      unsigned char 
;; Registers used:
;;		wreg
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         2       0       0
;;      Temps:          1       0       0
;;      Totals:         3       0       0
;;Total ram usage:        3 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    1
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_Read_EEPROM_u16
;; This function uses a non-reentrant model
;;
psect	text382
	file	"eeprom.c"
	line	64
	global	__size_of_Read_EEPROM_u8
	__size_of_Read_EEPROM_u8	equ	__end_of_Read_EEPROM_u8-_Read_EEPROM_u8
	
_Read_EEPROM_u8:	
	opt	stack 5
; Regs used in _Read_EEPROM_u8: [wreg]
;Read_EEPROM_u8@EEAddress stored from wreg
	line	67
	movwf	(Read_EEPROM_u8@EEAddress)
	
l7560:	
;eeprom.c: 65: unsigned char ReEepromData;
;eeprom.c: 67: EEADR = EEAddress;
	movf	(Read_EEPROM_u8@EEAddress),w
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	movwf	(155)^080h	;volatile
	line	68
	
l7562:	
;eeprom.c: 68: RD = 1;
	bsf	(1248/8)^080h,(1248)&7
	line	69
;eeprom.c: 69: ReEepromData = EEDAT;
	movf	(154)^080h,w	;volatile
	movwf	(??_Read_EEPROM_u8+0)+0
	movf	(??_Read_EEPROM_u8+0)+0,w
	movwf	(Read_EEPROM_u8@ReEepromData)
	line	70
	
l7564:	
;eeprom.c: 70: RD = 0;
	bcf	(1248/8)^080h,(1248)&7
	line	71
;eeprom.c: 71: return ReEepromData;
	movf	(Read_EEPROM_u8@ReEepromData),w
	goto	l2165
	
l7566:	
	line	72
	
l2165:	
	return
	opt stack 0
GLOBAL	__end_of_Read_EEPROM_u8
	__end_of_Read_EEPROM_u8:
;; =============== function _Read_EEPROM_u8 ends ============

	signat	_Read_EEPROM_u8,4217
	global	_Write_EEPROM_u8
psect	text383,local,class=CODE,delta=2
global __ptext383
__ptext383:

;; *************** function _Write_EEPROM_u8 *****************
;; Defined at:
;;		line 49 in file "eeprom.c"
;; Parameters:    Size  Location     Type
;;  EEAddress       1    wreg     unsigned char 
;;  EEDatas         1    4[COMMON] unsigned char 
;; Auto vars:     Size  Location     Type
;;  EEAddress       1    5[COMMON] unsigned char 
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         1       0       0
;;      Locals:         1       0       0
;;      Temps:          0       0       0
;;      Totals:         2       0       0
;;Total ram usage:        2 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    1
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_EEPROM_Init
;;		_Write_EEPROM_u16
;; This function uses a non-reentrant model
;;
psect	text383
	file	"eeprom.c"
	line	49
	global	__size_of_Write_EEPROM_u8
	__size_of_Write_EEPROM_u8	equ	__end_of_Write_EEPROM_u8-_Write_EEPROM_u8
	
_Write_EEPROM_u8:	
	opt	stack 5
; Regs used in _Write_EEPROM_u8: [wreg]
;Write_EEPROM_u8@EEAddress stored from wreg
	movwf	(Write_EEPROM_u8@EEAddress)
	line	50
	
l7552:	
;eeprom.c: 50: GIE = 0;
	bcf	(95/8),(95)&7
	line	51
;eeprom.c: 51: while(GIE) asm("clrwdt");
	goto	l2156
	
l2157:	
# 51 "eeprom.c"
clrwdt ;#
psect	text383
	
l2156:	
	btfsc	(95/8),(95)&7
	goto	u2401
	goto	u2400
u2401:
	goto	l2157
u2400:
	goto	l7554
	
l2158:	
	line	52
	
l7554:	
;eeprom.c: 52: EEADR = EEAddress;
	movf	(Write_EEPROM_u8@EEAddress),w
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	movwf	(155)^080h	;volatile
	line	53
;eeprom.c: 53: EEDAT = EEDatas;
	movf	(Write_EEPROM_u8@EEDatas),w
	movwf	(154)^080h	;volatile
	line	54
	
l7556:	
;eeprom.c: 54: EEIF = 0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bcf	(103/8),(103)&7
	line	55
;eeprom.c: 55: EECON1 = 0x34;
	movlw	(034h)
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	movwf	(156)^080h	;volatile
	line	56
	
l7558:	
;eeprom.c: 56: WR = 1;
	bsf	(1256/8)^080h,(1256)&7
	line	57
;eeprom.c: 57: while(WR) asm("clrwdt");
	goto	l2159
	
l2160:	
# 57 "eeprom.c"
clrwdt ;#
psect	text383
	
l2159:	
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	btfsc	(1256/8)^080h,(1256)&7
	goto	u2411
	goto	u2410
u2411:
	goto	l2160
u2410:
	
l2161:	
	line	59
;eeprom.c: 59: GIE = 1;
	bsf	(95/8),(95)&7
	line	60
	
l2162:	
	return
	opt stack 0
GLOBAL	__end_of_Write_EEPROM_u8
	__end_of_Write_EEPROM_u8:
;; =============== function _Write_EEPROM_u8 ends ============

	signat	_Write_EEPROM_u8,8312
	global	_GetADCValue
psect	text384,local,class=CODE,delta=2
global __ptext384
__ptext384:

;; *************** function _GetADCValue *****************
;; Defined at:
;;		line 44 in file "adc.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;  ADC_num         2    8[COMMON] unsigned int 
;; Return value:  Size  Location     Type
;;                  2    4[COMMON] unsigned int 
;; Registers used:
;;		wreg, status,2, status,0
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         2       0       0
;;      Locals:         2       0       0
;;      Temps:          2       0       0
;;      Totals:         6       0       0
;;Total ram usage:        6 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    1
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text384
	file	"adc.c"
	line	44
	global	__size_of_GetADCValue
	__size_of_GetADCValue	equ	__end_of_GetADCValue-_GetADCValue
	
_GetADCValue:	
	opt	stack 6
; Regs used in _GetADCValue: [wreg+status,2+status,0]
	line	45
	
l7542:	
;adc.c: 45: unsigned int ADC_num=0;
	clrf	(GetADCValue@ADC_num)
	clrf	(GetADCValue@ADC_num+1)
	line	47
;adc.c: 47: while(GO_DONE) asm("clrwdt");
	goto	l1072
	
l1073:	
# 47 "adc.c"
clrwdt ;#
psect	text384
	
l1072:	
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	btfsc	(249/8),(249)&7
	goto	u2391
	goto	u2390
u2391:
	goto	l1073
u2390:
	goto	l7544
	
l1074:	
	line	48
	
l7544:	
;adc.c: 48: ADC_num=ADRESH;
	movf	(30),w	;volatile
	movwf	(??_GetADCValue+0)+0
	clrf	(??_GetADCValue+0)+0+1
	movf	0+(??_GetADCValue+0)+0,w
	movwf	(GetADCValue@ADC_num)
	movf	1+(??_GetADCValue+0)+0,w
	movwf	(GetADCValue@ADC_num+1)
	line	49
;adc.c: 49: ADC_num=ADC_num<<8;
	movf	(GetADCValue@ADC_num),w
	movwf	(??_GetADCValue+0)+0+1
	clrf	(??_GetADCValue+0)+0
	movf	0+(??_GetADCValue+0)+0,w
	movwf	(GetADCValue@ADC_num)
	movf	1+(??_GetADCValue+0)+0,w
	movwf	(GetADCValue@ADC_num+1)
	line	50
;adc.c: 50: ADC_num=ADC_num|ADRESL;
	movf	(GetADCValue@ADC_num),w
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	iorwf	(158)^080h,w	;volatile
	movwf	(GetADCValue@ADC_num)
	movf	(GetADCValue@ADC_num+1),w
	movwf	1+(GetADCValue@ADC_num)
	line	51
	
l7546:	
;adc.c: 51: GO_DONE=1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(249/8),(249)&7
	line	52
	
l7548:	
;adc.c: 52: return ADC_num;
	movf	(GetADCValue@ADC_num+1),w
	clrf	(?_GetADCValue+1)
	addwf	(?_GetADCValue+1)
	movf	(GetADCValue@ADC_num),w
	clrf	(?_GetADCValue)
	addwf	(?_GetADCValue)

	goto	l1075
	
l7550:	
	line	53
	
l1075:	
	return
	opt stack 0
GLOBAL	__end_of_GetADCValue
	__end_of_GetADCValue:
;; =============== function _GetADCValue ends ============

	signat	_GetADCValue,90
	global	_ISR
psect	text385,local,class=CODE,delta=2
global __ptext385
__ptext385:

;; *************** function _ISR *****************
;; Defined at:
;;		line 120 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		None
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          4       0       0
;;      Totals:         4       0       0
;;Total ram usage:        4 bytes
;; Hardware stack levels used:    1
;; This function calls:
;;		Nothing
;; This function is called by:
;;		Interrupt level 1
;; This function uses a non-reentrant model
;;
psect	text385
	file	"main.c"
	line	120
	global	__size_of_ISR
	__size_of_ISR	equ	__end_of_ISR-_ISR
	
_ISR:	
	opt	stack 5
; Regs used in _ISR: []
psect	intentry,class=CODE,delta=2
global __pintentry
__pintentry:
global interrupt_function
interrupt_function:
	global saved_w
	saved_w	set	btemp+0
	movwf	saved_w
	swapf	status,w
	movwf	(??_ISR+0)
	movf	fsr0,w
	movwf	(??_ISR+1)
	movf	pclath,w
	movwf	(??_ISR+2)
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	btemp+1,w
	movwf	(??_ISR+3)
	ljmp	_ISR
psect	text385
	line	121
	
i1l3288:	
	movf	(??_ISR+3),w
	movwf	btemp+1
	movf	(??_ISR+2),w
	movwf	pclath
	movf	(??_ISR+1),w
	movwf	fsr0
	swapf	(??_ISR+0)^0FFFFFF80h,w
	movwf	status
	swapf	saved_w,f
	swapf	saved_w,w
	retfie
	opt stack 0
GLOBAL	__end_of_ISR
	__end_of_ISR:
;; =============== function _ISR ends ============

	signat	_ISR,88
psect	text386,local,class=CODE,delta=2
global __ptext386
__ptext386:
	global	btemp
	btemp set 07Eh

	DABS	1,126,2	;btemp
	global	wtemp0
	wtemp0 set btemp
	end
