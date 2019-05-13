/****************************************************************************
*       Copyright (C), 北京四维拓智教育科技有限公司
*
*       MS83Fxx02B
*       V0.1
*       2017-12-04
*       Woody  QQ:2490006131
*Description  :      <-MS83F0802BS->
* 				        _________
* 				   VDD|1         8|VSS
*    PA6/OSC2/CLKO/T1G|2   MS83  7|PA0/AN0/C1IN+/ICSPCLK
*         PC3/P1C/PWM4|3   F080  6|PA1/AN1/C1IN-/ICSPDAT
*     PC2/AN6/P1D/PWM5|4_________5|PC1/AN5/C2IN-/P1E/INT
*
*PC5->VS1838
*后一级输出PC3
*前一级输入PC1(AIN5)
*按键PA3
*拨码PA2
*****************************************************************************/
#include "includes.h"
#include "eeprom.h"
#include "adc.h"
#include "key.h"
#include "tim2.h"
/*  Variate ////////////////////////////////////////////////////////////////*/
uint16_t adc_input;
uint16_t adc_value;
uint16_t adc_Study;
uint8_t flag_sc=0;
uint16_t volume1;
uint16_t times=0;
/*******************************************************************************
 * 函数名：main
 * 描述  ：系统主函数
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
void main ( void )
{
    uint8_t i=0;

    System_init();
    GPIO_Init();
    OUTPUT=0;
    ADC_Init();
    EEPROM_Init();
    //Timer2_Init();
    adc_Study = Read_EEPROM_u16 ( 0x00 );
    if ( adc_Study == 0XFFFF ) {
        adc_Study = 600;
    }
    LED_Study_End();
    ExchChannel ( 5 );
    while ( 1 ) {
        flag_sc=0;
        //ExchChannel(5);
        //for(i=0;i<5;i++)
        adc_value = GetADCValue();
        if ( isKeyPressed() ) {
            volume1=0;
            Delay_xms ( 20 );
            times=0;
            //ExchChannel(5);
            while ( !isKeyPressed() ) {
                LED_STUDY=1;
                OUTPUT=0;
                if ( flag_sc == 0 ) {
                    flag_sc=1;
                    Delay_xms ( 300 );
                }
                times++;
                if ( times>=2000 ) {
                    adc_Study = volume1;
                    Write_EEPROM_u16 ( 0x00,adc_Study );
                    LED_STUDY=0;
                    return;
                }
                adc_value = GetADCValue();
                if ( volume1 < adc_value ) { //最大值765  0X2FD
                    if ( adc_value >= 700 ) {
                        adc_value = 700;
                    }
                    if ( adc_value<=540 ) { //原510
                        adc_value=540;
                    }
                    volume1 = adc_value;
                }
                Delay_xms ( 1 );
            }
        }
        if ( adc_value >= adc_Study ) {
            //if(flag_sc==0){
            //	flag_sc=1;
            ExchChannel ( 0 );
            for ( i=0; i<5; i++ ) {
                adc_input = GetADCValue();
            }
            if ( adc_input>=500 ) {
                ExchChannel ( 5 );
                OUTPUT=1;
                Delay_xms ( 100 );
                OUTPUT=0;
                Delay_xms ( 400 );
            } else  {
                OUTPUT=0;
                Delay_xms ( 10 );
                //ExchChannel(5);
            }
            ExchChannel ( 5 );
        } else {
            OUTPUT=0;
        }
    }
}

//===============Interrupt_Service_Routine===============
void interrupt ISR ( void )
{
}
/*******************************************************************************
 * 函数名：System_init
 * 描述  ：系统初始化函数
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
void System_init ( void )
{
    OPTION  = 0x00;//BIT7使能上拉位，0=使能，1=禁止
    OSCCON  = 0x70;  //内部振荡器选择 16Mhz
    WDTCON  = 0x00;  //BIT4~BIT1:看门狗复位时间512ms，bit0--1=开启，0=关闭
}
/*******************************************************************************
 * 函数名：GPIO_Init
 * 描述  ：GPIO初始化函数
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
void GPIO_Init ( void )
{
    //TRISA 0=OUTPUT,1=INPUT
    TRISA0=0;
    TRISA1=0;
    TRISA6=0;
    //TRISC 0=OUTPUT,1=INPUT
    TRISC1=0;
    TRISC2=0;
    TRISC3=1;

    CM0=1;
    CM1=1;
    CM2=1;//比较器关闭，CxIN管脚为数字IO管脚

    //当弱上拉和弱下拉同时在这些 IO 打开时，弱下拉将被禁止，弱上拉起作用
    //ANSEL portA 1=模拟 0=数字
    ANSEL0=0;
    ANSEL1=0;
    ANSEL2=0;
    ANSEL3=0;
    ANSEL4=0;
    ANSEL5=0;
    ANSEL6=0;
    ANSEL7=0;
    //WPUA 1=使能上拉，0=禁止
    WPUA0=1;
    WPUA1=1;
    WPUA6=1;
    //WPUC 1=使能上拉，0=禁止
    WPUC1=1;
    WPUC2=1;
    WPUC3=1;
    //WPD 1=使能下拉，0=断开下拉
    WPDA4=0;
    WPDC1=0;
    WPDC2=0;
    WPDC3=0;
}
/*******************************************************************************
 * 函数名：Delay_xms 毫秒级延迟函数
 * 描述  ：最大0xFFFF 65535
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
void Delay_xms ( uint16_t x ) //死延时x毫秒
{
    uint16_t i;
    for ( i=0; i<x; i++ ) {
        __delay_ms ( 1 );
        //CLRWDT();
    }
}
/*******************************************************************************
 * 函数名：LED_Study_End
 * 描述  ：LED效果
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
void LED_Study_End ( void )
{
    LED_STUDY = 1;
    Delay_xms ( 40 );
    LED_STUDY = 0;
    Delay_xms ( 40 );
    LED_STUDY = 1;
    Delay_xms ( 40 );
    LED_STUDY = 0;
    Delay_xms ( 40 );
    LED_STUDY = 1;
    Delay_xms ( 40 );
    LED_STUDY = 0;
    Delay_xms ( 40 );
}

