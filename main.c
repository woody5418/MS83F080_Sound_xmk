/****************************************************************************
*       Copyright (C), ������ά���ǽ����Ƽ����޹�˾
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
*��һ�����PC3
*ǰһ������PC1(AIN5)
*����PA3
*����PA2
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
 * ��������main
 * ����  ��ϵͳ������
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
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
                if ( volume1 < adc_value ) { //���ֵ765  0X2FD
                    if ( adc_value >= 700 ) {
                        adc_value = 700;
                    }
                    if ( adc_value<=540 ) { //ԭ510
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
 * ��������System_init
 * ����  ��ϵͳ��ʼ������
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
void System_init ( void )
{
    OPTION  = 0x00;//BIT7ʹ������λ��0=ʹ�ܣ�1=��ֹ
    OSCCON  = 0x70;  //�ڲ�����ѡ�� 16Mhz
    WDTCON  = 0x00;  //BIT4~BIT1:���Ź���λʱ��512ms��bit0--1=������0=�ر�
}
/*******************************************************************************
 * ��������GPIO_Init
 * ����  ��GPIO��ʼ������
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
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
    CM2=1;//�Ƚ����رգ�CxIN�ܽ�Ϊ����IO�ܽ�

    //����������������ͬʱ����Щ IO ��ʱ��������������ֹ��������������
    //ANSEL portA 1=ģ�� 0=����
    ANSEL0=0;
    ANSEL1=0;
    ANSEL2=0;
    ANSEL3=0;
    ANSEL4=0;
    ANSEL5=0;
    ANSEL6=0;
    ANSEL7=0;
    //WPUA 1=ʹ��������0=��ֹ
    WPUA0=1;
    WPUA1=1;
    WPUA6=1;
    //WPUC 1=ʹ��������0=��ֹ
    WPUC1=1;
    WPUC2=1;
    WPUC3=1;
    //WPD 1=ʹ��������0=�Ͽ�����
    WPDA4=0;
    WPDC1=0;
    WPDC2=0;
    WPDC3=0;
}
/*******************************************************************************
 * ��������Delay_xms ���뼶�ӳٺ���
 * ����  �����0xFFFF 65535
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
void Delay_xms ( uint16_t x ) //����ʱx����
{
    uint16_t i;
    for ( i=0; i<x; i++ ) {
        __delay_ms ( 1 );
        //CLRWDT();
    }
}
/*******************************************************************************
 * ��������LED_Study_End
 * ����  ��LEDЧ��
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
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

