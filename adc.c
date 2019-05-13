//********************************************************************
//
//      ADC Init Initialization Part
//      4DClass
//      Copyright(C)  2017
//      All rights reserved
//***********************************************************************
#include "includes.h"
/*******************************************************************************
 * ��������ADC_Init
 * ����  ��ADC��ʼ������
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
void ADC_Init(void)
{
    //���ö˿�Ϊ����ģ��
    TRISA0=1;
    TRISC1=1;
    ANSEL0=1;
    ANSEL5=1;	//����ģ������
    //����ADCģ��
    //ADCON1
    DIVS=0;//ѡ��ʱ��
    ADCS0=0;
    ADCS1=0;
    ADCS2=0;//ʱ��ѡ��
    //ADCON0
    ADCON0=0B10000001;//�Ҷ��� VDD AN0  ʹ��ADC
    VCFG0=0;//VCFG1=0;//���òο���ѹ VCFG1û�ж���
    CHS0=1;
    CHS1=0;
    CHS2=1;//ģ��ͨ��ѡ��5
    ADFM=1;	//1=�Ҷ��룬0=�����
    ADON=1;//ADCʹ��λ 1=enable 0=disable

    Delay_xms(1);
    GO_DONE=1;//Ӳ����0��Ӳ����1 1=����ת�� 0=ת�����
}

//��ȡ10λ��ADCֵ
uint16_t GetADCValue(void)
{
    uint16_t ADC_num=0;

    while(GO_DONE) CLRWDT();//ADC�Ƿ�ת�����
    ADC_num=ADRESH;
    ADC_num=ADC_num<<8;
    ADC_num=ADC_num|ADRESL;
    GO_DONE=1;//�����1�����¶�ADC
    return ADC_num;
}



/*******************************************************************************
 * ��������ExchChannel
 * ����  ��ADC��ʼ������
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 * ˵��  : ��ͨ��ʱ����ʱ10us-1ms
 ******************************************************************************/
void ExchChannel(unsigned char ch_temp)//ģ��ͨ��ѡ��
{
    unsigned char adc_ch_temp;

    adc_ch_temp = ch_temp;
    adc_ch_temp = adc_ch_temp<<2;
    ADCON0 = (ADCON0&0xe3)|adc_ch_temp;
    __delay_ms(1);
    GO_DONE=1;//Ӳ����0��Ӳ����1 1=����ת�� 0=ת�����
}















